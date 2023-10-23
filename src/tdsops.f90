module m_tdsops
   use iso_fortran_env, only: stderr => error_unit

   use m_common, only: dp, pi

   implicit none

   type :: tdsops_t
      !! Tridiagonal Solver Operators class.
      !!
      !! Operator arrays are preprocessed in this class based on the arguments
      !! provided. dist_fw and dist_bw are used in the first phase of the
      !! distributed tridiagonal solver algorithm. dist_sa and dist_sc are used
      !! in the final substitution phase. See the kernels_dist.f90 files in the
      !! relevant backend folders.
      !! coeff arrays define the specific rules of building the RHS
      !! corresponding to the tridiagonal system to be solved, and used only in
      !! the first phase of the distributed algorithm when building the RHS.
      !! If a boundary condition is defined then coeffs_s and coeffs_e differ
      !! from coeffs array and define the RHS rule for the first and last 4
      !! entries in the tridiagonal system (n_halo = 4).
      !!
      !! This class does not know about the current rank or its relative
      !! location among other ranks. All the operator arrays here are used when
      !! executing a distributed tridiagonal solver phase one or two.
      real(dp), allocatable, dimension(:) :: dist_fw, dist_bw, & !! fw/bw phase
                                             dist_sa, dist_sc, & !! back subs.
                                             dist_af !! the auxiliary factors
      real(dp), allocatable :: coeffs(:), coeffs_s(:, :), coeffs_e(:, :)
      integer :: n, n_halo
   contains
      procedure :: deriv_1st, deriv_2nd, interpl_mid, stagder_1st, preprocess
   end type tdsops_t

   interface tdsops_t
      module procedure tdsops_init
   end interface tdsops_t

contains

   function tdsops_init(n, delta, operation, scheme, n_halo, from_to, &
                        bc_start, bc_end, sym, c_nu, nu0_nu) result(tdsops)
      !! Constructor function for the tdsops_t class.
      !!
      !! 'n', 'delta', 'operation', and 'scheme' are necessary arguments.
      !! Number of points 'n', distance between two points 'delta', the
      !! 'operation' the tridiagonal system defines, and the 'scheme' that
      !! specifies the exact scheme we choose to apply for the operation.
      !! The remaining arguments are optional.
      !! 'from_to' is necessary for interpolation and staggared derivative, and
      !! it can be 'v2p' or 'p2v'.
      !! If the specific region the instance is operating is not a boundary
      !! region, then 'bc_start' and 'bc_end' are either 'null' or not defined.
      !! 'sym' is relevant when the boundary condition is free-slip. If sym is
      !! .true. then it means the field we operate on is assumed to be an even
      !! function (symmetric) accross the boundary. If it is .false. it means
      !! that the field is assumed to be an odd function (anti-symmetric).
      !! 'c_nu', 'nu0_nu' are relevant when operation is second order
      !! derivative and scheme is compact6-hyperviscous.
      implicit none

      type(tdsops_t) :: tdsops !! return value of the function

      integer, intent(in) :: n
      real(dp), intent(in) :: delta
      character(*), intent(in) :: operation, scheme
      integer, optional, intent(in) :: n_halo
      character(*), optional, intent(in) :: from_to, bc_start, bc_end
      logical, optional, intent(in) :: sym
      real(dp), optional, intent(in) :: c_nu, nu0_nu

      integer :: n_stencil

      tdsops%n = n
      if (present(n_halo)) then
         tdsops%n_halo = n_halo
         if (n_halo /= 4) then
            write (stderr, '("Warning: n_halo is set to ", i2, "be careful! &
                              The default is 4 and there are quite a few &
                              places where things are hardcoded assuming &
                              n_halo is 4.")') n_halo
         end if
      else
         tdsops%n_halo = 4
      end if

      n_stencil = 2*tdsops%n_halo + 1

      allocate(tdsops%dist_fw(n), tdsops%dist_bw(n))
      allocate(tdsops%dist_sa(n), tdsops%dist_sc(n))
      allocate(tdsops%dist_af(n))
      allocate(tdsops%coeffs(n_stencil))
      allocate(tdsops%coeffs_s(n_stencil, tdsops%n_halo))
      allocate(tdsops%coeffs_e(n_stencil, tdsops%n_halo))

      if (operation == 'first-deriv') then
         call tdsops%deriv_1st(delta, scheme, bc_start, bc_end, sym)
      else if (operation == 'second-deriv') then
         call tdsops%deriv_2nd(delta, scheme, bc_start, bc_end, sym, &
                               c_nu, nu0_nu)
      else if (operation == 'interpolate') then
         call tdsops%interpl_mid(scheme, from_to, bc_start, bc_end, sym)
      else if (operation == 'staggared-deriv') then
         call tdsops%stagder_1st(delta, scheme, from_to, bc_start, bc_end, sym)
      else
         error stop 'operation is not defined'
      end if

   end function tdsops_init

   subroutine deriv_1st(self, delta, scheme, bc_start, bc_end, sym)
      implicit none

      class(tdsops_t), intent(inout) :: self
      real(dp), intent(in) :: delta
      character(*), intent(in) :: scheme
      character(*), optional, intent(in) :: bc_start, bc_end
      logical, optional, intent(in) :: sym

      real(dp), allocatable :: dist_b(:)
      real(dp) :: alpha, afi, bfi
      integer :: i, n, n_halo
      logical :: symmetry

      if (self%n_halo < 2) error stop 'First derivative require n_halo >= 2'

      if (present(sym)) then
         symmetry = sym
      else
         symmetry = .false.
      end if

      ! alpha is alfa

      select case (scheme)
      case ('compact6')
         alpha = 1._dp/3._dp
         afi = 7._dp/9._dp/delta
         bfi = 1._dp/36._dp/delta
      case default
         error stop 'scheme is not defined'
      end select

      self%coeffs(:) = [0._dp, 0._dp, -bfi, -afi, &
                        0._dp, &
                        afi, bfi, 0._dp, 0._dp]

      do i = 1, self%n_halo
         self%coeffs_s(:, i) = self%coeffs(:)
         self%coeffs_e(:, i) = self%coeffs(:)
      end do

      self%dist_sa(:) = alpha; self%dist_sc(:) = alpha

      n = self%n
      n_halo = self%n_halo

      allocate(dist_b(n))
      dist_b(:) = 1._dp

      select case (bc_start)
      case ('neumann')
         if (symmetry) then
            ! sym == .true.; d(uu)/dx, dv/dx, dw/dx
            !                d(vv)/dy, du/dy, dw/dy
            !                d(ww)/dz, du/dz, dv/dz
            self%dist_sa(1) = 0._dp
            self%dist_sc(1) = 0._dp
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   0._dp, &
                                   0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, -afi, &
                                   -bfi, &
                                   afi, bfi, 0._dp, 0._dp]
         else
            ! sym == .false.; d(uv)/dx, d(uw)/dx, du/dx
            !                 d(vu)/dy, d(vw)/dy, dv/dy
            !                 d(wu)/dz, d(wv)/dz, dw/dz
            self%dist_sa(1) = 0._dp
            self%dist_sc(1) = 2*alpha
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   0._dp, &
                                   2*afi, 2*bfi, 0._dp, 0._dp]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, -afi, &
                                   bfi, &
                                   afi, bfi, 0._dp, 0._dp]
         end if
      case ('dirichlet')
         ! first line
         self%dist_sa(1) = 0._dp
         self%dist_sc(1) = 2._dp
         self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                -2.5_dp, &
                                2._dp, 0.5_dp, 0._dp, 0._dp]
         self%coeffs_s(:, 1) = self%coeffs_s(:, 1)/delta
         ! second line
         self%dist_sa(2) = 0.25_dp
         self%dist_sc(2) = 0.25_dp
         self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, -0.75_dp, &
                                0._dp, &
                                0.75_dp, 0._dp, 0._dp, 0._dp]
         self%coeffs_s(:, 2) = self%coeffs_s(:, 2)/delta
      end select

      select case (bc_end)
      case ('neumann')
         if (symmetry) then
            ! sym == .true.; d(uu)/dx, dv/dx, dw/dx
            !                d(vv)/dy, du/dy, dw/dy
            !                d(ww)/dz, du/dz, dv/dz
            self%dist_sa(n) = 0._dp
            self%dist_sc(n) = 0._dp
            self%coeffs_e(:, n_halo) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                        0._dp, &
                                        0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, n_halo - 1) = [0._dp, 0._dp, -bfi, -afi, &
                                            bfi, &
                                            afi, 0._dp, 0._dp, 0._dp]
         else
            ! sym == .false.; d(uv)/dx, d(uw)/dx, du/dx
            !                 d(vu)/dy, d(vw)/dy, dv/dy
            !                 d(wu)/dz, d(wv)/dz, dw/dz
            self%dist_sa(n) = 2*alpha
            self%dist_sc(n) = 0._dp
            self%coeffs_e(:, n_halo) = [0._dp, 0._dp, -2*bfi, -2*afi, &
                                        0._dp, &
                                        0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, n_halo - 1) = [0._dp, 0._dp, -bfi, -afi, &
                                            -bfi, &
                                            afi, 0._dp, 0._dp, 0._dp]
         end if
      case ('dirichlet')
         ! last line
         self%dist_sa(n) = 2._dp
         self%dist_sc(n) = 0._dp
         self%coeffs_e(:, n_halo) = [0._dp, 0._dp, -0.5_dp, -2._dp, &
                                     2.5_dp, &
                                     0._dp, 0._dp, 0._dp, 0._dp]
         self%coeffs_e(:, n_halo) = self%coeffs_e(:, n_halo)/delta
         ! second last line
         self%dist_sa(n - 1) = 0.25_dp
         self%dist_sc(n - 1) = 0.25_dp
         self%coeffs_e(:, n_halo - 1) = [0._dp, 0._dp, 0._dp, -0.75_dp, &
                                         0._dp, &
                                         0.75_dp, 0._dp, 0._dp, 0._dp]
         self%coeffs_e(:, n_halo - 1) = self%coeffs_e(:, n_halo - 1)/delta
      end select

      call self%preprocess(dist_b)

   end subroutine deriv_1st

   subroutine deriv_2nd(self, delta, scheme, bc_start, bc_end, sym, &
                        c_nu, nu0_nu)
      implicit none

      class(tdsops_t), intent(inout) :: self
      real(dp), intent(in) :: delta
      character(*), intent(in) :: scheme
      character(*), optional, intent(in) :: bc_start, bc_end
      logical, optional, intent(in) :: sym
      real(dp), optional, intent(in) :: c_nu, nu0_nu

      real(dp), allocatable :: dist_b(:)
      real(dp) :: alpha, asi, bsi, csi, dsi
      real(dp) :: dpis3, xnpi2, xmpi2, den, d2, temp1, temp2
      integer :: i, n, n_halo
      logical :: symmetry

      if (self%n_halo < 4) error stop 'Second derivative require n_halo >= 4'

      if (present(sym)) then
         symmetry = sym
      else
         symmetry = .false.
      end if

      d2 = delta*delta

      ! alpha is alsa

      select case (scheme)
      case ('compact6')
         alpha = 2._dp/11._dp
         asi = 12._dp/11._dp/d2
         bsi = 3._dp/44._dp/d2
         csi = 0._dp
         dsi = 0._dp
      case ('compact6-hyperviscous')
         if (present(c_nu) .and. present(nu0_nu)) then
            dpis3 = 2._dp*pi/3._dp
            xnpi2 = pi*pi*(1._dp + nu0_nu)
            xmpi2 = dpis3*dpis3*(1._dp + c_nu*nu0_nu)
            den = 405._dp*xnpi2 - 640._dp*xmpi2 + 144._dp
            alpha = 0.5_dp - (320._dp*xmpi2 - 1296._dp)/den
            asi = -(4329._dp*xnpi2/8._dp - 32._dp*xmpi2 &
                    - 140._dp*xnpi2*xmpi2 + 286._dp)/den/d2
            bsi = (2115._dp*xnpi2 - 1792._dp*xmpi2 &
                   - 280._dp*xnpi2*xmpi2 + 1328._dp)/den/(4._dp*d2)
            csi = -(7695._dp*xnpi2/8._dp + 288._dp*xmpi2 &
                    - 180._dp*xnpi2*xmpi2 - 2574._dp)/den/(9._dp*d2)
            dsi = (198._dp*xnpi2 + 128._dp*xmpi2 &
                   - 40._dp*xnpi2*xmpi2 - 736._dp)/den/(16._dp*d2)
         else
            error stop 'compact6-hyperviscous requires c_nu and nu0_nu'
         end if
      case default
         error stop 'scheme is not defined'
      end select

      self%coeffs(:) = [dsi, csi, bsi, asi, &
                        -2._dp*(asi + bsi + csi + dsi), &
                        asi, bsi, csi, dsi]

      do i = 1, self%n_halo
         self%coeffs_s(:, i) = self%coeffs(:)
         self%coeffs_e(:, i) = self%coeffs(:)
      end do

      self%dist_sa(:) = alpha; self%dist_sc(:) = alpha

      n = self%n
      n_halo = self%n_halo

      allocate(dist_b(n))
      dist_b(:) = 1._dp

      select case (bc_start)
      case ('neumann')
         if (symmetry) then
            ! sym == .true.; d2v/dx2, d2w/dx2
            !                d2u/dy2, d2w/dy2
            !                d2u/dz2, d2v/dz2
            self%dist_sa(1) = 0._dp
            self%dist_sc(1) = 2*alpha
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   -2*asi - 2*bsi - 2*csi - 2*dsi, &
                                   2*asi, 2*bsi, 2*csi, 2*dsi]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, asi, &
                                   -2*asi - bsi - 2*csi - 2*dsi, &
                                   asi + csi, bsi + dsi, csi, dsi]
            self%coeffs_s(:, 3) = [0._dp, 0._dp, bsi, asi + csi, &
                                   -2*asi - 2*bsi - 2*csi - dsi, &
                                   asi, bsi, csi, dsi]
            self%coeffs_s(:, 4) = [0._dp, csi, bsi + dsi, asi, &
                                   -2*asi - 2*bsi - 2*csi - 2*dsi, &
                                   asi, bsi, csi, dsi]
         else
            ! sym == .false.; d2u/dx2
            !                 d2v/dy2
            !                 d2w/dz2
            self%dist_sa(1) = 0._dp
            self%dist_sc(1) = 0._dp
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   0._dp, &
                                   0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, asi, &
                                   -2*asi - 3*bsi - 2*csi - 2*dsi, &
                                   asi - csi, bsi - dsi, csi, dsi]
            self%coeffs_s(:, 3) = [0._dp, 0._dp, bsi, asi - csi, &
                                   -2*asi - 2*bsi - 2*csi - 3*dsi, &
                                   asi, bsi, csi, dsi]
            self%coeffs_s(:, 4) = [0._dp, -csi, bsi - dsi, asi, &
                                   -2*asi - 2*bsi - 2*csi - 2*dsi, &
                                   asi, bsi, csi, dsi]
         end if
      case ('dirichlet')
         ! first line
         self%dist_sa(1) = 0._dp
         self%dist_sc(1) = 11._dp
         self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                13._dp/d2, &
                                -27._dp/d2, 15._dp/d2, -1._dp/d2, 0._dp]
         ! second line
         self%dist_sa(2) = 0.1_dp
         self%dist_sc(2) = 0.1_dp
         self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, 1.2_dp/d2, &
                                -2.4_dp/d2, &
                                1.2_dp/d2, 0._dp, 0._dp, 0._dp]
         ! third line
         self%dist_sa(3) = 2._dp/11._dp
         self%dist_sc(3) = 2._dp/11._dp
         temp1 = 3._dp/44._dp/d2; temp2 = 12._dp/11._dp/d2
         self%coeffs_s(:, 3) = [0._dp, 0._dp, temp1, temp2, &
                                -2._dp*(temp1 + temp2), &
                                temp2, temp1, 0._dp, 0._dp]
         ! fourth line is same as third
         self%dist_sa(4) = 2._dp/11._dp
         self%dist_sc(4) = 2._dp/11._dp
         self%coeffs_s(:, 4) = self%coeffs_s(:, 3)
      end select

      select case (bc_end)
      case ('neumann')
         if (symmetry) then
            ! sym == .true.; d2v/dx2, d2w/dx2
            !                d2u/dy2, d2w/dy2
            !                d2u/dz2, d2v/dz2
            self%dist_sa(n) = 2*alpha
            self%dist_sc(n) = 0._dp
            self%coeffs_e(:, 4) = [dsi, csi, bsi, asi, &
                                   -2*asi - 2*bsi - 2*csi - 2*dsi, &
                                   0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 3) = [dsi, csi, bsi + dsi, asi + csi, &
                                   -2*asi - bsi - 2*csi - 2*dsi, &
                                   asi, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 2) = [dsi, csi, bsi, asi, &
                                   -2*asi - 2*bsi - 2*csi - dsi, &
                                   asi + csi, bsi, 0._dp, 0._dp]
            self%coeffs_e(:, 1) = [dsi, csi, bsi, asi, &
                                   -2*asi - 2*bsi - 2*csi - 2*dsi, &
                                   asi, bsi + dsi, csi, 0._dp]
         else
            ! sym == .false.; d2u/dx2
            !                 d2v/dy2
            !                 d2w/dz2
            self%dist_sa(n) = 0._dp
            self%dist_sc(n) = 0._dp
            self%coeffs_e(:, 4) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   0._dp, &
                                   0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 3) = [dsi, csi, bsi - dsi, asi - csi, &
                                   -2*asi - 3*bsi - 2*csi - 2*dsi, &
                                   asi, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 2) = [dsi, csi, bsi, asi, &
                                   -2*asi - 2*bsi - 2*csi - 3*dsi, &
                                   asi - csi, bsi, 0._dp, 0._dp]
            self%coeffs_e(:, 1) = [dsi, csi, bsi, asi, &
                                   -2*asi - 2*bsi - 2*csi - 2*dsi, &
                                   asi, bsi - dsi, -csi, 0._dp]
         end if
      case ('dirichlet')
         ! last line
         self%dist_sa(n) = 11._dp
         self%dist_sc(n) = 0._dp
         self%coeffs_e(:, 4) = [0._dp, -1._dp/d2, 15._dp/d2, -27._dp/d2, &
                                13._dp/d2, &
                                0._dp, 0._dp, 0._dp, 0._dp]
         ! second last line
         self%dist_sa(n - 1) = 0.1_dp
         self%dist_sc(n - 1) = 0.1_dp
         self%coeffs_e(:, 3) = [0._dp, 0._dp, 0._dp, 1.2_dp/d2, &
                                -2.4_dp/d2, &
                                1.2_dp/d2, 0._dp, 0._dp, 0._dp]
         ! third last line
         self%dist_sa(n - 2) = 2._dp/11._dp
         self%dist_sc(n - 2) = 2._dp/11._dp
         temp1 = 3._dp/44._dp/d2; temp2 = 12._dp/11._dp/d2
         self%coeffs_e(:, 2) = [0._dp, 0._dp, temp1, temp2, &
                                -2._dp*(temp1 + temp2), &
                                temp2, temp1, 0._dp, 0._dp]
         ! fourth last line is same as third last
         self%dist_sa(n - 3) = 2._dp/11._dp
         self%dist_sc(n - 3) = 2._dp/11._dp
         self%coeffs_e(:, 1) = self%coeffs_e(:, 2)
      end select

      call self%preprocess(dist_b)

   end subroutine deriv_2nd

   subroutine interpl_mid(self, scheme, from_to, bc_start, bc_end, sym)
      implicit none

      class(tdsops_t), intent(inout) :: self
      character(*), intent(in) :: scheme, from_to
      character(*), optional, intent(in) :: bc_start, bc_end
      logical, optional, intent(in) :: sym

      real(dp), allocatable :: dist_b(:)
      real(dp) :: alpha, aici, bici, cici, dici
      integer :: i, n, n_halo

      if (self%n_halo < 4) error stop 'Interpolation require n_halo >= 4'

      ! alpha is ailcai

      select case (scheme)
      case ('classic')
         alpha = 0.3_dp
         aici = 0.75_dp
         bici = 0.05_dp
         cici = 0._dp
         dici = 0._dp
      case ('optimised')
         alpha = 0.461658_dp
         dici = 0.00146508_dp
         aici = (75._dp + 70._dp*alpha - 640._dp*dici)/128._dp
         bici = (-25._dp + 126._dp*alpha + 2304._dp*dici)/256._dp
         cici = (3._dp - 10._dp*alpha - 1280._dp*dici)/256._dp
      case ('aggressive')
         alpha = 0.49_dp
         aici = (75._dp + 70._dp*alpha)/128._dp
         bici = (-25._dp + 126._dp*alpha)/256._dp
         cici = (3._dp - 10._dp*alpha)/256._dp
         dici = 0._dp
      case default
         error stop 'scheme is not defined'
      end select

      select case (from_to)
      case ('v2p')
         self%coeffs(:) = [0._dp, dici, cici, bici, &
                           aici, &
                           aici, bici, cici, dici]
      case ('p2v')
         self%coeffs(:) = [dici, cici, bici, aici, &
                           aici, &
                           bici, cici, dici, 0._dp]
      end select

      do i = 1, self%n_halo
         self%coeffs_s(:, i) = self%coeffs(:)
         self%coeffs_e(:, i) = self%coeffs(:)
      end do

      self%dist_sa(:) = alpha; self%dist_sc(:) = alpha

      n = self%n
      n_halo = self%n_halo

      allocate(dist_b(n))
      dist_b(:) = 1._dp

      if ((bc_start == 'dirichlet') .or. (bc_start == 'neumann')) then
         self%dist_sa(1) = 0._dp

         select case (from_to)
         case ('v2p')
            ! sym is always .true.
            dist_b(1) = 1._dp + alpha
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   aici, &
                                   aici + bici, bici + cici, cici + dici, dici]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, bici, &
                                   aici + cici, &
                                   aici + dici, bici, cici, dici]
            self%coeffs_s(:, 3) = [0._dp, 0._dp, cici, bici + dici, &
                                   aici, &
                                   aici, bici, cici, dici]
         case ('p2v')
            ! sym is always .true.
            self%dist_sc(1) = 2*alpha
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   2*aici, &
                                   2*bici, 2*cici, 2*dici, 0._dp]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, aici + bici, &
                                   aici + cici, &
                                   bici + dici, cici, dici, 0._dp]
            self%coeffs_s(:, 3) = [0._dp, 0._dp, bici + cici, aici + dici, &
                                   aici, &
                                   bici, cici, dici, 0._dp]
            self%coeffs_s(:, 4) = [0._dp, cici + dici, bici, aici, &
                                   aici, &
                                   bici, cici, dici, 0._dp]
         end select
      end if

      if ((bc_end == 'dirichlet') .or. (bc_end == 'neumann')) then
         self%dist_sc(n) = 0._dp

         select case (from_to)
         case ('v2p')
            ! sym is always .true.
            dist_b(n) = 1._dp + alpha
            self%coeffs_e(:, 4) = [0._dp, dici, cici + dici, bici + cici, &
                                   aici + bici, &
                                   aici, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 3) = [0._dp, dici, cici, bici, &
                                   aici + dici, &
                                   aici + cici, bici, 0._dp, 0._dp]
            self%coeffs_e(:, 2) = [0._dp, dici, cici, bici, &
                                   aici, &
                                   aici, bici + dici, cici, 0._dp]
         case ('p2v')
            ! sym is always .true.
            self%dist_sa(n) = 2*alpha
            self%coeffs_e(:, 4) = [2*dici, 2*cici, 2*bici, 2*aici, &
                                   0._dp, &
                                   0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 3) = [dici, cici, bici + dici, aici + cici, &
                                   aici + bici, &
                                   0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 2) = [dici, cici, bici, aici, &
                                   aici + dici, &
                                   bici + cici, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, 1) = [dici, cici, bici, aici, &
                                   aici, &
                                   bici, cici + dici, 0._dp, 0._dp]
         end select
      end if

      call self%preprocess(dist_b)

   end subroutine interpl_mid

   subroutine stagder_1st(self, delta, scheme, from_to, bc_start, bc_end, sym)
      implicit none

      class(tdsops_t), intent(inout) :: self
      real(dp), intent(in) :: delta
      character(*), intent(in) :: scheme, from_to
      character(*), optional, intent(in) :: bc_start, bc_end
      logical, optional, intent(in) :: sym

      real(dp), allocatable :: dist_b(:)
      real(dp) :: alpha, aci, bci, cci, dci
      integer :: i, n, n_halo

      if (self%n_halo < 2) error stop 'Staggared deriv require n_halo >= 2'

      ! alpha is alcai

      select case (scheme)
      case ('compact6')
         alpha = 9._dp/62._dp
         aci = 63._dp/62._dp/delta
         bci = 17._dp/62._dp/3._dp/delta
      case default
         error stop 'scheme is not defined'
      end select

      select case (from_to)
      case ('v2p')
         self%coeffs(:) = [0._dp, 0._dp, 0._dp, -bci, &
                           -aci, &
                           aci, bci, 0._dp, 0._dp]
      case ('p2v')
         self%coeffs(:) = [0._dp, 0._dp, -bci, -aci, &
                           aci, &
                           bci, 0._dp, 0._dp, 0._dp]
      end select

      do i = 1, self%n_halo
         self%coeffs_s(:, i) = self%coeffs(:)
         self%coeffs_e(:, i) = self%coeffs(:)
      end do

      self%dist_sa(:) = alpha; self%dist_sc(:) = alpha

      n = self%n
      n_halo = self%n_halo

      allocate(dist_b(n))
      dist_b(:) = 1._dp

      if ((bc_start == 'dirichlet') .or. (bc_start == 'neumann')) then
         self%dist_sa(1) = 0._dp

         select case (from_to)
         case ('v2p')
            ! sym is always .false.
            dist_b(1) = 1._dp + alpha
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   -aci - 2*bci, &
                                   aci + bci, bci, 0._dp, 0._dp]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, -bci, &
                                   -aci, &
                                   aci, bci, 0._dp, 0._dp]
         case ('p2v')
            ! sym is always .true.
            self%dist_sc(1) = 0._dp
            self%coeffs_s(:, 1) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                   0._dp, &
                                   0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_s(:, 2) = [0._dp, 0._dp, 0._dp, -aci - bci, &
                                   aci, &
                                   bci, 0._dp, 0._dp, 0._dp]
         end select
      end if

      if ((bc_end == 'dirichlet') .or. (bc_end == 'neumann')) then
         self%dist_sc(n) = 0._dp

         select case (from_to)
         case ('v2p')
            ! sym is always .false.
            dist_b(n) = 1._dp + alpha
            self%coeffs_e(:, n_halo) = [0._dp, 0._dp, 0._dp, -bci, &
                                        -aci - bci, &
                                        aci + 2*bci, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, n_halo - 1) = [0._dp, 0._dp, 0._dp, -bci, &
                                            -aci, &
                                            aci, bci, 0._dp, 0._dp]
         case ('p2v')
            ! sym is always .true.
            self%dist_sa(n) = 0._dp
            self%coeffs_e(:, n_halo) = [0._dp, 0._dp, 0._dp, 0._dp, &
                                        0._dp, &
                                        0._dp, 0._dp, 0._dp, 0._dp]
            self%coeffs_e(:, n_halo - 1) = [0._dp, 0._dp, -bci, -aci, &
                                            aci + bci, &
                                            0._dp, 0._dp, 0._dp, 0._dp]
         end select
      end if

      call self%preprocess(dist_b)

   end subroutine stagder_1st

   subroutine preprocess(self, dist_b)
      implicit none

      class(tdsops_t), intent(inout) :: self

      real(dp), dimension(:), intent(in) :: dist_b

      integer :: i

      ! start the hybrid algorithm
      do i = 1, 2
         self%dist_sa(i) = self%dist_sa(i)/dist_b(i)
         self%dist_sc(i) = self%dist_sc(i)/dist_b(i)
         self%dist_bw(i) = self%dist_sc(i)
         self%dist_af(i) = 1._dp/dist_b(i)
      end do
      do i = 3, self%n
         self%dist_fw(i) = 1._dp/(dist_b(i) &
                                  - self%dist_sa(i)*self%dist_sc(i - 1))
         self%dist_af(i) = self%dist_sa(i)
         self%dist_sa(i) = -self%dist_fw(i)*self%dist_sa(i) &
                           *self%dist_sa(i - 1)
         self%dist_sc(i) = self%dist_fw(i)*self%dist_sc(i)
      end do
      do i = self%n - 2, 2, -1
         self%dist_sa(i) = self%dist_sa(i) &
                           - self%dist_sc(i)*self%dist_sa(i + 1)
         self%dist_bw(i) = self%dist_sc(i)
         self%dist_sc(i) = -self%dist_sc(i)*self%dist_sc(i + 1)
      end do
      ! dist_fw(1) is never used, so store this extra factor instead
      self%dist_fw(1) = 1._dp/(1._dp - self%dist_sc(1)*self%dist_sa(2))

      self%dist_sa(1) = self%dist_fw(1)*self%dist_sa(1)
      self%dist_sc(1) = -self%dist_fw(1)*self%dist_sc(1)*self%dist_sc(2)

   end subroutine preprocess

end module m_tdsops

