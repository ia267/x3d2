module m_cuda_tdsops
  use iso_fortran_env, only: stderr => error_unit

  use m_common, only: dp
  use m_tdsops, only: tdsops_t, tdsops_init

  implicit none

  type, extends(tdsops_t) :: cuda_tdsops_t
    !! CUDA extension of the Tridiagonal Solver Operators class.
    !!
    !! Regular tdsops_t class is initiated and the coefficient arrays are
    !! copied into device arrays so that cuda kernels can use them.
    real(dp), device, allocatable :: dist_fw_dev(:), dist_bw_dev(:), &
                                     dist_sa_dev(:), dist_sc_dev(:), &
                                     dist_af_dev(:)
    real(dp), device, allocatable :: thom_f_dev(:), thom_s_dev(:), &
                                     thom_w_dev(:), thom_p_dev(:)
    real(dp), device, allocatable :: stretch_dev(:), stretch_correct_dev(:)
    real(dp), device, allocatable :: coeffs_dev(:), &
                                     coeffs_s_dev(:, :), coeffs_e_dev(:, :)
  contains
    procedure :: cuda_tdsops_init
  end type cuda_tdsops_t

contains

  subroutine cuda_tdsops_init( &
    self, n_tds, delta, operation, scheme, bc_start, bc_end, &
    stretch, stretch_correct, n_halo, from_to, sym, c_nu, nu0_nu &
    )
    !! Initialiser for the cuda_tdsops_t class.
    !! See tdsops_t for details.
    implicit none

    class(cuda_tdsops_t), intent(inout) :: self

    integer, intent(in) :: n_tds
    real(dp), intent(in) :: delta
    character(*), intent(in) :: operation, scheme
    integer, intent(in) :: bc_start, bc_end
    real(dp), optional, intent(in) :: stretch(:), stretch_correct(:)
    integer, optional, intent(in) :: n_halo
    character(*), optional, intent(in) :: from_to
    logical, optional, intent(in) :: sym
    real(dp), optional, intent(in) :: c_nu, nu0_nu

    integer :: n, n_stencil

    self%tdsops_t = tdsops_init(n_tds, delta, operation, scheme, bc_start, &
                                bc_end, stretch, stretch_correct, n_halo, &
                                from_to, sym, c_nu, nu0_nu)

    n = self%n_rhs
    allocate (self%dist_fw_dev(n), self%dist_bw_dev(n))
    allocate (self%dist_sa_dev(n), self%dist_sc_dev(n))
    allocate (self%dist_af_dev(n))
    allocate (self%thom_f_dev(n), self%thom_s_dev(n))
    allocate (self%thom_w_dev(n), self%thom_p_dev(n))

    allocate (self%stretch_dev(self%n_tds))
    allocate (self%stretch_correct_dev(self%n_tds))

    n_stencil = 2*self%n_halo + 1
    allocate (self%coeffs_dev(n_stencil))
    allocate (self%coeffs_s_dev(n_stencil, self%n_halo))
    allocate (self%coeffs_e_dev(n_stencil, self%n_halo))

    self%dist_fw_dev(:) = self%dist_fw(:)
    self%dist_bw_dev(:) = self%dist_bw(:)
    self%dist_sa_dev(:) = self%dist_sa(:)
    self%dist_sc_dev(:) = self%dist_sc(:)
    self%dist_af_dev(:) = self%dist_af(:)

    self%thom_f_dev(:) = self%thom_f(:)
    self%thom_s_dev(:) = self%thom_s(:)
    self%thom_w_dev(:) = self%thom_w(:)
    self%thom_p_dev(:) = self%thom_p(:)

    self%stretch_dev(:) = self%stretch(:)
    self%stretch_correct_dev(:) = self%stretch_correct(:)

    self%coeffs_dev(:) = self%coeffs(:)
    self%coeffs_s_dev(:, :) = self%coeffs_s(:, :)
    self%coeffs_e_dev(:, :) = self%coeffs_e(:, :)

  end subroutine cuda_tdsops_init

end module m_cuda_tdsops

