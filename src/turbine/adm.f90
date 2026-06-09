module m_adm
  !! Actuator Disc Model (ADM) turbine forcing (iturbine = 2).
  !!
  !! Each turbine is a disc that applies a thrust force computed from a thrust
  !! coefficient and the disc-averaged inflow velocity. The force is smeared
  !! onto the grid with a per-disc super-Gaussian kernel (gamma) built once at
  !! init, normalised to sum to one. The disc-averaged velocity is then just
  !! the backend scalar product of gamma with the velocity field (which already
  !! performs the global MPI reduction), and the smeared force is accumulated
  !! into the momentum RHS with a single vecadd per component.
  !!
  !! Adapted from the actuator disc implementation by S. Hatfield
  !! (semi-h/windturbines), ported to the turbine_model_t interface.
  use iso_fortran_env, only: stderr => error_unit
  use mpi

  use m_allocator, only: allocator_t
  use m_base_backend, only: base_backend_t
  use m_common, only: dp, pi, MPI_X3D2_DP, DIR_X, DIR_C, VERT
  use m_field, only: field_t
  use m_mesh, only: mesh_t
  use m_turbine_model, only: turbine_model_t

  implicit none

  type :: actuator_disc_t
    real(dp) :: coords(3)         !! disc centre
    real(dp) :: yaw, tilt         !! rotor yaw/tilt angles [rad]
    real(dp) :: D, area           !! diameter and swept area
    real(dp) :: C_T, alpha        !! thrust and induction coefficients
    real(dp) :: rot_N(3)          !! unit rotor-axis vector
    real(dp) :: U_disc            !! instantaneous disc-averaged speed
    real(dp) :: thrust, power     !! instantaneous thrust and power
    class(field_t), pointer :: gamma_disc => null() !! smearing kernel (DIR_X)
  end type actuator_disc_t

  type, extends(turbine_model_t) :: adm_t
    integer :: n_turb = 0
    real(dp) :: rho_air = 1._dp
    real(dp) :: cell_vol
    character(len=256) :: coords_file = ''
    type(actuator_disc_t), allocatable :: disc(:)
    class(base_backend_t), pointer :: backend => null()
    type(mesh_t), pointer :: mesh => null()
    type(allocator_t), pointer :: host_allocator => null()
  contains
    procedure :: init => init_adm
    procedure :: update => update_adm
    procedure :: project_forces => project_forces_adm
    procedure :: write_output => write_output_adm
  end type adm_t

contains

  subroutine init_adm(self, backend, mesh, host_allocator, dt)
    implicit none
    class(adm_t), intent(inout) :: self
    class(base_backend_t), target, intent(in) :: backend
    type(mesh_t), target, intent(in) :: mesh
    type(allocator_t), target, intent(in) :: host_allocator
    real(dp), intent(in) :: dt

    real(dp), allocatable :: disc_params(:, :)
    class(field_t), pointer :: gamma_host
    integer :: t, i, j, k, dims(3), ierr
    real(dp) :: coords(3), delta, delta_N, delta_R, disc_thick
    real(dp) :: dx, dy, dz, gamma_val, gamma_tot
    real(dp) :: yaw, tilt, D, rot_N(3)

    self%backend => backend
    self%mesh => mesh
    self%host_allocator => host_allocator
    self%cell_vol = product(mesh%geo%d)

    call read_ad_file(self%coords_file, disc_params)
    self%n_turb = size(disc_params, dim=1)
    allocate (self%disc(self%n_turb))

    dims = mesh%get_dims(VERT)
    gamma_host => self%host_allocator%get_block(DIR_C)

    do t = 1, self%n_turb
      yaw = disc_params(t, 4)*pi/180._dp
      tilt = disc_params(t, 5)*pi/180._dp
      D = disc_params(t, 6)
      rot_N(1) = cos(yaw)*cos(tilt)
      rot_N(2) = sin(tilt)
      rot_N(3) = sin(yaw)

      self%disc(t)%coords(:) = disc_params(t, 1:3)
      self%disc(t)%yaw = yaw
      self%disc(t)%tilt = tilt
      self%disc(t)%D = D
      self%disc(t)%C_T = disc_params(t, 7)
      self%disc(t)%alpha = disc_params(t, 8)
      self%disc(t)%rot_N(:) = rot_N(:)
      self%disc(t)%area = pi*(D**2)/4._dp

      ! Build the (un-normalised) super-Gaussian smearing kernel on the host:
      ! Gaussian in the rotor-normal direction (delta_N), super-Gaussian (^8)
      ! in the radial direction (delta_R) to give a flat-topped disc.
      gamma_host%data(:, :, :) = 0._dp
      gamma_tot = 0._dp
      delta = sqrt((mesh%geo%d(1)*rot_N(1))**2 &
                   + (mesh%geo%d(2)*rot_N(2))**2 &
                   + (mesh%geo%d(3)*rot_N(3))**2)
      disc_thick = max(D/8._dp, delta*1.5_dp)
      do k = 1, dims(3)
        do j = 1, dims(2)
          do i = 1, dims(1)
            coords = mesh%get_coordinates(i, j, k)
            dx = coords(1) - self%disc(t)%coords(1)
            dy = coords(2) - self%disc(t)%coords(2)
            dz = coords(3) - self%disc(t)%coords(3)
            delta_N = dx*rot_N(1) + dy*rot_N(2) - dz*rot_N(3)

            dx = coords(1) - delta_N*rot_N(1)
            dy = coords(2) - delta_N*rot_N(2)
            dz = coords(3) + delta_N*rot_N(3)
            delta_R = sqrt((dx - self%disc(t)%coords(1))**2 &
                           + (dy - self%disc(t)%coords(2))**2 &
                           + (dz - self%disc(t)%coords(3))**2)

            gamma_val = exp(-((delta_N/(disc_thick/2._dp))**2 &
                              + (delta_R/(D/2._dp))**8))
            gamma_host%data(i, j, k) = gamma_val
            gamma_tot = gamma_tot + gamma_val
          end do
        end do
      end do

      ! Normalise so the kernel sums to one across all ranks. The disc-averaged
      ! velocity is then scalar_product(gamma_disc, u).
      call MPI_Allreduce(MPI_IN_PLACE, gamma_tot, 1, MPI_X3D2_DP, &
                         MPI_SUM, MPI_COMM_WORLD, ierr)
      gamma_host%data(:, :, :) = gamma_host%data(:, :, :)/gamma_tot

      self%disc(t)%gamma_disc => self%backend%allocator%get_block(DIR_X)
      call self%backend%set_field_data(self%disc(t)%gamma_disc, &
                                       gamma_host%data)
      call self%disc(t)%gamma_disc%set_data_loc(VERT)
    end do

    call self%host_allocator%release_block(gamma_host)
  end subroutine init_adm

  subroutine update_adm(self, t, dt)
    implicit none
    class(adm_t), intent(inout) :: self
    real(dp), intent(in) :: t, dt
    ! The disc model has no internal kinematics to advance. Velocity-filtering
    ! (legacy T_relax) could be added here as a follow-up.
  end subroutine update_adm

  subroutine project_forces_adm(self, du, dv, dw, u, v, w)
    implicit none
    class(adm_t), intent(inout) :: self
    class(field_t), intent(inout) :: du, dv, dw
    class(field_t), intent(in) :: u, v, w

    integer :: t
    real(dp) :: u_avg, v_avg, w_avg, rot_N(3), C_T_prime
    real(dp) :: coeff_x, coeff_y, coeff_z

    do t = 1, self%n_turb
      rot_N(:) = self%disc(t)%rot_N(:)

      ! Disc-averaged velocity: gamma is normalised, so the scalar product is
      ! the gamma-weighted average. scalar_product already reduces over ranks.
      u_avg = self%backend%scalar_product(self%disc(t)%gamma_disc, u)
      v_avg = self%backend%scalar_product(self%disc(t)%gamma_disc, v)
      w_avg = self%backend%scalar_product(self%disc(t)%gamma_disc, w)
      self%disc(t)%U_disc = u_avg*rot_N(1) + v_avg*rot_N(2) - w_avg*rot_N(3)

      ! Thrust from the local (Calaf-style) thrust coefficient.
      C_T_prime = self%disc(t)%C_T/(1._dp - self%disc(t)%alpha)**2
      self%disc(t)%thrust = 0.5_dp*self%rho_air*C_T_prime &
                            *self%disc(t)%U_disc**2*self%disc(t)%area
      self%disc(t)%power = self%disc(t)%thrust*self%disc(t)%U_disc

      ! Smear the thrust (opposing the inflow) onto the momentum RHS.
      coeff_x = -self%disc(t)%thrust*rot_N(1)/self%cell_vol
      coeff_y = -self%disc(t)%thrust*rot_N(2)/self%cell_vol
      coeff_z = self%disc(t)%thrust*rot_N(3)/self%cell_vol
      call self%backend%vecadd(coeff_x, self%disc(t)%gamma_disc, 1._dp, du)
      call self%backend%vecadd(coeff_y, self%disc(t)%gamma_disc, 1._dp, dv)
      call self%backend%vecadd(coeff_z, self%disc(t)%gamma_disc, 1._dp, dw)
    end do
  end subroutine project_forces_adm

  subroutine write_output_adm(self, iter, is_root)
    implicit none
    class(adm_t), intent(inout) :: self
    integer, intent(in) :: iter
    logical, intent(in) :: is_root
    integer :: t

    if (.not. is_root) return
    do t = 1, self%n_turb
      print '(A,I0,A,I0,3(A,ES12.5))', ' ADM disc ', t, ' iter ', iter, &
        ': U_disc = ', self%disc(t)%U_disc, &
        '  thrust = ', self%disc(t)%thrust, &
        '  power = ', self%disc(t)%power
    end do
  end subroutine write_output_adm

  subroutine read_ad_file(filename, disc_params)
    !! Read turbine disc parameters from a `.ad` file. Format: one header line
    !! followed by one row per disc of
    !!   CoR(x) CoR(y) CoR(z) Yaw[deg] Tilt[deg] RotorDiam C_T alpha
    implicit none
    character(*), intent(in) :: filename
    real(dp), allocatable, intent(out) :: disc_params(:, :)

    integer :: unit, ios, n_turb, t
    character(len=512) :: line

    if (len_trim(filename) == 0) then
      error stop 'ADM: no disc coordinates file specified (adm_coords).'
    end if

    open (newunit=unit, file=trim(filename), status='old', action='read', &
          iostat=ios)
    if (ios /= 0) then
      write (stderr, *) 'ADM: cannot open disc coordinates file: ', &
        trim(filename)
      error stop
    end if

    ! Count data rows (skip the header line).
    read (unit, '(A)', iostat=ios) line  ! header
    n_turb = 0
    do
      read (unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      if (len_trim(line) == 0) cycle
      n_turb = n_turb + 1
    end do

    if (n_turb == 0) error stop 'ADM: no discs found in coordinates file.'

    allocate (disc_params(n_turb, 8))
    rewind (unit)
    read (unit, '(A)', iostat=ios) line  ! header again
    do t = 1, n_turb
      read (unit, *, iostat=ios) disc_params(t, :)
      if (ios /= 0) error stop 'ADM: error reading disc coordinates row.'
    end do
    close (unit)
  end subroutine read_ad_file

end module m_adm
