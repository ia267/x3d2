module m_case_foil
  use iso_fortran_env, only: stderr => error_unit
  use mpi

  use m_allocator, only: allocator_t
  use m_base_backend, only: base_backend_t
  use m_base_case, only: base_case_t
  use m_common, only: dp, pi, MPI_X3D2_DP, DIR_C, DIR_X, VERT, &
                      X_FACE, Y_FACE, BC_DIRICHLET
  use m_field, only: field_t
  use m_mesh, only: mesh_t
  use m_solver, only: init

  implicit none

  ! Free-stream velocity (streamwise, +x). Inflow and far-field value.
  real(dp), parameter :: u_inflow = 14.5_dp
  ! Thickness (in x-vertices) of the damping layer before the outflow.
  integer, parameter :: damping_layer_size = 50

  type, extends(base_case_t) :: case_foil_t
    real(dp) :: out_vel = 0._dp
    real(dp) :: flow_rate_diff = 0._dp
    ! Persistent device sponge-coefficient field (DIR_X, VERT). Zero in
    ! the interior and ramping up to one across the damping layer just
    ! before the x-outflow (downstream of the foils). Built once in
    ! initial_conditions and released at program end.
    class(field_t), pointer :: sponge_dev => null()
  contains
    procedure :: define_BC => define_BC_foil
    procedure :: initial_conditions => initial_conditions_foil
    procedure :: forcings => forcings_foil
    procedure :: apply_BC => apply_BC_foil
    procedure :: postprocess => postprocess_foil
    procedure :: case_finalise => finalise_foil
    procedure :: compute_outflow_params
  end type case_foil_t

  interface case_foil_t
    module procedure case_foil_init
  end interface case_foil_t

contains

  function case_foil_init(backend, mesh, host_allocator) result(flow_case)
    implicit none

    class(base_backend_t), target, intent(inout) :: backend
    type(mesh_t), target, intent(inout) :: mesh
    type(allocator_t), target, intent(inout) :: host_allocator
    type(case_foil_t) :: flow_case

    call flow_case%case_init(backend, mesh, host_allocator)

  end function case_foil_init

  subroutine define_BC_foil(self)
    implicit none

    class(case_foil_t) :: self

    ! Sample the convective-outflow parameters from solver%u once per
    ! substep and cache them on self, so apply_BC (later in the substep)
    ! can read them. No writes to solver%u/v/w here.
    call self%compute_outflow_params(self%out_vel, self%flow_rate_diff)
  end subroutine define_BC_foil

  ! ==========================================================================
  ! Compute outflow convective velocity number and inlet/outlet flow-rate
  ! imbalance from solver%u. Mirrors the cylinder case. Single rank here,
  ! but the MPI reductions are kept so the logic stays correct if the
  ! constraint is lifted later.
  ! ==========================================================================
  subroutine compute_outflow_params(self, out_vel, flow_rate_diff)
    implicit none
    class(case_foil_t) :: self
    real(dp), intent(out) :: out_vel, flow_rate_diff

    integer :: dims(3), nx, ierr
    real(dp) :: uxmax, uxmax_discard
    real(dp) :: flow_rate_in, flow_rate_out
    real(dp) :: flow_rate_in_max_discard, flow_rate_out_max_discard
    real(dp) :: fl_sums(2), ny_nz
    real(dp) :: dx, gdt

    dims = self%solver%mesh%get_dims(VERT)
    nx = dims(1)
    dx = self%solver%mesh%geo%d(1)
    ny_nz = real(dims(2)*dims(3), dp)
    gdt = self%solver%time_integrator%gdt

    call self%solver%backend%slice_max_sum( &
      uxmax, uxmax_discard, self%solver%u, nx - 1)
    call self%solver%backend%slice_max_sum( &
      flow_rate_in_max_discard, flow_rate_in, self%solver%u, 1)
    call self%solver%backend%slice_max_sum( &
      flow_rate_out_max_discard, flow_rate_out, self%solver%u, nx)

    call MPI_Allreduce(MPI_IN_PLACE, uxmax, 1, MPI_X3D2_DP, &
                       MPI_MAX, MPI_COMM_WORLD, ierr)
    fl_sums(1) = flow_rate_in
    fl_sums(2) = flow_rate_out
    call MPI_Allreduce(MPI_IN_PLACE, fl_sums, 2, MPI_X3D2_DP, MPI_SUM, &
                       MPI_COMM_WORLD, ierr)
    flow_rate_in = fl_sums(1)/ny_nz
    flow_rate_out = fl_sums(2)/ny_nz

    out_vel = uxmax*gdt/dx
    flow_rate_diff = flow_rate_in - flow_rate_out
  end subroutine compute_outflow_params

  subroutine initial_conditions_foil(self)
    implicit none

    class(case_foil_t) :: self

    integer, dimension(3) :: dims
    class(field_t), pointer :: sponge_host
    integer :: i, l

    dims = self%solver%mesh%get_dims(VERT)

    ! Build the sponge coefficient: zero in the interior (no forcing) and
    ! ramping up to one across the damping layer just upstream of the
    ! x-outflow (largest i).
    sponge_host => self%solver%host_allocator%get_block(DIR_C)
    sponge_host%data = 0.0_dp
    do l = 1, damping_layer_size
      i = dims(1) - damping_layer_size + l
      sponge_host%data(i, :, :) = &
        1.0_dp - cos(pi*l/(damping_layer_size*2.0_dp))
    end do

    self%sponge_dev => self%solver%backend%allocator%get_block(DIR_X, VERT)
    call self%sponge_dev%set_data_loc(VERT)
    call self%solver%backend%set_field_data(self%sponge_dev, sponge_host%data)

    call self%solver%host_allocator%release_block(sponge_host)

    call self%solver%u%fill(u_inflow)
    call self%solver%v%fill(0._dp)
    call self%solver%w%fill(0._dp)

    call self%solver%u%set_data_loc(VERT)
    call self%solver%v%set_data_loc(VERT)
    call self%solver%w%set_data_loc(VERT)

  end subroutine initial_conditions_foil

  subroutine forcings_foil(self, du, dv, dw, iter)
    implicit none

    class(case_foil_t) :: self
    class(field_t), intent(inout) :: du, dv, dw
    integer, intent(in) :: iter

    class(field_t), pointer :: tmp

    ! Sponge layer: relax u towards u_inflow with strength sponge_dev,
    ! i.e. du = du + sponge*(u_inflow - u).
    tmp => self%solver%backend%allocator%get_block(DIR_X, VERT)

    call tmp%fill(u_inflow)
    ! tmp = u_inflow - u
    call self%solver%backend%vecadd(-1._dp, self%solver%u, 1._dp, tmp)
    ! tmp = sponge*(u_inflow - u)
    call self%solver%backend%vecmult(tmp, self%sponge_dev)
    ! du = du + tmp
    call self%solver%backend%vecadd(1._dp, tmp, 1._dp, du)

    call self%solver%backend%allocator%release_block(tmp)

  end subroutine forcings_foil

  ! ==========================================================================
  ! Pre-correction (called per substep after the integrator step):
  !   x-faces: Dirichlet free-stream inflow (i = 1) and convective outflow
  !            (i = nx), with the global flow-rate correction.
  !   y-faces: Dirichlet free-stream far-field (top and bottom).
  !   z-faces: periodic (handled by the solver, nothing to do here).
  ! ==========================================================================
  subroutine apply_BC_foil(self, u, v, w)
    implicit none

    class(case_foil_t) :: self
    class(field_t), intent(inout) :: u, v, w

    ! Streamwise x-faces: free-stream inflow + convective outflow.
    call self%solver%backend%field_set_face( &
      u, u_inflow, self%out_vel, X_FACE, &
      bc_start=BC_DIRICHLET, bc_end=BC_DIRICHLET, &
      flow_rate_diff=self%flow_rate_diff)
    call self%solver%backend%field_set_face( &
      v, 0._dp, self%out_vel, X_FACE, &
      bc_start=BC_DIRICHLET, bc_end=BC_DIRICHLET, &
      flow_rate_diff=self%flow_rate_diff)
    call self%solver%backend%field_set_face( &
      w, 0._dp, self%out_vel, X_FACE, &
      bc_start=BC_DIRICHLET, bc_end=BC_DIRICHLET, &
      flow_rate_diff=self%flow_rate_diff)

    ! Lift-normal y-faces: free-stream far-field (Dirichlet both sides).
    call self%solver%backend%field_set_face(u, u_inflow, u_inflow, Y_FACE)
    call self%solver%backend%field_set_face(v, 0._dp, 0._dp, Y_FACE)
    call self%solver%backend%field_set_face(w, 0._dp, 0._dp, Y_FACE)

  end subroutine apply_BC_foil

  subroutine postprocess_foil(self, iter, t)
    implicit none

    class(case_foil_t) :: self
    integer, intent(in) :: iter
    real(dp), intent(in) :: t

    if (self%solver%mesh%par%is_root()) then
      print *, 'time =', t, 'iteration =', iter
      print '(A, ES12.5, A, ES12.5)', &
        ' out_vel = ', self%out_vel, &
        '  flow_rate_diff = ', self%flow_rate_diff
    end if

    call self%monitoring%write_step( &
      self%solver, t, self%solver%u, self%solver%v, self%solver%w)

  end subroutine postprocess_foil

  subroutine finalise_foil(self)
    implicit none
    class(case_foil_t) :: self

    if (associated(self%sponge_dev)) then
      call self%solver%backend%allocator%release_block(self%sponge_dev)
    end if

    ! Call parent's finalisation
    call self%base_case_t%case_finalise()

  end subroutine finalise_foil

end module m_case_foil
