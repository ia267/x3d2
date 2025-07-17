module m_case_foil
  use iso_fortran_env, only: stderr => error_unit
  use mpi

  use m_allocator, only: allocator_t, field_t
  use m_base_backend, only: base_backend_t
  use m_base_case, only: base_case_t
  use m_common, only: dp, pi, get_argument, DIR_C, VERT, CELL, Y_FACE
  use m_mesh, only: mesh_t
  use m_solver, only: init

  implicit none

  type, extends(base_case_t) :: case_foil_t
  contains
    procedure :: boundary_conditions => boundary_conditions_foil
    procedure :: initial_conditions => initial_conditions_foil
    procedure :: postprocess => postprocess_foil
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

  subroutine boundary_conditions_foil(self)
    implicit none

    class(case_foil_t) :: self

    integer, dimension(3) :: dims
    integer :: damping_layer_size
    class(field_t), pointer :: ramp
    type(field_t), pointer :: ramp_dev

    ! Number of cells used in the damping region
    damping_layer_size = 50

    dims = mesh%get_dims(VERT)
    ramp => self%solver%host_allocator%get_block(DIR_C)

    do j = dims(2) - damping_layer_size
      ramp%data(:, j, :) = 1.0_dp
    end do

    do l = 1, damping_layer_size
      ramp%data(:, dims(2) - damping_layer_size + j, :) = cos(pi*l/(damping_layer_size*2))
    end do

    ramp_dev => self%backend%allocator%get_block(DIR_X)
    call self%solver%backend%set_field_data(ramp_dev, ramp%data)

    call self%host_allocator%release_block(ramp)


    ! Gradially dampen velocity
    call self%solver%backend%vecmult(self%solver%u, ramp_dev)
    call self%solver%backend%vecmult(self%solver%v, ramp_dev)
    call self%solver%backend%vecmult(self%solver%w, ramp_dev)

    ! Gradually force V to be inlet velocity
    ! construct: U_inlet * (1 - ramp)
    call self%solver%backend%field_scale(ramp_dev, -1.0_dp)
    call self%solver%backend%field_shift(ramp_dev, 1.0_dp)
    call self%solver%backend%field_scale(ramp_dev, 14.5_dp)
    call self%solver%backend%vecmult(self%solver%v, ramp_dev)

    call self%solver%backend%allocator%release_block(ramp_dev)


  end subroutine boundary_conditions_foil

  subroutine initial_conditions_foil(self)
    implicit none

    class(case_foil_t) :: self

    class(field_t), pointer :: u_init, v_init, w_init

    u_init => self%solver%host_allocator%get_block(DIR_C)
    v_init => self%solver%host_allocator%get_block(DIR_C)
    w_init => self%solver%host_allocator%get_block(DIR_C)

    u_init%data(:, :, :) = 0.0_dp
    v_init%data(:, :, :) = 14.5_dp
    w_init%data(:, :, :) = 0.0_dp

    call self%solver%backend%set_field_data(self%solver%u, u_init%data)
    call self%solver%backend%set_field_data(self%solver%v, v_init%data)
    call self%solver%backend%set_field_data(self%solver%w, w_init%data)

    call self%solver%host_allocator%release_block(u_init)
    call self%solver%host_allocator%release_block(v_init)
    call self%solver%host_allocator%release_block(w_init)

    call self%solver%u%set_data_loc(VERT)
    call self%solver%v%set_data_loc(VERT)
    call self%solver%w%set_data_loc(VERT)

  end subroutine initial_conditions_foil

  subroutine postprocess_foil(self, iter, t)
    implicit none

    class(case_foil_t) :: self
    integer, intent(in) :: iter
    real(dp), intent(in) :: t

    if (self%solver%mesh%par%is_root()) then
      print *, 'time =', t, 'iteration =', iter
    end if

    call self%print_div_max_mean(self%solver%u, self%solver%v, self%solver%w)

  end subroutine postprocess_foil

end module m_case_foil
