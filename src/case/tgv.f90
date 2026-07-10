module m_case_tgv
  use iso_fortran_env, only: stderr => error_unit

  use m_allocator, only: allocator_t
  use m_base_backend, only: base_backend_t
  use m_base_case, only: base_case_t
  use m_common, only: dp, VERT, DIR_C, DIR_X, DIR_Z, get_argument
  use m_field, only: field_t
  use m_mesh, only: mesh_t
  use m_scalar_series, only: scalar_series_t
  use m_solver, only: init

  implicit none

  type, extends(base_case_t) :: case_tgv_t
    !! TGV-specific scalar time-series output.
    !! Columns match the Dairay et al. (2017) reference data (TGV_Re1600.dat):
    !!   E_k       = (1/2N) sum(u^2 + v^2 + w^2)
    !!   enstrophy = (1/2N) sum(|curl u|^2)          [= Dzeta in reference]
    !!   epsilon   = 2 * nu * enstrophy               [viscous dissipation rate]
    !!   div_u_max = max(|div u|)                     [incompressibility check]
    type(scalar_series_t), private :: tgv_series
    integer, private :: ngrid = 1
    real(dp), private :: nu = 0._dp
  contains
    procedure :: define_BC => define_BC_tgv
    procedure :: initial_conditions => initial_conditions_tgv
    procedure :: forcings => forcings_tgv
    procedure :: apply_BC => apply_BC_tgv
    procedure :: postprocess => postprocess_tgv
    procedure :: finalise_case_specific => finalise_tgv
  end type case_tgv_t

  interface case_tgv_t
    module procedure case_tgv_init
  end interface case_tgv_t

contains

  function case_tgv_init(backend, mesh, host_allocator) result(flow_case)
    implicit none

    class(base_backend_t), target, intent(inout) :: backend
    type(mesh_t), target, intent(inout) :: mesh
    type(allocator_t), target, intent(inout) :: host_allocator
    type(case_tgv_t) :: flow_case

    character(len=16), parameter :: cols(4) = &
      ['E_k             ', 'enstrophy       ', 'epsilon         ', &
       'div_u_max       ']
    logical :: is_restart

    call flow_case%case_init(backend, mesh, host_allocator)

    flow_case%ngrid = product(mesh%get_global_dims(VERT))
    flow_case%nu = flow_case%solver%nu

    is_restart = flow_case%io_mgr%is_restart()
    call flow_case%tgv_series%init('tgv_monitoring.csv', cols, &
                                   mesh%par%is_root(), is_restart)

  end function case_tgv_init

  subroutine initial_conditions_tgv(self)
    implicit none

    class(case_tgv_t) :: self

    call self%set_init(self%solver%u, u_func)
    call self%set_init(self%solver%v, v_func)
    call self%solver%w%fill(0._dp)

    call self%solver%u%set_data_loc(VERT)
    call self%solver%v%set_data_loc(VERT)
    call self%solver%w%set_data_loc(VERT)

  end subroutine initial_conditions_tgv

  pure function u_func(coords) result(r)
    implicit none

    real(dp), intent(in) :: coords(3)
    real(dp) :: r

    r = sin(coords(1))*cos(coords(2))*cos(coords(3))
  end function u_func

  pure function v_func(coords) result(r)
    implicit none

    real(dp), intent(in) :: coords(3)
    real(dp) :: r

    r = -cos(coords(1))*sin(coords(2))*cos(coords(3))
  end function v_func

  subroutine define_BC_tgv(self)
    implicit none

    class(case_tgv_t) :: self

    ! do nothing for TGV case
  end subroutine define_BC_tgv

  subroutine forcings_tgv(self, du, dv, dw, iter)
    implicit none

    class(case_tgv_t) :: self
    class(field_t), intent(inout) :: du, dv, dw
    integer, intent(in) :: iter

    ! do nothing for TGV case
  end subroutine forcings_tgv

  subroutine apply_BC_tgv(self, u, v, w)
    implicit none

    class(case_tgv_t) :: self
    class(field_t), intent(inout) :: u, v, w

    ! do nothing for TGV case
  end subroutine apply_BC_tgv

  subroutine postprocess_tgv(self, iter, t)
    implicit none

    class(case_tgv_t) :: self
    integer, intent(in) :: iter
    real(dp), intent(in) :: t

    class(field_t), pointer :: du, dv, dw, div_u
    real(dp) :: E_k, enstrophy, epsilon, div_u_max, div_u_mean_discard

    ! Kinetic energy: E_k = (1/2N) * sum(u^2 + v^2 + w^2)
    E_k = 0.5_dp*(self%solver%backend%scalar_product(self%solver%u, self%solver%u) &
                + self%solver%backend%scalar_product(self%solver%v, self%solver%v) &
                + self%solver%backend%scalar_product(self%solver%w, self%solver%w)) &
          /self%ngrid

    ! Enstrophy (= Dzeta in TGV_Re1600.dat col 5): (1/2N) * sum(|curl u|^2)
    ! curl is computed once here; monitoring%write_step is NOT called so the
    ! curl is not duplicated.
    du => self%solver%backend%allocator%get_block(DIR_X, VERT)
    dv => self%solver%backend%allocator%get_block(DIR_X, VERT)
    dw => self%solver%backend%allocator%get_block(DIR_X, VERT)

    call self%solver%curl(du, dv, dw, self%solver%u, self%solver%v, self%solver%w)

    enstrophy = 0.5_dp*(self%solver%backend%scalar_product(du, du) &
                      + self%solver%backend%scalar_product(dv, dv) &
                      + self%solver%backend%scalar_product(dw, dw)) &
                /self%ngrid

    call self%solver%backend%allocator%release_block(du)
    call self%solver%backend%allocator%release_block(dv)
    call self%solver%backend%allocator%release_block(dw)

    ! Viscous dissipation rate: epsilon = 2*nu*enstrophy
    ! (exact for incompressible periodic flow; matches col 4 of TGV_Re1600.dat)
    epsilon = 2.0_dp*self%nu*enstrophy

    ! Divergence max (incompressibility check)
    div_u => self%solver%backend%allocator%get_block(DIR_Z)
    call self%solver%divergence_v2p(div_u, self%solver%u, self%solver%v, self%solver%w)
    call self%solver%backend%field_max_mean(div_u_max, div_u_mean_discard, div_u)
    call self%solver%backend%allocator%release_block(div_u)

    if (self%solver%mesh%par%is_root()) then
      print '(A,ES12.5,A,I0)', ' time = ', t, '  iter = ', iter
      print '(3(A,ES12.5))', '  E_k = ', E_k, '  enstrophy = ', enstrophy, &
                              '  epsilon = ', epsilon
      print '(A,ES12.5)',    '  div_u_max = ', div_u_max
    end if

    call self%tgv_series%write_step(t, [E_k, enstrophy, epsilon, div_u_max])

  end subroutine postprocess_tgv

  subroutine finalise_tgv(self)
    implicit none
    class(case_tgv_t) :: self

    call self%tgv_series%finalise()
  end subroutine finalise_tgv

end module m_case_tgv
