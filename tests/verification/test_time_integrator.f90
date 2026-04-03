program test_omp_adamsbashforth
  use iso_fortran_env, only: stderr => error_unit
  use mpi

  use m_common, only: dp, pi, DIR_X, is_sp
  use m_mesh, only: mesh_t
  use m_allocator, only: allocator_t
  use m_base_backend, only: base_backend_t
  use m_backend_runtime, only: backend_runtime_t
  use m_time_integrator, only: time_intg_t
  use m_field, only: flist_t

  implicit none

  logical :: allpass = .true.
  integer :: i, j, k, stage, istartup, nrank, nproc, ierr
  integer :: nstep0 = 64, nstep, nrun = 3, nmethod = 8
  character(len=3) :: method(8)
  real(dp), allocatable, dimension(:) :: err
  real(dp), allocatable, dimension(:) :: norm
  real(dp) :: dt0 = 0.25_dp, dt, order
  type(flist_t), allocatable :: sol(:)
  type(flist_t), allocatable :: deriv(:)
  real(dp), allocatable, dimension(:, :, :) :: data_array
  integer, dimension(3) :: dims_global, nproc_dir, dims
  real(dp), dimension(3) :: L_global
  character(len=20) :: BC_x(2), BC_y(2), BC_z(2)

  type(backend_runtime_t), target :: runtime
  class(base_backend_t), pointer :: backend
  class(allocator_t), pointer :: allocator
  type(mesh_t), target :: mesh
  class(time_intg_t), allocatable :: time_integrator

  ! initialize MPI
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, nrank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)

  ! Global number of cells in each direction
  dims_global = [1, 1, 1]

  ! Global domain dimensions
  L_global = [2*pi, 2*pi, 2*pi]

  ! Domain decomposition in each direction
  nproc_dir = [1, 1, 1]

  BC_x = ['periodic', 'periodic']
  BC_y = ['periodic', 'periodic']
  BC_z = ['periodic', 'periodic']

  mesh = mesh_t(dims_global, nproc_dir, L_global, BC_x, BC_y, BC_z)

  if (.not. is_sp) dt0 = 0.01_dp

  call runtime%init(mesh)
  allocator => runtime%allocator
  backend => runtime%backend
  if (nrank == 0) then
    print *, trim(runtime%backend_name), 'allocator instantiated'
    print *, trim(runtime%backend_name), 'backend instantiated'
  end if

  ! allocate memory
  allocate (sol(1))
  allocate (deriv(1))
  sol(1)%ptr => allocator%get_block(DIR_X)
  deriv(1)%ptr => allocator%get_block(DIR_X)

  allocate (norm(nrun))
  dims = sol(1)%ptr%get_shape()
  allocate (data_array(dims(1), dims(2), dims(3)))

  method = ['AB1', 'AB2', 'AB3', 'AB4', 'RK1', 'RK2', 'RK3', 'RK4']

  ! compute l2 norm for various step sizes
  do k = 1, nmethod

    ! initialize time-integrator
    time_integrator = time_intg_t(allocator=allocator, &
                                  backend=backend, method=method(k), nvars=1)

    dt = dt0
    nstep = nstep0
    do j = 1, nrun
      ! initial condition
      time_integrator%istep = 1

      ! compute l2 norm for a given step size
      allocate (err(nstep))
      call sol(1)%ptr%fill(1.0_dp)
      call backend%get_field_data(data_array, sol(1)%ptr, DIR_X)

      ! startup
      istartup = time_integrator%nstep - 1
      do i = 1, istartup
        call backend%get_field_data(data_array, sol(1)%ptr, DIR_X)
        call deriv(1)%ptr%fill(dahlquist_rhs(data_array(1, 1, 1)))
        call time_integrator%step(sol, deriv, dt)

        call sol(1)%ptr%fill(dahlquist_exact_sol(real(i, dp)*dt))
      end do

      ! post-startup
      do i = 1, nstep
        do stage = 1, time_integrator%nstage
          call backend%get_field_data(data_array, sol(1)%ptr, DIR_X)
          call deriv(1)%ptr%fill(dahlquist_rhs(data_array(1, 1, 1)))
          call time_integrator%step(sol, deriv, dt)
        end do

        call backend%get_field_data(data_array, sol(1)%ptr, DIR_X)
        err(i) = data_array(1, 1, 1) &
                 - dahlquist_exact_sol(real(i + istartup, dp)*dt)
      end do

      ! compute l2 norms
      norm(j) = norm2(err)
      norm(j) = sqrt(norm(j)*norm(j)/real(nstep, dp))
      print *, norm(j)
      deallocate (err)

      ! refine time stepping
      dt = dt/2.0_dp
      nstep = nstep*2
    end do

    ! check order of convergence
    order = log(norm(nrun - 1)/norm(nrun))/log(2.0_dp)
    print *, 'order', order
    if (abs(order - real(time_integrator%order, dp)) > 0.25_dp) then
      allpass = .false.
      write (stderr, '(a)') 'Check order... failed'
    else
      write (stderr, '(a)') 'Check order... passed'
    end if

    ! deallocate time-integrator for each scheme
    call time_integrator%finalize

  end do

  ! check if all tests are passing
  if (allpass) then
    write (stderr, '(a)') 'ALL TESTS PASSED SUCCESSFULLY.'
  else
    error stop 'SOME TESTS FAILED.'
  end if

  call allocator%release_block(sol(1)%ptr)
  call allocator%release_block(deriv(1)%ptr)

  ! deallocate memory
  deallocate (sol)
  deallocate (deriv)
  deallocate (norm)

  ! finalize MPI
  call MPI_Finalize(ierr)

contains
  function dahlquist_rhs(u)
    implicit none

    real(dp) :: dahlquist_rhs
    real(dp), intent(in) :: u

    real(dp) :: lambda = -1.0_dp

    dahlquist_rhs = lambda*u

  end function dahlquist_rhs

  function dahlquist_exact_sol(time)
    implicit none

    real(dp) :: dahlquist_exact_sol
    real(dp), intent(in) :: time

    real(dp) :: lambda = -1.0_dp

    dahlquist_exact_sol = exp(lambda*time)

  end function dahlquist_exact_sol

end program test_omp_adamsbashforth
