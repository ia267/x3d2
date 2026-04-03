program xcompact
  use mpi

  use m_base_case, only: base_case_t
  use m_common, only: dp, get_argument
  use m_config, only: domain_config_t, solver_config_t
  use m_backend_runtime, only: backend_runtime_t, backend_is_cuda
  use m_mesh
  use m_case_channel, only: case_channel_t
  use m_case_cylinder, only: case_cylinder_t
  use m_case_generic, only: case_generic_t
  use m_case_tgv, only: case_tgv_t

  implicit none

  type(mesh_t), target :: mesh
  type(backend_runtime_t), target :: backend_runtime
  class(base_case_t), allocatable :: flow_case

  real(dp) :: t_start, t_end

  type(domain_config_t) :: domain_cfg
  type(solver_config_t) :: solver_cfg
  character(32) :: backend_name
  integer :: nrank, nproc, ierr
  logical :: use_2decomp

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, nrank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)

  if (nrank == 0) then
    print *, 'Parallel run with', nproc, 'ranks'
    print *, 'Data precision is', dp
  end if

  if (backend_is_cuda) then
    backend_name = 'CUDA'
  else
    backend_name = 'OMP'
  end if

  call domain_cfg%read(nml_file=get_argument(1))
  call solver_cfg%read(nml_file=get_argument(1))

  if (product(domain_cfg%nproc_dir) /= nproc) then
    if (nrank == 0) print *, 'nproc_dir specified in the input file does &
                              &not match the total number of ranks, falling &
                              &back to a 1D decomposition along Z-dir instead.'
    domain_cfg%nproc_dir = [1, 1, nproc]
  end if

  ! Decide whether 2decomp is used or not
  use_2decomp = solver_cfg%poisson_solver_type == 'FFT' &
                .and. trim(backend_name) == 'OMP'

  mesh = mesh_t(domain_cfg%dims_global, domain_cfg%nproc_dir, &
                domain_cfg%L_global, domain_cfg%BC_x, domain_cfg%BC_y, &
                domain_cfg%BC_z, domain_cfg%stretching, domain_cfg%beta, &
                use_2decomp=use_2decomp)

  call backend_runtime%init(mesh)
  if (nrank == 0) then
    print *, trim(backend_runtime%backend_name), 'allocator instantiated'
    print *, trim(backend_runtime%backend_name), 'backend instantiated'
  end if

  if (nrank == 0) print *, 'Flow case: ', domain_cfg%flow_case_name

  select case (trim(domain_cfg%flow_case_name))
  case ('channel')
    allocate (case_channel_t :: flow_case)
    flow_case = case_channel_t(backend_runtime%backend, mesh, &
                               backend_runtime%host_allocator)
  case ('cylinder')
    allocate (case_cylinder_t :: flow_case)
    flow_case = case_cylinder_t(backend_runtime%backend, mesh, &
                                backend_runtime%host_allocator)
  case ('generic')
    allocate (case_generic_t :: flow_case)
    flow_case = case_generic_t(backend_runtime%backend, mesh, &
                               backend_runtime%host_allocator)
  case ('tgv')
    allocate (case_tgv_t :: flow_case)
    flow_case = case_tgv_t(backend_runtime%backend, mesh, &
                           backend_runtime%host_allocator)
  case default
    error stop 'Undefined flow_case.'
  end select
  if (nrank == 0) print *, 'solver instantiated'

  call cpu_time(t_start)

  call flow_case%run()

  call cpu_time(t_end)

  if (nrank == 0) print *, 'Time: ', t_end - t_start

  call MPI_Finalize(ierr)

end program xcompact
