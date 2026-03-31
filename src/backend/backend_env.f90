module m_backend_env
  use mpi

  use m_allocator, only: allocator_t
  use m_base_backend, only: base_backend_t
  use m_common, only: VERT
  use m_mesh, only: mesh_t

#ifdef CUDA
  use cudafor
  use m_cuda_allocator, only: cuda_allocator_t
  use m_cuda_backend, only: cuda_backend_t
  use m_cuda_common, only: SZ
#else
  use m_omp_backend, only: omp_backend_t
  use m_omp_common, only: SZ
#endif

  implicit none

  integer, parameter :: backend_sz = SZ

#ifdef CUDA
  logical, parameter :: backend_is_cuda = .true.
#else
  logical, parameter :: backend_is_cuda = .false.
#endif

  type :: backend_env_t
    class(base_backend_t), pointer :: backend => null()
    class(allocator_t), pointer :: allocator => null()
    type(allocator_t), pointer :: host_allocator => null()
    character(len=8) :: backend_name = ''
#ifdef CUDA
    type(cuda_backend_t) :: cuda_backend
    type(cuda_allocator_t) :: cuda_allocator
#else
    type(omp_backend_t) :: omp_backend
    type(allocator_t) :: omp_allocator
#endif
    type(allocator_t) :: host_allocator_storage
  contains
    procedure :: init
  end type backend_env_t

contains

  subroutine init(self, mesh, separate_host_allocator)
    class(backend_env_t), target, intent(inout) :: self
    type(mesh_t), target, intent(inout) :: mesh
    logical, optional, intent(in) :: separate_host_allocator

    integer :: dims(3)
    logical :: need_separate_host_allocator

#ifdef CUDA
    integer :: ierr, nrank, ndevs, devnum
#endif

    dims = mesh%get_dims(VERT)
    need_separate_host_allocator = .false.
    if (present(separate_host_allocator)) then
      need_separate_host_allocator = separate_host_allocator
    end if

#ifdef CUDA
    call MPI_Comm_rank(MPI_COMM_WORLD, nrank, ierr)
    ierr = cudaGetDeviceCount(ndevs)
    if (ndevs < 1) then
      error stop 'backend_env_t%init: no CUDA devices available'
    end if
    ierr = cudaSetDevice(mod(nrank, ndevs))
    ierr = cudaGetDevice(devnum)

    self%backend_name = 'CUDA'
    self%cuda_allocator = cuda_allocator_t(dims, SZ)
    self%allocator => self%cuda_allocator
    self%host_allocator_storage = allocator_t(dims, SZ)
    self%host_allocator => self%host_allocator_storage
    self%cuda_backend = cuda_backend_t(mesh, self%allocator)
    self%backend => self%cuda_backend
#else
    self%backend_name = 'OMP'
    self%omp_allocator = allocator_t(dims, SZ)
    self%allocator => self%omp_allocator

    if (need_separate_host_allocator) then
      self%host_allocator_storage = allocator_t(dims, SZ)
      self%host_allocator => self%host_allocator_storage
    else
      self%host_allocator => self%omp_allocator
    end if

    self%omp_backend = omp_backend_t(mesh, self%allocator)
    self%backend => self%omp_backend
#endif

  end subroutine init

end module m_backend_env
