module m_io_factory
!! Factory for creating appropriate I/O backend based on build configuration
  use m_io_interface, only: io_backend_t
  
  implicit none
  
  private
  public :: create_io_backend, io_backend_type_adios2, io_backend_type_dummy
  
  ! I/O backend type constants
  integer, parameter :: io_backend_type_adios2 = 1
  integer, parameter :: io_backend_type_dummy = 2
  integer, parameter :: io_backend_type_mpi_io = 3  ! For future use

contains

  function create_io_backend(backend_type) result(backend)
    !! Factory function to create appropriate I/O backend
    integer, intent(in), optional :: backend_type
    class(io_backend_t), allocatable :: backend
    
    integer :: actual_backend_type
    
    ! Determine which backend to use
    if (present(backend_type)) then
      actual_backend_type = backend_type
    else
      ! Auto-detect based on compile-time options
      actual_backend_type = get_default_backend_type()
    end if
    
    select case (actual_backend_type)
    case (io_backend_type_adios2)
      call create_adios2_backend(backend)
    case (io_backend_type_dummy)
      call create_dummy_backend(backend)
    case default
      ! Fall back to dummy
      call create_dummy_backend(backend)
    end select
    
  end function create_io_backend
  
  function get_default_backend_type() result(backend_type)
    !! Determine the default backend type based on compile-time configuration
    integer :: backend_type
    
#ifdef WITH_ADIOS2
    backend_type = io_backend_type_adios2
#else
    backend_type = io_backend_type_dummy
#endif
  end function get_default_backend_type
  
  subroutine create_adios2_backend(backend)
    !! Create ADIOS2 I/O backend
#ifdef WITH_ADIOS2
    use m_io_backend_adios2, only: io_adios2_t
#endif
    use m_io_backend_dummy, only: io_dummy_t
    class(io_backend_t), allocatable, intent(out) :: backend
    
#ifdef WITH_ADIOS2
    allocate(io_adios2_t :: backend)
#else
    ! Fall back to dummy if ADIOS2 not available
    allocate(io_dummy_t :: backend)
#endif
  end subroutine create_adios2_backend
  
  subroutine create_dummy_backend(backend)
    !! Create dummy I/O backend
    use m_io_backend_dummy, only: io_dummy_t
    class(io_backend_t), allocatable, intent(out) :: backend
    
    allocate(io_dummy_t :: backend)
  end subroutine create_dummy_backend

end module m_io_factory
