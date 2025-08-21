module m_io_backend_dummy
!! Dummy implementation of the I/O interface for when ADIOS2 is not available
  use m_io_interface, only: io_backend_t, io_file_t, io_mode_read, io_mode_write
  use m_common, only: dp, i8
  use iso_fortran_env, only: stderr => error_unit
  use mpi, only: MPI_Comm_rank, MPI_COMM_WORLD
  
  implicit none
  
  private
  public :: io_dummy_t, io_dummy_file_t
  
  ! Dummy file type
  type, extends(io_file_t) :: io_dummy_file_t
    ! Empty - no actual file handling
  end type io_dummy_file_t
  
  ! Dummy backend implementation
  type, extends(io_backend_t) :: io_dummy_t
    logical :: warning_printed = .false.
  contains
    procedure :: init => dummy_init
    procedure :: open => dummy_open
    procedure :: close => dummy_close
    procedure :: read_scalar_real => dummy_read_scalar_real
    procedure :: read_scalar_i8 => dummy_read_scalar_i8
    procedure :: read_array_3d_real => dummy_read_array_3d_real
    procedure :: write_scalar_real => dummy_write_scalar_real
    procedure :: write_array_3d_real => dummy_write_array_3d_real
    procedure :: begin_step => dummy_begin_step
    procedure :: end_step => dummy_end_step
    procedure :: finalise => dummy_finalise
    procedure :: print_warning_once => dummy_print_warning_once
  end type io_dummy_t

contains

  subroutine dummy_print_warning_once(self, operation)
    class(io_dummy_t), intent(inout) :: self
    character(len=*), intent(in) :: operation
    integer :: rank, ierr
    
    if (.not. self%warning_printed) then
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
      if (rank == 0) then
        write(stderr, '(A)') "WARNING: I/O operation '" // trim(operation) // "' requested"
        write(stderr, '(A)') "         but ADIOS2 is not enabled. Operation will be ignored."
        write(stderr, '(A)') "         Recompile with -DWITH_ADIOS2=ON to enable I/O functionality."
      end if
      self%warning_printed = .true.
    end if
  end subroutine dummy_print_warning_once

  subroutine dummy_init(self, comm, io_name)
    class(io_dummy_t), intent(inout) :: self
    integer, intent(in) :: comm
    character(len=*), intent(in) :: io_name
    
    call self%print_warning_once("init")
  end subroutine dummy_init

  function dummy_open(self, filename, mode, comm) result(file)
    class(io_dummy_t), intent(inout) :: self
    character(len=*), intent(in) :: filename
    integer, intent(in) :: mode
    integer, intent(in), optional :: comm
    class(io_file_t), allocatable :: file
    
    type(io_dummy_file_t), allocatable :: dummy_file
    
    call self%print_warning_once("open")
    
    allocate(dummy_file)
    call move_alloc(dummy_file, file)
  end function dummy_open

  subroutine dummy_close(self, file)
    class(io_dummy_t), intent(inout) :: self
    class(io_file_t), intent(inout) :: file
    
    ! Do nothing
  end subroutine dummy_close

  subroutine dummy_read_scalar_real(self, name, data, file)
    class(io_dummy_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(out) :: data
    class(io_file_t), intent(inout) :: file
    
    ! Return default value
    data = 0.0_dp
  end subroutine dummy_read_scalar_real

  subroutine dummy_read_scalar_i8(self, name, data, file)
    class(io_dummy_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    integer(i8), intent(out) :: data
    class(io_file_t), intent(inout) :: file
    
    ! Return default value (0 = disable IBM)
    data = 0_i8
  end subroutine dummy_read_scalar_i8

  subroutine dummy_read_array_3d_real(self, name, data, file, start_dims, count_dims)
    class(io_dummy_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), dimension(:,:,:), allocatable, intent(out) :: data
    class(io_file_t), intent(inout) :: file
    integer(i8), dimension(3), intent(in) :: start_dims, count_dims
    
    ! Allocate array with default values
    allocate(data(count_dims(1), count_dims(2), count_dims(3)))
    data = 0.0_dp
  end subroutine dummy_read_array_3d_real

  subroutine dummy_write_scalar_real(self, name, data, file)
    class(io_dummy_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: data
    class(io_file_t), intent(inout) :: file
    
    ! Do nothing
  end subroutine dummy_write_scalar_real

  subroutine dummy_write_array_3d_real(self, name, data, file, shape_dims, start_dims, count_dims)
    class(io_dummy_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), dimension(:,:,:), intent(in) :: data
    class(io_file_t), intent(inout) :: file
    integer(i8), dimension(3), intent(in) :: shape_dims, start_dims, count_dims
    
    ! Do nothing
  end subroutine dummy_write_array_3d_real

  subroutine dummy_begin_step(self, file)
    class(io_dummy_t), intent(inout) :: self
    class(io_file_t), intent(inout) :: file
    
    ! Do nothing
  end subroutine dummy_begin_step

  subroutine dummy_end_step(self, file)
    class(io_dummy_t), intent(inout) :: self
    class(io_file_t), intent(inout) :: file
    
    ! Do nothing
  end subroutine dummy_end_step

  subroutine dummy_finalise(self)
    class(io_dummy_t), intent(inout) :: self
    
    ! Do nothing
  end subroutine dummy_finalise

end module m_io_backend_dummy
