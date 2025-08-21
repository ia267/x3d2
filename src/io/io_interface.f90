module m_io_interface
!! Generic I/O interface that can be implemented by different backends
!! (ADIOS2, MPI-IO, HDF5, etc.)
  use m_common, only: dp, i8
  use mpi, only: MPI_COMM_NULL
  
  implicit none
  
  private
  public :: io_backend_t, io_file_t, io_mode_read, io_mode_write
  
  ! I/O mode constants
  integer, parameter :: io_mode_read = 1
  integer, parameter :: io_mode_write = 2
  
  ! Abstract file type
  type, abstract :: io_file_t
  end type io_file_t
  
  ! Abstract I/O backend interface
  type, abstract :: io_backend_t
  contains
    procedure(init_interface), deferred :: init
    procedure(open_interface), deferred :: open
    procedure(close_interface), deferred :: close
    procedure(read_scalar_real_interface), deferred :: read_scalar_real
    procedure(read_scalar_i8_interface), deferred :: read_scalar_i8
    procedure(read_array_3d_real_interface), deferred :: read_array_3d_real
    procedure(write_scalar_real_interface), deferred :: write_scalar_real
    procedure(write_array_3d_real_interface), deferred :: write_array_3d_real
    procedure(begin_step_interface), deferred :: begin_step
    procedure(end_step_interface), deferred :: end_step
    procedure(finalise_interface), deferred :: finalise
  end type io_backend_t
  
  ! Abstract interfaces
  abstract interface
    subroutine init_interface(self, comm, io_name)
      import :: io_backend_t
      class(io_backend_t), intent(inout) :: self
      integer, intent(in) :: comm
      character(len=*), intent(in) :: io_name
    end subroutine init_interface
    
    function open_interface(self, filename, mode, comm) result(file)
      import :: io_backend_t, io_file_t
      class(io_backend_t), intent(inout) :: self
      character(len=*), intent(in) :: filename
      integer, intent(in) :: mode
      integer, intent(in), optional :: comm
      class(io_file_t), allocatable :: file
    end function open_interface
    
    subroutine close_interface(self, file)
      import :: io_backend_t, io_file_t
      class(io_backend_t), intent(inout) :: self
      class(io_file_t), intent(inout) :: file
    end subroutine close_interface
    
    subroutine read_scalar_real_interface(self, name, data, file)
      import :: io_backend_t, io_file_t, dp
      class(io_backend_t), intent(inout) :: self
      character(len=*), intent(in) :: name
      real(dp), intent(out) :: data
      class(io_file_t), intent(inout) :: file
    end subroutine read_scalar_real_interface
    
    subroutine read_scalar_i8_interface(self, name, data, file)
      import :: io_backend_t, io_file_t, i8
      class(io_backend_t), intent(inout) :: self
      character(len=*), intent(in) :: name
      integer(i8), intent(out) :: data
      class(io_file_t), intent(inout) :: file
    end subroutine read_scalar_i8_interface
    
    subroutine read_array_3d_real_interface(self, name, data, file, start_dims, count_dims)
      import :: io_backend_t, io_file_t, dp, i8
      class(io_backend_t), intent(inout) :: self
      character(len=*), intent(in) :: name
      real(dp), dimension(:,:,:), allocatable, intent(out) :: data
      class(io_file_t), intent(inout) :: file
      integer(i8), dimension(3), intent(in) :: start_dims, count_dims
    end subroutine read_array_3d_real_interface
    
    subroutine write_scalar_real_interface(self, name, data, file)
      import :: io_backend_t, io_file_t, dp
      class(io_backend_t), intent(inout) :: self
      character(len=*), intent(in) :: name
      real(dp), intent(in) :: data
      class(io_file_t), intent(inout) :: file
    end subroutine write_scalar_real_interface
    
    subroutine write_array_3d_real_interface(self, name, data, file, shape_dims, start_dims, count_dims)
      import :: io_backend_t, io_file_t, dp, i8
      class(io_backend_t), intent(inout) :: self
      character(len=*), intent(in) :: name
      real(dp), dimension(:,:,:), intent(in) :: data
      class(io_file_t), intent(inout) :: file
      integer(i8), dimension(3), intent(in) :: shape_dims, start_dims, count_dims
    end subroutine write_array_3d_real_interface
    
    subroutine begin_step_interface(self, file)
      import :: io_backend_t, io_file_t
      class(io_backend_t), intent(inout) :: self
      class(io_file_t), intent(inout) :: file
    end subroutine begin_step_interface
    
    subroutine end_step_interface(self, file)
      import :: io_backend_t, io_file_t
      class(io_backend_t), intent(inout) :: self
      class(io_file_t), intent(inout) :: file
    end subroutine end_step_interface
    
    subroutine finalise_interface(self)
      import :: io_backend_t
      class(io_backend_t), intent(inout) :: self
    end subroutine finalise_interface
  end interface

end module m_io_interface
