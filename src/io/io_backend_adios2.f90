module m_io_backend_adios2
!! ADIOS2 implementation of the I/O interface
  use m_io_interface, only: io_backend_t, io_file_t, io_mode_read, io_mode_write
  use m_adios2_wrapper, only: adios2_reader_t, adios2_writer_t, adios2_file_t, &
                         adios2_mode_read, adios2_mode_write
  use m_common, only: dp, i8
  
  implicit none
  
  private
  public :: io_adios2_t, io_adios2_file_t
  
  ! ADIOS2 file wrapper
  type, extends(io_file_t) :: io_adios2_file_t
    type(adios2_file_t) :: adios2_file
  end type io_adios2_file_t
  
  ! ADIOS2 backend implementation
  type, extends(io_backend_t) :: io_adios2_t
    type(adios2_reader_t) :: reader
    type(adios2_writer_t) :: writer
    logical :: is_reader_mode = .false.
  contains
    procedure :: init => adios2_init
    procedure :: open => adios2_open
    procedure :: close => adios2_close
    procedure :: read_scalar_real => adios2_read_scalar_real
    procedure :: read_scalar_i8 => adios2_read_scalar_i8
    procedure :: read_array_3d_real => adios2_read_array_3d_real
    procedure :: write_scalar_real => adios2_write_scalar_real
    procedure :: write_array_3d_real => adios2_write_array_3d_real
    procedure :: begin_step => adios2_begin_step
    procedure :: end_step => adios2_end_step
    procedure :: finalise => adios2_finalise
  end type io_adios2_t

contains

  subroutine adios2_init(self, comm, io_name)
    class(io_adios2_t), intent(inout) :: self
    integer, intent(in) :: comm
    character(len=*), intent(in) :: io_name
    
    ! Initialize both reader and writer - we'll use the appropriate one based on mode
    call self%reader%init(comm, trim(io_name) // "_reader")
    call self%writer%init(comm, trim(io_name) // "_writer")
  end subroutine adios2_init

  function adios2_open(self, filename, mode, comm) result(file)
    class(io_adios2_t), intent(inout) :: self
    character(len=*), intent(in) :: filename
    integer, intent(in) :: mode
    integer, intent(in), optional :: comm
    class(io_file_t), allocatable :: file
    
    type(io_adios2_file_t), allocatable :: adios2_file
    
    allocate(adios2_file)
    
    if (mode == io_mode_read) then
      self%is_reader_mode = .true.
      if (present(comm)) then
        adios2_file%adios2_file = self%reader%open(filename, adios2_mode_read, comm)
      else
        adios2_file%adios2_file = self%reader%open(filename, adios2_mode_read)
      end if
    else ! io_mode_write
      self%is_reader_mode = .false.
      if (present(comm)) then
        adios2_file%adios2_file = self%writer%open(filename, adios2_mode_write, comm)
      else
        adios2_file%adios2_file = self%writer%open(filename, adios2_mode_write)
      end if
    end if
    
    call move_alloc(adios2_file, file)
  end function adios2_open

  subroutine adios2_close(self, file)
    class(io_adios2_t), intent(inout) :: self
    class(io_file_t), intent(inout) :: file
    
    select type(file)
    type is (io_adios2_file_t)
      if (self%is_reader_mode) then
        call self%reader%close(file%adios2_file)
      else
        call self%writer%close(file%adios2_file)
      end if
    end select
  end subroutine adios2_close

  subroutine adios2_read_scalar_real(self, name, data, file)
    class(io_adios2_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(out) :: data
    class(io_file_t), intent(inout) :: file
    
    select type(file)
    type is (io_adios2_file_t)
      call self%reader%read_data(name, data, file%adios2_file)
    end select
  end subroutine adios2_read_scalar_real

  subroutine adios2_read_scalar_i8(self, name, data, file)
    class(io_adios2_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    integer(i8), intent(out) :: data
    class(io_file_t), intent(inout) :: file
    
    select type(file)
    type is (io_adios2_file_t)
      call self%reader%read_data(name, data, file%adios2_file)
    end select
  end subroutine adios2_read_scalar_i8

  subroutine adios2_read_array_3d_real(self, name, data, file, start_dims, count_dims)
    class(io_adios2_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), dimension(:,:,:), allocatable, intent(out) :: data
    class(io_file_t), intent(inout) :: file
    integer(i8), dimension(3), intent(in) :: start_dims, count_dims
    
    select type(file)
    type is (io_adios2_file_t)
      call self%reader%read_data(name, data, file%adios2_file, start_dims, count_dims)
    end select
  end subroutine adios2_read_array_3d_real

  subroutine adios2_write_scalar_real(self, name, data, file)
    class(io_adios2_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: data
    class(io_file_t), intent(inout) :: file
    
    select type(file)
    type is (io_adios2_file_t)
      call self%writer%write_data(name, data, file%adios2_file)
    end select
  end subroutine adios2_write_scalar_real

  subroutine adios2_write_array_3d_real(self, name, data, file, shape_dims, start_dims, count_dims)
    class(io_adios2_t), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp), dimension(:,:,:), intent(in) :: data
    class(io_file_t), intent(inout) :: file
    integer(i8), dimension(3), intent(in) :: shape_dims, start_dims, count_dims
    
    select type(file)
    type is (io_adios2_file_t)
      call self%writer%write_data(name, data, file%adios2_file, shape_dims, start_dims, count_dims)
    end select
  end subroutine adios2_write_array_3d_real

  subroutine adios2_begin_step(self, file)
    class(io_adios2_t), intent(inout) :: self
    class(io_file_t), intent(inout) :: file
    
    select type(file)
    type is (io_adios2_file_t)
      if (self%is_reader_mode) then
        call self%reader%begin_step(file%adios2_file)
      else
        call self%writer%begin_step(file%adios2_file)
      end if
    end select
  end subroutine adios2_begin_step

  subroutine adios2_end_step(self, file)
    class(io_adios2_t), intent(inout) :: self
    class(io_file_t), intent(inout) :: file
    
    select type(file)
    type is (io_adios2_file_t)
      if (self%is_reader_mode) then
        call self%reader%end_step(file%adios2_file)
      else
        call self%writer%end_step(file%adios2_file)
      end if
    end select
  end subroutine adios2_end_step

  subroutine adios2_finalise(self)
    class(io_adios2_t), intent(inout) :: self
    
    call self%reader%finalise()
    call self%writer%finalise()
  end subroutine adios2_finalise

end module m_io_adios2
