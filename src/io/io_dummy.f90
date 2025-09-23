module m_io_dummy
!! Dummy implementation of the general I/O interface for when ADIOS2 is not available

  use iso_fortran_env, only: stderr => error_unit
  use m_io_base, only: io_reader_t, io_writer_t, io_file_t, io_mode_read, io_mode_write
  use m_common, only: dp, i8

  implicit none

  private
  public :: io_dummy_reader_t, io_dummy_writer_t, io_dummy_file_t

  type, extends(io_file_t) :: io_dummy_file_t
    logical :: is_open = .false.
  contains
    procedure :: close => file_close
    procedure :: begin_step => file_begin_step
    procedure :: end_step => file_end_step
  end type io_dummy_file_t

  type, extends(io_reader_t) :: io_dummy_reader_t
    logical :: initialised = .false.
  contains
    procedure :: init => reader_init
    procedure :: open => reader_open
    procedure :: finalise => reader_finalise
    
    generic :: read_data => read_data_i8, read_data_integer, read_data_real, read_data_array_3d
    procedure, private :: read_data_i8 => reader_read_data_i8
    procedure, private :: read_data_integer => reader_read_data_integer
    procedure, private :: read_data_real => reader_read_data_real
    procedure, private :: read_data_array_3d => reader_read_data_array_3d
  end type io_dummy_reader_t

  type, extends(io_writer_t) :: io_dummy_writer_t
    logical :: initialised = .false.
  contains
    procedure :: init => writer_init
    procedure :: open => writer_open
    procedure :: finalise => writer_finalise
    
    generic :: write_data => write_data_i8, write_data_integer, write_data_real, write_data_array_3d
    procedure, private :: write_data_i8 => writer_write_data_i8
    procedure, private :: write_data_integer => writer_write_data_integer
    procedure, private :: write_data_real => writer_write_data_real
    procedure, private :: write_data_array_3d => writer_write_data_array_3d
  end type io_dummy_writer_t

contains

  subroutine file_close(self)
    class(io_dummy_file_t), intent(inout) :: self
    write(stderr, '(A)') "WARNING: Dummy I/O backend - file close operation ignored"
    self%is_open = .false.
  end subroutine file_close

  subroutine file_begin_step(self)
    class(io_dummy_file_t), intent(inout) :: self
    write(stderr, '(A)') "WARNING: Dummy I/O backend - begin_step operation ignored"
  end subroutine file_begin_step

  subroutine file_end_step(self)
    class(io_dummy_file_t), intent(inout) :: self
    write(stderr, '(A)') "WARNING: Dummy I/O backend - end_step operation ignored"
  end subroutine file_end_step

  subroutine reader_init(self, comm, name)
    class(io_dummy_reader_t), intent(inout) :: self
    integer, intent(in) :: comm
    character(len=*), intent(in) :: name
    write(stderr, '(A)') "WARNING: Using dummy I/O backend - ADIOS2 not available"
    write(stderr, '(A)') "I/O operations will not function properly"
    self%initialised = .true.
  end subroutine reader_init

  function reader_open(self, filename, mode, comm) result(file_handle)
    class(io_dummy_reader_t), intent(inout) :: self
    character(len=*), intent(in) :: filename
    integer, intent(in) :: mode
    integer, intent(in) :: comm
    class(io_file_t), allocatable :: file_handle

    type(io_dummy_file_t), allocatable :: dummy_file

    write(stderr, '(A)') "ERROR: Cannot open file '" // trim(filename) // "' - ADIOS2 not available"
    write(stderr, '(A)') "Compilation was done without ADIOS2 support"
    write(stderr, '(A)') "Please recompile with -DWITH_ADIOS2=ON to enable I/O functionality"

    allocate(dummy_file)
    dummy_file%is_open = .false.

    call move_alloc(dummy_file, file_handle)
  end function reader_open

  subroutine reader_read_data_i8(self, variable_name, value, file_handle)
    class(io_dummy_reader_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    integer(i8), intent(out) :: value
    class(io_file_t), intent(inout) :: file_handle

    write(stderr, '(A)') "ERROR: Cannot read scalar i8 '" // trim(variable_name) // "' - ADIOS2 not available"
    value = 0_i8
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine reader_read_data_i8

  subroutine reader_read_data_integer(self, variable_name, value, file_handle)
    class(io_dummy_reader_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    integer, intent(out) :: value
    class(io_file_t), intent(inout) :: file_handle

    write(stderr, '(A)') "ERROR: Cannot read scalar integer '" // trim(variable_name) // "' - ADIOS2 not available"
    value = 0
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine reader_read_data_integer

  subroutine reader_read_data_real(self, variable_name, value, file_handle)
    class(io_dummy_reader_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    real(dp), intent(out) :: value
    class(io_file_t), intent(inout) :: file_handle

    write(stderr, '(A)') "ERROR: Cannot read scalar real '" // trim(variable_name) // "' - ADIOS2 not available"
    value = 0.0_dp
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine reader_read_data_real

  subroutine reader_read_data_array_3d(self, variable_name, array, file_handle, shape_dims, start_dims, count_dims)
    class(io_dummy_reader_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    real(dp), intent(inout) :: array(:, :, :)
    class(io_file_t), intent(inout) :: file_handle
    integer(i8), intent(in), optional :: shape_dims(3)
    integer(i8), intent(in), optional :: start_dims(3)
    integer(i8), intent(in), optional :: count_dims(3)

    write(stderr, '(A)') "ERROR: Cannot read 3D array '" // trim(variable_name) // "' - ADIOS2 not available"
    array = 0.0_dp
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine reader_read_data_array_3d

  subroutine reader_finalise(self)
    class(io_dummy_reader_t), intent(inout) :: self
    write(stderr, '(A)') "WARNING: Dummy I/O backend - finalise operation ignored"
    self%initialised = .false.
  end subroutine reader_finalise

  subroutine writer_init(self, comm, name)
    class(io_dummy_writer_t), intent(inout) :: self
    integer, intent(in) :: comm
    character(len=*), intent(in) :: name
    write(stderr, '(A)') "WARNING: Using dummy I/O backend - ADIOS2 not available"
    write(stderr, '(A)') "I/O operations will not function properly"
    self%initialised = .true.
  end subroutine writer_init

  function writer_open(self, filename, mode, comm) result(file_handle)
    class(io_dummy_writer_t), intent(inout) :: self
    character(len=*), intent(in) :: filename
    integer, intent(in) :: mode
    integer, intent(in) :: comm
    class(io_file_t), allocatable :: file_handle

    type(io_dummy_file_t), allocatable :: dummy_file

    write(stderr, '(A)') "ERROR: Cannot open file '" // trim(filename) // "' for writing - ADIOS2 not available"
    write(stderr, '(A)') "Compilation was done without ADIOS2 support"
    write(stderr, '(A)') "Please recompile with -DWITH_ADIOS2=ON to enable I/O functionality"

    allocate(dummy_file)
    dummy_file%is_open = .false.

    call move_alloc(dummy_file, file_handle)
  end function writer_open

  subroutine writer_write_data_i8(self, variable_name, value, file_handle)
    class(io_dummy_writer_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    integer(i8), intent(in) :: value
    class(io_file_t), intent(inout) :: file_handle

    write(stderr, '(A)') "ERROR: Cannot write scalar i8 '" // trim(variable_name) // "' - ADIOS2 not available"
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine writer_write_data_i8

  subroutine writer_write_data_integer(self, variable_name, value, file_handle)
    class(io_dummy_writer_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    integer, intent(in) :: value
    class(io_file_t), intent(inout) :: file_handle

    write(stderr, '(A)') "ERROR: Cannot write scalar integer '" // trim(variable_name) // "' - ADIOS2 not available"
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine writer_write_data_integer

  subroutine writer_write_data_real(self, variable_name, value, file_handle)
    class(io_dummy_writer_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    real(dp), intent(in) :: value
    class(io_file_t), intent(inout) :: file_handle

    write(stderr, '(A)') "ERROR: Cannot write scalar real '" // trim(variable_name) // "' - ADIOS2 not available"
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine writer_write_data_real

  subroutine writer_write_data_array_3d(self, variable_name, array, file_handle, shape_dims, start_dims, count_dims)
    class(io_dummy_writer_t), intent(inout) :: self
    character(len=*), intent(in) :: variable_name
    real(dp), intent(in) :: array(:, :, :)
    class(io_file_t), intent(inout) :: file_handle
    integer(i8), intent(in), optional :: shape_dims(3)
    integer(i8), intent(in), optional :: start_dims(3)
    integer(i8), intent(in), optional :: count_dims(3)

    write(stderr, '(A)') "ERROR: Cannot write 3D array '" // trim(variable_name) // "' - ADIOS2 not available"
    error stop "I/O operation failed: ADIOS2 support not compiled"
  end subroutine writer_write_data_array_3d

  subroutine writer_finalise(self)
    class(io_dummy_writer_t), intent(inout) :: self
    write(stderr, '(A)') "WARNING: Dummy I/O backend - finalise operation ignored"
    self%initialised = .false.
  end subroutine writer_finalise

end module m_io_dummy
