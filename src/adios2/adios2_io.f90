module m_adios2_io
!! This module contains ADIOS2 (ADaptable Input Output System version 2)
!! operations for reading and writing data. ADIOS2 transports data as 
!! groups of self-describing variables and attributes across different 
!! media types (e.g., files, memory, network).
!! ADIOS2 APIs are based on:
!! - MPI (although non-MPI serial code is also supported)
!! - Deferred/prefetch/grouped variables transport mode by default
!! - Engine abstraction for reusing the APIs for different transport modes
    use adios2
    use iso_fortran_env, only: real32, real64, int64
    use m_allocator, only: field_t
    use m_common, only: DIR_C
    implicit none

    private
    public :: base_adios2_t, adios2_writer_t, adios2_reader_t

    ! ADIOS2 module parameters
    public :: adios2_mode_write, adios2_mode_append, adios2_mode_read

    ! module parameters
    integer, parameter :: ndims_2d = 2   !! 2D array dimension
    integer, parameter :: ndims_3d = 3   !! 3D array dimension

    !> ADIOS2 base type 
    !> Abstract base module for ADIOS2 operations
    !> Defines the abstract base type `base_adios2_t` for common ADIOS2 components
    !> Abstract base type for ADIOS2 operations
    !> This type provides common ADIOS2 attributes for reading and writing operations
    type, abstract :: base_adios2_t
    private
        type(adios2_adios) :: adios              !! ADIOS2 global handler
        type(adios2_io) :: io                    !! ADIOS2 IO object for managing I/O
        type(adios2_engine) :: engine            !! ADIOS2 engine for data reading/writing
        logical :: is_step_active = .false.      !! Flag to track if a step is active
        integer :: ierr = 0                      !! Error code for ADIOS2 operations

        !> IO configuration parameters
        integer :: output_stride(3) = [1, 1, 1]  !! Output stride for 3D arrays
        integer :: output_precision = real64     !! Output precision (single/double precision)
    contains
        ! common operations
        procedure, public :: init                !! Initialises ADIOS2
        procedure, public :: open                !! Opens an ADIOS2 engine
        procedure, public :: close               !! Closes the ADIOS2 session
        procedure, public :: end_step            !! Ends a step in the ADIOS2 engine
        procedure, public :: handle_error        !! Error handling for ADIOS2 operations
        procedure, public :: setup_io            !! Setup ADIOS2 IO output

        ! deferred implementations
        procedure(begin_step), deferred, public :: begin_step
    end type base_adios2_t

    !> ADIOS2 writer type
    type, extends(base_adios2_t) :: adios2_writer_t
    contains
        procedure, public :: begin_step => begin_step_writer
        procedure, public :: write_fields

        generic, public :: write_data => write_scalar_real, write_array_2d_real

        procedure, private :: write_scalar_real    !! Writes scalar real data
        procedure, private :: write_array_2d_real  !! Writes 2d array real data
        procedure, private :: write_array_3d_real  !! Writes 3d array real data
    end type adios2_writer_t

    !> ADIOS2 reader type
    type, extends(base_adios2_t) :: adios2_reader_t
    contains
        procedure, public :: begin_step => begin_step_reader

        generic, public :: read_data => read_scalar_real, read_array2d_real

        procedure, private :: read_scalar_real      !! Reads scalar real data
        procedure, private :: read_array2d_real     !! Reads 2d array real data
        procedure, private :: read_array3d_real     !! Reads 3d array real data
    end type adios2_reader_t

    abstract interface
        !> Begins a step in the ADIOS2 engine
        subroutine begin_step(self)
            import :: base_adios2_t
            class(base_adios2_t), intent(inout) :: self
        end subroutine begin_step
    end interface

    !public :: adios2_mode_read, adios2_mode_write, adios2_mode_append

contains

    !> Initialises ADIOS2
    !> self: Instance of `base_adios2_t`
    !> comm: MPI communicator (use `MPI_COMM_WORLD` for parallel runs)
    !> io_name: unique name associated with IO component inside ADIOS2
    subroutine init(self, comm, io_name)
        class(base_adios2_t), intent(inout) :: self
        integer, intent(in) :: comm
        !> io that spawns an engine based on its configuration
        character(len=*), intent(in) :: io_name

        ! create adios handler passing the communicator and error flag
        call adios2_init(self%adios, comm, self%ierr)
        call self%handle_error(self%ierr, "Failed to initialise ADIOS2")

        ! declare IO process configuration inside adios
        call adios2_declare_io(self%io, self%adios, io_name, self%ierr)
        call self%handle_error(self%ierr, "Failed to declare ADIOS2 IO object")
    end subroutine init

    !> Opens an ADIOS2 engine
    !> filename: Unique engine identifier within io
    !> mode: Opening mode (write, append, read)
    subroutine open(self, filename, mode, comm)
        class(base_adios2_t), intent(inout) :: self
        character(len=*), intent(in) :: filename   !! Unique engine identifier within io
        integer, intent(in) :: mode                !! Opening mode (write, append, read)
        integer, intent(in), optional :: comm      !! MPI communicator (optional)

        if (present(comm)) then
            call adios2_open(self%engine, self%io, filename, mode, comm, self%ierr)
        else
            call adios2_open(self%engine, self%io, filename, mode, self%ierr)
        end if
        call self%handle_error(self%ierr, "Failed to open ADIOS2 engine")
    end subroutine open

    !> Closes ADIOS2 session
    subroutine close(self)
        class(base_adios2_t), intent(inout) :: self

        if (self%is_step_active) call self%end_step()

        call adios2_close(self%engine, self%ierr)
        call self%handle_error(self%ierr, "Failed to close ADIOS2 engine")

        call adios2_finalize(self%adios, self%ierr)
        call self%handle_error(self%ierr, "Failed to finalise ADIOS2")
    end subroutine close

    !> Ends a step in the ADIOS2 engine
    subroutine end_step(self)
        class(base_adios2_t), intent(inout) :: self

        if (.not. self%is_step_active) return
        
        call adios2_end_step(self%engine, self%ierr)
        call self%handle_error(self%ierr, "Failed to end ADIOS2 step")
        self%is_step_active = .false.
    end subroutine end_step

    !> Handles ADIOS2 errors
    subroutine handle_error(self, ierr, message)
        class(base_adios2_t), intent(inout) :: self
        integer, intent(in) :: ierr              !! Error code from ADIOS2 operations
        character(len=*), intent(in) :: message  !! Error message to display

        self%ierr = ierr
        if (ierr /= 0) then
            print *, "ADIOS2 Error: ", message
            print *, "Error code: ", ierr
            error stop
        end if  
    end subroutine handle_error

    !> Configure I/O settings and ADIOS2 operations
    subroutine setup_io(self, stride, precision)
        class(base_adios2_t), intent(inout) :: self
        integer, intent(in), optional :: stride    !! Spatial stride for output
        integer, intent(in), optional :: precision !! Output precision

        if (present(stride)) self%output_stride = stride
        if (present(precision)) self%output_precision = precision
    end subroutine setup_io

    !> Begin a step for ADIOS2 writer type
    subroutine begin_step_writer(self)
        class(adios2_writer_t), intent(inout) :: self

        if (self%is_step_active) return

        call adios2_begin_step(self%engine, adios2_step_mode_append, self%ierr)
        call self%handle_error(self%ierr, "Error beginning  ADIOS2 step")
        self%is_step_active = .true.  ! set flag after successful step begin
    end subroutine begin_step_writer

    !> Write scalar real data
    subroutine write_scalar_real(self, name, data, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name  !! unique variable identifier within io
        real(kind=real64), intent(in) :: data !! scalar real data
        integer(kind=int64), dimension(:), optional, intent(in) :: shape_dims, &
                                                          start_dims, count_dims
        type(adios2_variable) :: var          !! handler to newly defined variable

        ! define adios2 variable to be written in given file format
        call adios2_define_variable(var, self%io, name, &
                                    adios2_type_dp, self%ierr)
        call self%handle_error(self%ierr, "Error defining ADIOS2 scalar single precision real variable")

        call adios2_put(self%engine, var, data, adios2_mode_deferred, self%ierr)
        call self%handle_error(self%ierr, "Error writing ADIOS2 scalar single precision real data")
    end subroutine write_scalar_real

    !> Write 2d array real data
    subroutine write_array_2d_real(self, name, data, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:), intent(in) :: data
        integer(kind=int64), dimension(ndims_2d), intent(in) :: shape_dims, &
                                                       start_dims, count_dims
        type(adios2_variable) :: var

        print*, "writing ADIOS2 2d array"
        call adios2_define_variable(var, self%io, name, adios2_type_dp, &
                                    ndims_2d, shape_dims, start_dims, &
                                    count_dims, adios2_constant_dims, self%ierr)
        call self%handle_error(self%ierr, "Error defining ADIOS2 2D array single precision real variable")

        call adios2_put(self%engine, var, data, adios2_mode_deferred, self%ierr)
        call self%handle_error(self%ierr, "Error writing ADIOS2 2D array single precision real data")
    end subroutine write_array_2d_real

    !> Write 3d array real data
    subroutine write_array_3d_real(self, name, data, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:,:), intent(in) :: data
        integer(kind=int64), dimension(ndims_3d), intent(in) :: shape_dims, &
                                                       start_dims, count_dims
        type(adios2_variable) :: var

        print*, "writing ADIOS2 3d array"
        call adios2_define_variable(var, self%io, name, adios2_type_dp, &
                                    ndims_3d, shape_dims, start_dims, &
                                    count_dims, adios2_constant_dims, self%ierr)
        call self%handle_error(self%ierr, "Error defining ADIOS2 3D array single precision real variable")

        call adios2_put(self%engine, var, data, adios2_mode_deferred, self%ierr)
        call self%handle_error(self%ierr, "Error writing ADIOS2 3D array single precision real data")
    end subroutine write_array_3d_real

    !> Write field data
    subroutine write_fields(self, timestep, field_data, names, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        integer, intent(in) :: timestep
        real(kind=real64), dimension(:,:,:,:), intent(in) :: field_data !! Array of fields to write
        character(len=*), intent(in) :: names(:)           !! Corresponding names for each field
        integer(kind=int64), dimension(ndims_3d), intent(in) :: shape_dims, &
                                                       start_dims, count_dims
        character(len=256) :: filename
        integer :: i

        ! create filename with timestep
        write(filename, '(A,I6.6,".bp")') "output_", timestep
        call self%open(filename, adios2_mode_write)
        call self%begin_step()

        do i = 1, size(names)
            call self%write_array_3d_real(names(i), field_data(i,:,:,:), &
                                         shape_dims, start_dims, count_dims)
        end do

        call self%end_step()
        call self%close()
    end subroutine write_fields

    !> Begin a step for ADIOS2 reader type
    subroutine begin_step_reader(self)
        class(adios2_reader_t), intent(inout) :: self

        if (self%is_step_active) return

        call adios2_begin_step(self%engine, adios2_step_mode_read, self%ierr)
        call self%handle_error(self%ierr, "Error beginning  ADIOS2 step")
        self%is_step_active = .true.  ! set flag after successful step begin
    end subroutine begin_step_reader

    !> Read scalar real data
    subroutine read_scalar_real(self, name, data)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), intent(out) :: data
        type(adios2_variable) :: var

        ! retrieve a variable hanlder within current io handler
        call adios2_inquire_variable(var, self%io, name, self%ierr)
        call self%handle_error(self%ierr, "Failed to inquire variable"//name)

        if (self%ierr == adios2_found) then
            call adios2_get(self%engine, var, data, adios2_mode_sync, self%ierr)
            call self%handle_error(self%ierr, "Failed to read variable"//name)
        end if
    end subroutine read_scalar_real

    !> Read 2d array real data
    subroutine read_array2d_real(self, name, data, start_dims, count_dims)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:), allocatable, intent(out) :: data
        integer(kind=int64), dimension(ndims_2d), intent(in) :: start_dims, count_dims
        type(adios2_variable) :: var

        call adios2_inquire_variable(var, self%io, name, self%ierr)
        call self%handle_error(self%ierr, "Failed to inquire variable"//name)

        if (self%ierr == adios2_found) then
            if (.not. allocated(data)) allocate(data(count_dims(1), count_dims(2)))

            call adios2_set_selection(var, ndims_2d, start_dims, count_dims, self%ierr)
            call self%handle_error(self%ierr, "Failed to set selection for variable"//name)

            call adios2_get(self%engine, var, data, adios2_mode_sync, self%ierr)
            call self%handle_error(self%ierr, "Failed to read variable"//name)
        end if
    end subroutine read_array2d_real

    !> Read 3d array real data
    subroutine read_array3d_real(self, name, data, start_dims, count_dims)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:,:), allocatable, intent(out) :: data
        integer(kind=int64), dimension(ndims_3d), intent(in) :: start_dims, count_dims
        type(adios2_variable) :: var

        call adios2_inquire_variable(var, self%io, name, self%ierr)
        call self%handle_error(self%ierr, "Failed to inquire variable"//name)

        if (self%ierr == adios2_found) then
            if (.not. allocated(data)) allocate(data(count_dims(1), count_dims(2), count_dims(3)))

            call adios2_set_selection(var, ndims_3d, start_dims, count_dims, self%ierr)
            call self%handle_error(self%ierr, "Failed to set selection for variable"//name)

            call adios2_get(self%engine, var, data, adios2_mode_sync, self%ierr)
            call self%handle_error(self%ierr, "Failed to read variable"//name)
        end if

    end subroutine read_array3d_real

end module m_adios2_io
