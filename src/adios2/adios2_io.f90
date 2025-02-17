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
    use mpi
    use m_field, only: field_t
    use m_solver, only: solver_t
    use m_common, only: DIR_C
    implicit none

    private
    public :: base_adios2_t, adios2_writer_t, adios2_reader_t, adios2_file_t

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
        integer :: comm = MPI_COMM_NULL          !! MPI communicator

        !> IO configuration parameters
        integer :: output_stride(3) = [1, 1, 1]  !! Output stride for 3D arrays
        integer :: output_precision = real64     !! Output precision (single/double precision)
    contains
        ! common operations
        procedure, public :: init                !! Initialises ADIOS2 handler
        procedure, public :: open                !! Opens an ADIOS2 engine
        procedure, public :: close               !! Closes the ADIOS2 session
        procedure, public :: end_step            !! Ends a step in the ADIOS2 engine
        procedure, public :: handle_error        !! Error handling for ADIOS2 operations
        procedure, public :: setup_io            !! Setup ADIOS2 IO output
        procedure, public :: finalise            !! Finalises ADIOS2 handler

        ! deferred implementations
        procedure(begin_step), deferred, public :: begin_step !! Begins a step in the ADIOS2 engine
    end type base_adios2_t

    !> ADIOS2 writer type
    type, extends(base_adios2_t) :: adios2_writer_t
    contains
        procedure, public :: begin_step => begin_step_writer
        procedure, public :: write_fields

        generic, public :: write_data => write_scalar_real, write_array_2d_real,&
                                        write_array_3d_real

        procedure, private :: write_scalar_real    !! Writes scalar real data
        procedure, private :: write_array_2d_real  !! Writes 2d array real data
        procedure, private :: write_array_3d_real  !! Writes 3d array real data
    end type adios2_writer_t

    !> ADIOS2 reader type
    type, extends(base_adios2_t) :: adios2_reader_t
    contains
        procedure, public :: begin_step => begin_step_reader

        generic, public :: read_data => read_scalar_real, read_array2d_real, &
                                        read_array3d_real

        procedure, private :: read_scalar_real      !! Reads scalar real data
        procedure, private :: read_array2d_real     !! Reads 2d array real data
        procedure, private :: read_array3d_real     !! Reads 3d array real data
    end type adios2_reader_t

    !> ADIOS2 file type
    type :: adios2_file_t
        type(adios2_engine) :: engine
    end type adios2_file_t

    abstract interface
        !> Begins a step in the ADIOS2 engine
        subroutine begin_step(self, file)
            import :: base_adios2_t, adios2_file_t
            class(base_adios2_t), intent(inout) :: self
            type (adios2_file_t), intent(inout) :: file
        end subroutine begin_step
    end interface
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

        logical :: is_mpi_initialised
        integer :: ierr, comm_rank

        if (comm == MPI_COMM_NULL) call self%handle_error(1, &
                                    "Invalid MPI communicator")
        call MPI_Initialized(is_mpi_initialised, ierr)
        if (.not. is_mpi_initialised) call self%handle_error(1, &
                "MPI must be initialised before calling ADIOS2 init")


        self%comm = comm
        call MPI_Comm_rank(self%comm, comm_rank, ierr)
        call self%handle_error(ierr, "Failed to get MPI rank")
    
        ! create adios handler passing communicator
        call adios2_init(self%adios, comm, ierr)
        call self%handle_error(ierr, "Failed to initialise ADIOS2")

        ! declare IO process configuration inside adios
        call adios2_declare_io(self%io, self%adios, io_name, ierr)
        call self%handle_error(ierr, "Failed to declare ADIOS2 IO object")

        if (.not. self%io%valid) call self%handle_error(1, "Failed to create ADIOS2 IO object")
    end subroutine init

    !> Opens an ADIOS2 engine
    !> filename: Unique engine identifier within io
    !> mode: Opening mode (write, append, read)
    function open(self, filename, mode, comm) result(file)
        class(base_adios2_t), intent(inout) :: self
        character(len=*), intent(in) :: filename   !! Unique engine identifier within io
        integer, intent(in) :: mode                !! Opening mode (write, append, read)
        integer, intent(in), optional :: comm      !! MPI communicator (optional)
        integer :: ierr, use_comm
        type(adios2_file_t) :: file   !! ADIOS2 file object

        use_comm = self%comm
        if (present(comm)) use_comm = comm
        if (.not. self%io%valid) call self%handle_error(1, "ADIOS2 IO object is not valid")

        call adios2_open(file%engine, self%io, filename, mode, use_comm, ierr)
        call self%handle_error(ierr, "Failed to open ADIOS2 engine")
    end function open

    !> Closes ADIOS2 session
    subroutine close(self, file)
        class(base_adios2_t), intent(inout) :: self
        type(adios2_file_t), intent(inout) :: file
        integer :: ierr

        call adios2_remove_all_variables(self%io, ierr)
        call self%handle_error(ierr, "Failed to remove all ADIOS2 variables")

        if (self%is_step_active) call self%end_step(file)

        if (file%engine%valid) then
            call adios2_close(file%engine, ierr)
            call self%handle_error(ierr, "Failed to close ADIOS2 engine")
        end if
    end subroutine close

    !> Ends a step in the ADIOS2 engine
    subroutine end_step(self, file)
        class(base_adios2_t), intent(inout) :: self
        type(adios2_file_t), intent(inout) :: file
        integer :: ierr

        if (.not. self%is_step_active) return
        
        call adios2_end_step(file%engine, ierr)
        call self%handle_error(ierr, "Failed to end ADIOS2 step")
        self%is_step_active = .false.
    end subroutine end_step

    !> Finalises ADIOS2 handler
    subroutine finalise(self)
        class(base_adios2_t), intent(inout) :: self
        integer :: ierr

        if (self%adios%valid) then
            call adios2_finalize(self%adios, ierr)
            call self%handle_error(ierr, "Failed to finalise ADIOS2")
        end if
    end subroutine finalise

    !> Handles ADIOS2 errors
    subroutine handle_error(self, ierr, message)
        class(base_adios2_t), intent(inout) :: self
        integer, intent(in) :: ierr              !! Error code from ADIOS2 operations
        character(len=*), intent(in) :: message  !! Error message to display

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
    subroutine begin_step_writer(self, file)
        class(adios2_writer_t), intent(inout) :: self
        type(adios2_file_t), intent(inout) :: file
        integer :: ierr

        if (self%is_step_active) return

        call adios2_begin_step(file%engine, adios2_step_mode_append, ierr)
        call self%handle_error(ierr, "Error beginning  ADIOS2 step")
        self%is_step_active = .true.
    end subroutine begin_step_writer

    !> Write scalar real data
    subroutine write_scalar_real(self, name, data, file, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name  !! unique variable identifier within io
        real(kind=real64), intent(in) :: data !! scalar real data
        type(adios2_file_t), intent(inout) :: file
        integer(kind=int64), dimension(:), optional, intent(in) :: shape_dims, &
                                                          start_dims, count_dims
        type(adios2_variable) :: var          !! handler to newly defined variable
        integer :: ierr

        ! define adios2 variable to be written in given file format
        call adios2_define_variable(var, self%io, name, &
                                    adios2_type_dp, ierr)
        call self%handle_error(ierr, "Error defining ADIOS2 scalar single precision real variable")

        call adios2_put(file%engine, var, data, adios2_mode_deferred, ierr)
        call self%handle_error(ierr, "Error writing ADIOS2 scalar single precision real data")
    end subroutine write_scalar_real

    !> Write 2d array real data
    subroutine write_array_2d_real(self, name, data, file, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:), intent(in) :: data
        type(adios2_file_t), intent(inout) :: file
        integer(kind=int64), dimension(ndims_2d), intent(in) :: shape_dims, &
                                                       start_dims, count_dims
        type(adios2_variable) :: var
        integer :: ierr

        call adios2_define_variable(var, self%io, name, adios2_type_dp, &
                                    ndims_2d, shape_dims, start_dims, &
                                    count_dims, adios2_constant_dims, ierr)
        call self%handle_error(ierr, "Error defining ADIOS2 2D array single precision real variable")

        call adios2_put(file%engine, var, data, adios2_mode_deferred, ierr)
        call self%handle_error(ierr, "Error writing ADIOS2 2D array single precision real data")
    end subroutine write_array_2d_real

    !> Write 3d array real data
    subroutine write_array_3d_real(self, name, data, file, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:,:), intent(in) :: data
        type(adios2_file_t), intent(inout) :: file
        integer(kind=int64), dimension(ndims_3d), intent(in) :: shape_dims, &
                                                       start_dims, count_dims
        type(adios2_variable) :: var
        integer :: ierr
        call adios2_define_variable(var, self%io, name, adios2_type_dp, &
                                    ndims_3d, shape_dims, start_dims, &
                                    count_dims, adios2_constant_dims, ierr)
        call self%handle_error(ierr, "Error defining ADIOS2 3D array single precision real variable")

        call adios2_put(file%engine, var, data, adios2_mode_deferred, ierr)
        call self%handle_error(ierr, "Error writing ADIOS2 3D array single precision real data")
    end subroutine write_array_3d_real

    !> Write field data
    subroutine write_fields(self, timestep, field_names, solver)
        class(adios2_writer_t), intent(inout) :: self
        integer, intent(in) :: timestep
        character(len=*), intent(in) :: field_names(:)   !! Names of fields to write
        class(solver_t), intent(in) :: solver

        character(len=256) :: filename
        type(adios2_file_t) :: file
        class(field_t), pointer :: host_field, field
        integer :: i, dims(3), data_loc
        integer(kind=int64), dimension(ndims_3d)  :: shape_dims, &   !! Global size of field data
                                                     start_dims, &   !! Local offset of field data
                                                     count_dims      !! Local size of field data
        data_loc = solver%u%data_loc
        dims = solver%mesh%get_dims(data_loc)
        shape_dims = int(solver%mesh%get_global_dims(data_loc), kind=int64)
        start_dims = int(solver%mesh%par%n_offset, kind=int64)
        count_dims = int(dims, kind=int64)

        ! create filename with timestep
        write(filename, '(A,I6.6,".bp")') "output_", timestep

        file = self%open(filename, adios2_mode_write, self%comm)
        call self%begin_step(file)

        do i = 1, size(field_names)
            select case (trim(field_names(i)))
                case ("u")
                    field => solver%u
                case ("v")
                    field => solver%v
                case ("w")
                    field => solver%w
                case default
                    call self%handle_error(1, "Invalid field name"//trim(field_names(i)))
                    cycle
            end select

            host_field => solver%host_allocator%get_block(DIR_C, data_loc)
            call solver%backend%get_field_data(host_field%data, field)
            call self%write_array_3d_real(field_names(i), host_field%data(1:dims(1), &
                                         1:dims(2), &
                                         1:dims(3)), &
                                         file, shape_dims, start_dims, count_dims)
            call solver%host_allocator%release_block(host_field)
        end do

        call self%close(file)
    end subroutine write_fields

    !> Begin a step for ADIOS2 reader type
    subroutine begin_step_reader(self, file)
        class(adios2_reader_t), intent(inout) :: self
        type(adios2_file_t), intent(inout) :: file
        
        integer :: ierr

        if (self%is_step_active) return

        call adios2_begin_step(file%engine, adios2_step_mode_read, ierr)
        call self%handle_error(ierr, "Error beginning  ADIOS2 step")
        self%is_step_active = .true.  ! set flag after successful step begin
    end subroutine begin_step_reader

    !> Read scalar real data
    subroutine read_scalar_real(self, name, data, file)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), intent(out) :: data
        type(adios2_file_t), intent(inout) :: file

        type(adios2_variable) :: var
        integer :: ierr

        ! retrieve a variable hanlder within current io handler
        call adios2_inquire_variable(var, self%io, name, ierr)
        call self%handle_error(ierr, "Failed to inquire variable"//name)

        if (ierr == adios2_found) then
            call adios2_get(file%engine, var, data, adios2_mode_sync, ierr)
            call self%handle_error(ierr, "Failed to read variable"//name)
        end if
    end subroutine read_scalar_real

    !> Read 2d array real data
    subroutine read_array2d_real(self, name, data, file, start_dims, count_dims)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:), allocatable, intent(out) :: data
        type(adios2_file_t), intent(inout) :: file
        integer(kind=int64), dimension(ndims_2d), intent(in) :: start_dims, count_dims

        type(adios2_variable) :: var
        integer :: ierr

        call adios2_inquire_variable(var, self%io, name, ierr)
        call self%handle_error(ierr, "Failed to inquire variable"//name)

        if (ierr == adios2_found) then
            if (.not. allocated(data)) allocate(data(count_dims(1), count_dims(2)))

            call adios2_set_selection(var, ndims_2d, start_dims, count_dims, ierr)
            call self%handle_error(ierr, "Failed to set selection for variable"//name)

            call adios2_get(file%engine, var, data, adios2_mode_sync, ierr)
            call self%handle_error(ierr, "Failed to read variable"//name)
        end if
    end subroutine read_array2d_real

    !> Read 3d array real data
    subroutine read_array3d_real(self, name, data, file, start_dims, count_dims)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(kind=real64), dimension(:,:,:), allocatable, intent(out) :: data
        type(adios2_file_t), intent(inout) :: file
        integer(kind=int64), dimension(ndims_3d), intent(in) :: start_dims, count_dims

        type(adios2_variable) :: var
        integer :: ierr

        call adios2_inquire_variable(var, self%io, name, ierr)
        call self%handle_error(ierr, "Failed to inquire variable"//name)

        if (ierr == adios2_found) then
            if (.not. allocated(data)) allocate(data(count_dims(1), count_dims(2), count_dims(3)))

            call adios2_set_selection(var, ndims_3d, start_dims, count_dims, ierr)
            call self%handle_error(ierr, "Failed to set selection for variable"//name)

            call adios2_get(file%engine, var, data, adios2_mode_sync, ierr)
            call self%handle_error(ierr, "Failed to read variable"//name)
        end if
    end subroutine read_array3d_real

end module m_adios2_io
