module m_adios2_reader
    use adios2
    use m_base_adios2
    use iso_fortran_env, only: real32, real64, int32, int64
    implicit none

    private
    public :: adios2_reader_t

    !> @brief ADIOS2 reader type
    !> @details Extends `base_adios2_t` and implements reading functionality
    type, extends(base_adios2_t) :: adios2_reader_t
    contains
        procedure, public :: init => init_reader    !! Initialises ADIOS2 reader
        procedure, public :: open => open_reader    !! Opens ADIOS2 engine
        procedure, public :: close => close_reader  !! Closes ADIOS2 reader
        procedure, public :: begin_step => begin_step_reader  !! Begins a step in the ADIOS2 engine

        ! generic interface for write data
        generic, public :: read_data => read_scalar_real32, read_array2d_real32

        ! specific interfaces for writing data
        procedure, private :: read_scalar_real32    !! Reads scalar single precision real data
        procedure, private :: read_array2d_real32  !! Reads 2d array single precision real data
    end type adios2_reader_t

contains

    subroutine init_reader(self, comm, io_name)
        class(adios2_reader_t), intent(inout) :: self
        integer, intent(in) :: comm
        character(len=*), intent(in) :: io_name
        integer :: ierr

        print*, "Initialising ADIOS2 reader for ", io_name

        call adios2_init(self%adios, comm, ierr)
        call self%handle_error(ierr, "Error initialising ADIOS2 reader")

        call adios2_declare_io(self%io, self%adios, io_name, ierr)
        call self%handle_error(ierr, "Error declaring ADIOS2 IO object")

        call adios2_open(self%engine, self%io, "output.bp", adios2_mode_read, ierr)
        call self%handle_error(ierr, "Error opening ADIOS2 reader engine")
    end subroutine init_reader

    subroutine begin_step_reader(self)
        class(adios2_reader_t), intent(inout) :: self
        integer :: ierr

        if (self%is_step_active) return

        call adios2_begin_step(self%engine, adios2_step_mode_read, ierr)
        call self%handle_error(ierr, "Error beginning  ADIOS2 step")
        self%is_step_active = .true.  ! set flag after successful step begin
    end subroutine begin_step_reader

    subroutine open_reader(self, filename, mode, comm)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: filename   !! Unique engine identifier within io
        integer, intent(in) :: mode                !! Opening mode (write, append, read)
        integer, intent(in), optional :: comm      !! MPI communicator (optional)
        integer :: ierr

        if (present(comm)) then
            call adios2_open(self%engine, self%io, filename, mode, comm, ierr)
        else
            call adios2_open(self%engine, self%io, filename, mode, ierr)
        end if
        call self%handle_error(ierr, "Error opening ADIOS2 reader engine")

        call adios2_begin_step(self%engine, ierr)
        call self%handle_error(ierr, "Error beginning  ADIOS2 step")
    end subroutine open_reader

    subroutine read_scalar_real32(self, name, data)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(real32), intent(out) :: data
        type(adios2_variable) :: var
        integer :: ierr

        ! retrieve a variable hanlder within current io handler
        call adios2_inquire_variable(var, self%io, name, ierr)
        call self%handle_error(ierr, "Failed to inquire variable"//name)

        if (ierr == adios2_found) then
            call adios2_get(self%engine, var, data, adios2_mode_sync, ierr)
            call self%handle_error(ierr, "Failed to read variable"//name)
        end if

        print*, "Reading data: ", var
    end subroutine read_scalar_real32

    subroutine read_array2d_real32(self, name, data, start_dims, count_dims)
        class(adios2_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(real32), dimension(:,:), allocatable, intent(out) :: data
        integer(int64), dimension(ndims_2d), intent(in) :: start_dims, count_dims
        type(adios2_variable) :: var
        integer :: ierr

        ! retrieve a variable hanlder within current io handler
        call adios2_inquire_variable(var, self%io, name, ierr)
        call self%handle_error(ierr, "Failed to inquire variable"//name)

        if (ierr == adios2_found) then
            ! allocate data array
            if (.not. allocated(data)) allocate(data(count_dims(1), count_dims(2)))

            ! set selection for reading portion of data
            call adios2_set_selection(var, ndims_2d, start_dims, count_dims, ierr)
            call self%handle_error(ierr, "Failed to set selection for variable"//name)

            ! read data
            call adios2_get(self%engine, var, data, adios2_mode_sync, ierr)
            call self%handle_error(ierr, "Failed to read variable"//name)
        end if
    end subroutine read_array2d_real32

    subroutine close_reader(self)
        class(adios2_reader_t), intent(inout) :: self
        integer :: ierr

        call adios2_end_step(self%engine, ierr)

        print*, "Closing ADIOS2 reader"
        call adios2_close(self%engine, ierr)
        call self%handle_error(ierr, "Error closing ADIOS2 reader engine")

        call adios2_finalize(self%adios, ierr)
        call self%handle_error(ierr, "Error finalising ADIOS2 reader")
    end subroutine close_reader

end module m_adios2_reader
