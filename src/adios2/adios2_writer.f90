module m_adios2_writer
    use adios2
    use m_base_adios2
    use iso_fortran_env, only: real32, real64, int32, int64
    implicit none

    private
    public :: adios2_writer_t

    !> @brief ADIOS2 writer type
    !> @details Extends `base_adios2_t` and implements writing functionality
    type, extends(base_adios2_t) :: adios2_writer_t
    contains
        procedure, public :: init => init_writer              !! Initialises ADIOS2 writer
        procedure, public :: open => open_writer              !! Opens ADIOS2 engine
        procedure, public :: close => close_writer            !! Closes ADIOS2 writer
        procedure, public :: begin_step => begin_step_writer  !! Begins a step in the ADIOS2 engine

        ! generic interface for write data
        generic, public :: write_data => write_scalar_real32, write_array_2d_real32

        ! specific interfaces for writing data
        procedure, private :: write_scalar_real32    !! Writes scalar single precision real data
        procedure, private :: write_array_2d_real32  !! Writes 2d array single precision real data
    end type adios2_writer_t

contains

    subroutine init_writer(self, comm, io_name)
        class(adios2_writer_t), intent(inout) :: self
        integer, intent(in) :: comm
        character(len=*), intent(in) :: io_name    !! io that spawns an engine based on its configuration
        integer :: ierr

        print*, "Initialising ADIOS2 writer for ", io_name

        ! create adios handler passing the communicator and error flag
        call adios2_init(self%adios, comm, ierr)
        call self%handle_error(ierr, "Failed to initialise ADIOS2 writer")

        ! declare IO process configuration inside adios
        call adios2_declare_io(self%io, self%adios, io_name, ierr)
        call self%handle_error(ierr, "Failed to declare ADIOS2 IO object")

    end subroutine init_writer

    subroutine begin_step_writer(self)
        class(adios2_writer_t), intent(inout) :: self
        integer :: ierr

        if (self%is_step_active) return

        call adios2_begin_step(self%engine, adios2_step_mode_append, ierr)
        call self%handle_error(ierr, "Error beginning  ADIOS2 step")
        self%is_step_active = .true.  ! set flag after successful step begin
    end subroutine begin_step_writer

    subroutine open_writer(self, filename, mode, comm)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: filename   !! Unique engine identifier within io, filename for default BPFile engine
        integer, intent(in) :: mode                !! Opening mode (write, append, read)
        integer, intent(in), optional :: comm      !! MPI communicator (optional)
        integer :: ierr

        if (present(comm)) then
            call adios2_open(self%engine, self%io, filename, mode, comm, ierr)
        else
            call adios2_open(self%engine, self%io, filename, mode, ierr)
        end if  
        call self%handle_error(ierr, "Error opening ADIOS2 writer engine")

        call adios2_begin_step(self%engine, ierr)
        call self%handle_error(ierr, "Error beginning  ADIOS2 step")
    end subroutine open_writer

    ! single precision scalar real data
    subroutine write_scalar_real32(self, name, data, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name  !! unique variable identifier within io
        real(real32), intent(in) :: data
        integer(int32), dimension(:), optional, intent(in) :: shape_dims, start_dims, count_dims
        type(adios2_variable) :: var                   !! hanlder to newly defined variable
        integer :: ierr

        ! define adios2 variable to be written in given file format
        call adios2_define_variable(var, self%io, name, &
                                    adios2_type_real, ierr)
        call self%handle_error(ierr, "Error defining ADIOS2 scalar single precision real variable")

        call adios2_put(self%engine, var, data, adios2_mode_deferred, ierr)
        call self%handle_error(ierr, "Error writing ADIOS2 scalar single precision real data")
    end subroutine write_scalar_real32

    ! single precision 2d-array real data
    subroutine write_array_2d_real32(self, name, data, shape_dims, start_dims, count_dims)
        class(adios2_writer_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        real(real32), dimension(:,:), intent(in) :: data
        integer(int64), dimension(ndims_2d), intent(in) :: shape_dims, start_dims, count_dims
        type(adios2_variable) :: var
        integer :: ierr

        call adios2_define_variable(var, self%io, name, adios2_type_real, ndims_2d, &
                                    shape_dims, start_dims, count_dims, &
                                    adios2_constant_dims, ierr)
        call self%handle_error(ierr, "Error defining ADIOS2 2D array single precision real variable")

        call adios2_put(self%engine, var, data, adios2_mode_deferred, ierr)
        call self%handle_error(ierr, "Error writing ADIOS2 2D array single precision real data")
    end subroutine write_array_2d_real32

    subroutine close_writer(self)
        class(adios2_writer_t), intent(inout) :: self
        integer :: ierr

        call adios2_end_step(self%engine, ierr)

        print*, "Closing ADIOS2 writer"
        call adios2_close(self%engine, ierr)
        call self%handle_error(ierr, "Error closing ADIOS2 writer engine")

        call adios2_finalize(self%adios, ierr)
        call self%handle_error(ierr, "Error finalising ADIOS2 writer")
    end subroutine close_writer

end module m_adios2_writer
