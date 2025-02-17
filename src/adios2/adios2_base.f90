!> @brief Abstract base module for ADIOS2 operations
!> @details Defines the abstract base type `base_adios2_t` for common ADIOS2 components
module m_base_adios2
    use adios2
    use iso_fortran_env, only: real64, int64
    implicit none

    private
    public :: base_adios2_t

    ! make adios2 modes public
    public :: adios2_mode_read, adios2_mode_write, adios2_mode_append
    ! module parameters
    integer, parameter, public :: ndims_2d = 2   !! Number of dimensions for 2D arrays
    integer, parameter, public :: ndims_3d = 3   !! Number of dimensions for 3D arrays

    !> @brief Abstract base type for ADIOS2 operations
    !> @details This type provides common ADIOS2 attributes for reading and writing operations
    type, abstract :: base_adios2_t
        type(adios2_adios) :: adios              !! ADIOS2 global handler
        type(adios2_io) :: io                    !! ADIOS2 IO object for managing I/O
        type(adios2_engine) :: engine            !! ADIOS2 engine for data reading/writing
        logical :: is_step_active = .false.      !! Track if a step is active
        integer :: ierr = 0                      !! Error code for ADIOS2 operations

        !> IO configuration parameters
        integer :: output_stride(3) = [1, 1, 1]  !! Output stride for 3D arrays
        integer :: output_precision = real64     !! Output precision (single/double precision)
    contains
        ! common operations
        procedure, public :: init         !! Initialises ADIOS2
        procedure, public :: open         !! Opens an ADIOS2 engine
        procedure, public :: close        !! Closes the ADIOS2 session
        procedure, public :: end_step     !! Ends a step in the ADIOS2 engine
        procedure, public :: handle_error !! Error handling for ADIOS2 operations
        procedure, public :: setup_io     !! Setup ADIOS2 IO output

        ! deferred implementations
        procedure(begin_step), deferred, public :: begin_step !! Begins a step in the ADIOS2 engine
    end type base_adios2_t

    !> @brief Abstract interface for initialising, closing and error handling
    abstract interface
        !> @brief Begins a step in the ADIOS2 engine
        subroutine begin_step(self)
            import :: base_adios2_t
            class(base_adios2_t), intent(inout) :: self
        end subroutine begin_step
    end interface

contains

    !> @brief Initialises ADIOS2
    !> @param self Instance of `base_adios2_t`
    !> @param comm MPI communicator (use `MPI_COMM_WORLD` for parallel runs)
    !> @param io_name: unique name associated with IO component inside ADIOS2
    subroutine init(self, comm, io_name)
        class(base_adios2_t), intent(inout) :: self
        integer, intent(in) :: comm
        character(len=*), intent(in) :: io_name    !! io that spawns an engine based on its configuration

        ! create adios handler passing the communicator and error flag
        call adios2_init(self%adios, comm, self%ierr)
        call self%handle_error(self%ierr, "Failed to initialise ADIOS2")

        ! declare IO process configuration inside adios
        call adios2_declare_io(self%io, self%adios, io_name, self%ierr)
        call self%handle_error(self%ierr, "Failed to declare ADIOS2 IO object")
    end subroutine init

    !> @brief Opens an ADIOS2 engine
    !> @param self Instance of `base_adios2_t`
    !> @param filename Unique engine identifier within io
    !> @param mode Opening mode (write, append, read)
    !> @param comm PI communicator (optional)
    subroutine open(self, filename, mode, comm)
        class(base_adios2_t), intent(inout) :: self
        character(len=*), intent(in) :: filename   !! Unique engine identifier within io, filename for default BPFile engine
        integer, intent(in) :: mode                !! Opening mode (write, append, read)
        integer, intent(in), optional :: comm      !! MPI communicator (optional)

        if (present(comm)) then
            call adios2_open(self%engine, self%io, filename, mode, comm, self%ierr)
        else
            call adios2_open(self%engine, self%io, filename, mode, self%ierr)
        end if
        call self%handle_error(self%ierr, "Failed to open ADIOS2 engine")
    end subroutine open

    !> @brief Closes the ADIOS2 session
    !> @param self Instance of `base_adios2_t`
    subroutine close(self)
        class(base_adios2_t), intent(inout) :: self

        if (self%is_step_active) call self%end_step()

        call adios2_close(self%engine, self%ierr)
        call self%handle_error(self%ierr, "Failed to close ADIOS2 engine")

        call adios2_finalize(self%adios, self%ierr)
        call self%handle_error(self%ierr, "Failed to finalise ADIOS2")
    end subroutine close

    !> @brief Ends a step in the ADIOS2 engine
    subroutine end_step(self)
        class(base_adios2_t), intent(inout) :: self

        if (.not. self%is_step_active) return
        
        call adios2_end_step(self%engine, self%ierr)
        call self%handle_error(self%ierr, "Failed to end ADIOS2 step")
        self%is_step_active = .false.
    end subroutine end_step

    !> @brief Handles ADIOS2 errors
    !> @param self Instance of `base_adios2_t`
    !> @param ierr Error codea from ADIOS2 operations
    !> @param msg Error messagea to display
    subroutine handle_error(self, ierr, message)
        class(base_adios2_t), intent(inout) :: self
        integer, intent(in) :: ierr
        character(len=*), intent(in) :: message

        self%ierr = ierr
        if (ierr /= 0) then
            print *, "ADIOS2 Error: ", message
            print *, "Error code: ", ierr
            error stop
        end if  
    end subroutine handle_error

    subroutine setup_io(self, stride, precision)
        !! configure I/O settings and ADIOS2 operations
        class(base_adios2_t), intent(inout) :: self
        integer, intent(in), optional :: stride    !! Spatial stride for output
        integer, intent(in), optional :: precision !! Output precision

        if (present(stride)) self%output_stride = stride
        if (present(precision)) self%output_precision = precision
    end subroutine setup_io
end module m_base_adios2
