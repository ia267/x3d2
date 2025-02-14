!> @brief Abstract base module for ADIOS2 operations
!> @details Defines the abstract base type `base_adios2_t` for common ADIOS2 components
module m_base_adios2
    use adios2
    implicit none

    private
    public :: base_adios2_t

    ! module parameters
    integer, parameter, public :: ndims_2d = 2   !! Number of dimensions for 2D arrays

    !> @brief Abstract base type for ADIOS2 operations
    !> @details This type provides common ADIOS2 attributes for reading and writing operations
    type, abstract :: base_adios2_t
        type(adios2_adios) :: adios         !! ADIOS2 global handler
        type(adios2_io) :: io               !! ADIOS2 IO object for managing I/O
        type(adios2_engine) :: engine       !! ADIOS2 engine for data reading/writing
        logical :: is_step_active = .false. !! Track if a step is active
    contains
        procedure(init_adios2), deferred, public :: init  !! Initialises ADIOS2
        procedure(open_adios2), deferred, public :: open  !! Opens an ADIOS2 engine
        procedure(begin_step_adios2), deferred, public :: begin_step !! Begins a step in the ADIOS2 engine
        procedure(close_adios2), deferred, public :: close !! Closes the ADIOS2 session
        procedure, public :: end_step       !! Ends a step in the ADIOS2 engine
        procedure, public :: handle_error   !! Error handling for ADIOS2 operations
    end type base_adios2_t

    !> @brief Abstract interface for initialising, closing and error handling
    abstract interface
        !> @brief Initialises ADIOS2
        !> @param self Instance of `base_adios2_t`
        !> @param comm MPI communicator (use `MPI_COMM_WORLD` for parallel runs)
        !> @param io_name: unique name associated with IO component inside ADIOS2
        subroutine init_adios2(self, comm, io_name)
            import :: base_adios2_t
            class(base_adios2_t), intent(inout) :: self
            integer, intent(in) :: comm
            character(len=*), intent(in) :: io_name
        end subroutine init_adios2

        !> @brief Opens an ADIOS2 engine
        !> @param self Instance of `base_adios2_t`
        !> @param filename Unique engine identifier within io
        !> @param mode Opening mode (write, append, read)
        !> @param comm PI communicator (optional)
        subroutine open_adios2(self, filename, mode, comm)
            import :: base_adios2_t
            class(base_adios2_t), intent(inout) :: self
            character(len=*), intent(in) :: filename
            integer, intent(in) :: mode
            integer, intent(in), optional :: comm
        end subroutine open_adios2

        !> @brief Closes the ADIOS2 session
        !> @param self Instance of `base_adios2_t`
        subroutine close_adios2(self)
            import :: base_adios2_t
            class(base_adios2_t), intent(inout) :: self
        end subroutine close_adios2

        !> @brief Begins a step in the ADIOS2 engine
        subroutine begin_step_adios2(self)
            import :: base_adios2_t
            class(base_adios2_t), intent(inout) :: self
        end subroutine begin_step_adios2

        !> @brief Ends a step in the ADIOS2 engine
        subroutine end_step_adios2(self)
            import :: base_adios2_t
            class(base_adios2_t), intent(inout) :: self
        end subroutine end_step_adios2

    end interface

contains

    !> @brief Ends a step in the ADIOS2 engine
    subroutine end_step(self)
        class(base_adios2_t), intent(inout) :: self
        integer :: ierr

        if (.not. self%is_step_active) return
        
        call adios2_end_step(self%engine, ierr)
        call self%handle_error(ierr, "Error ending ADIOS2 step")
        self%is_step_active = .false.
    end subroutine end_step

    !> @brief Handles ADIOS2 errors
    !> @param self Instance of `base_adios2_t`
    !> @param ierr Error codea from ADIOS2 operations
    !> @param msg Error messagea to display
    subroutine handle_error(self, ierr, message)
        class(base_adios2_t), intent(in) :: self
        integer, intent(in) :: ierr
        character(len=*), intent(in) :: message

        if (ierr /= 0) then
            print *, "ADIOS2 Error: ", message
            print *, "Error code: ", ierr
            error stop
        end if  
    end subroutine handle_error

end module m_base_adios2
