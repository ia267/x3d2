module m_cuda_allocator
  use m_allocator, only: allocator_t
  use m_common, only: dp
  use m_field, only: field_t
  use m_mesh, only: mesh_t

  implicit none

  type, extends(allocator_t) :: cuda_allocator_t
  contains
    procedure :: create_block => create_cuda_block
  end type cuda_allocator_t

  interface cuda_allocator_t
    module procedure cuda_allocator_init
  end interface cuda_allocator_t

  type, extends(field_t) :: cuda_field_t
    real(dp), device, pointer, private :: p_data_d(:)
    real(dp), device, pointer, contiguous :: data_d(:, :, :)
  contains
    procedure :: init => cuda_field_init
    procedure :: fill => fill_cuda
    procedure :: get_shape => get_shape_cuda
    procedure :: set_shape => set_shape_cuda
  end type cuda_field_t

contains

  subroutine cuda_field_init(self, ngrid, next, id)
    class(cuda_field_t), intent(inout) :: self
    integer, intent(in) :: ngrid, id
    class(field_t), pointer, intent(in) :: next

    allocate (self%p_data_d(ngrid))

    self%refcount = 0
    self%next => next
    self%id = id
  end subroutine cuda_field_init

  subroutine fill_cuda(self, c)
    implicit none
    class(cuda_field_t) :: self
    real(dp), intent(in) :: c

    if (associated(self%p_data_d)) then
      self%p_data_d(:) = c
    else
      error stop "Attempted to fill a field with a null device pointer!"
    end if
  end subroutine fill_cuda

  function get_shape_cuda(self) result(dims)
    implicit none
    class(cuda_field_t) :: self
    integer :: dims(3)

    if (associated(self%data_d)) then
       dims = shape(self%data_d)
    else
       dims = [-1, -1, -1]
    end if
  end function get_shape_cuda

  subroutine set_shape_cuda(self, dims)
    implicit none
    class(cuda_field_t) :: self
    integer, intent(in) :: dims(3)

    if (associated(self%p_data_d)) then
      self%data_d(1:dims(1), 1:dims(2), 1:dims(3)) => self%p_data_d
    else
      error stop "Attempted to set_shape on a field with a null device pointer!"
    end if
  end subroutine set_shape_cuda

  function cuda_allocator_init(dims, sz) result(allocator)
    integer, intent(in) :: dims(3), sz
    type(cuda_allocator_t) :: allocator

    call allocator%init(dims, sz)
  end function cuda_allocator_init

  function create_cuda_block(self, next) result(ptr)
    class(cuda_allocator_t), intent(inout) :: self
    class(field_t), pointer, intent(in) :: next
    type(cuda_field_t), pointer :: newblock
    class(field_t), pointer :: ptr

    allocate (newblock)
    self%next_id = self%next_id + 1
    call newblock%init(self%ngrid, next, id=self%next_id)
    ptr => newblock
  end function create_cuda_block

end module m_cuda_allocator

