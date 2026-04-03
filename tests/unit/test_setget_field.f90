program test_setget_field

  use mpi

  use m_allocator, only: allocator_t, field_t
  use m_base_backend, only: base_backend_t
  use m_common, only: dp, DIR_C, DIR_X, DIR_Y, DIR_Z, VERT
  use m_backend_runtime, only: backend_runtime_t
  use m_mesh, only: mesh_t

  implicit none

  type(backend_runtime_t), target :: runtime
  class(allocator_t), pointer :: allocator
  class(base_backend_t), pointer :: backend
  type(mesh_t), target :: mesh

  class(field_t), pointer :: fld, fld_c
  real(dp), dimension(:, :, :), allocatable :: arr
  integer, dimension(3) :: shape_c

  integer :: ierr

  call MPI_Init(ierr)

  mesh = mesh_t([16, 32, 48], [1, 1, 1], [1.0_dp, 1.0_dp, 1.0_dp], &
                ["periodic", "periodic"], &
                ["periodic", "periodic"], &
                ["periodic", "periodic"])

  call runtime%init(mesh)
  allocator => runtime%allocator
  backend => runtime%backend

  fld => backend%allocator%get_block(DIR_X, VERT)
  fld_c => backend%allocator%get_block(DIR_C, VERT)
  shape_c = fld_c%get_shape()
  allocate (arr(shape_c(1), shape_c(2), shape_c(3)))
  arr = 1.0_dp
  call backend%set_field_data(fld, arr)

  if (fld%data_loc /= VERT) then
    error stop "Field location was changed by set_field_data"
  end if

  deallocate (arr)
  call backend%allocator%release_block(fld)
  call backend%allocator%release_block(fld_c)

  call MPI_Finalize(ierr)

end program test_setget_field
