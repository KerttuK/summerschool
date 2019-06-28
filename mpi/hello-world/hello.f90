program hello
  use mpi
  implicit none


  integer :: rc
  
  call mpi_init(rc)

  write(*,*) 'Hello world!'


  call mpi_finalize(rc)



end program hello
