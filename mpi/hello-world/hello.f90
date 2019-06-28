program hello
  use mpi_f08
  implicit none


  call mpi_init

  write(*,*) 'Hello world!'


  call mpi_finalize



end program hello
