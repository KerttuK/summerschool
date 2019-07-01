program hello
  use omp_lib
  use mpi
  implicit none
  integer :: my_id, tid, rc, omp_rank
  integer :: provided, required=MPI_THREAD_FUNNELED

  ! TODO: Initialize MPI with thread support.

  call mpi_init(rc)

  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.

  ! TODO: Investigate the provided thread support level.


  
  !$omp parallel private(omp_rank)
  omp_rank = omp_get_thread_num()

  print *, 'Hello world! by thread ', omp_rank
  !$omp end parallel
  

  
  call MPI_Finalize(rc)
end program hello
