program hello
  use omp_lib
  use mpi
  implicit none
  integer :: my_id, tid, rc
  integer :: provided, required=MPI_THREAD_FUNNELED

  ! TODO: Initialize MPI with thread support.

  call mpi_init_thread(required, provided, rc)
  !$omp parallel private(tid)

  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.

  call mpi_comm_rank(mpi_comm_world, my_id, rc)
  tid = omp_get_thread_num()

  ! TODO: Investigate the provided thread support level.

  if (provided == MPI_THREAD_MULTIPLE) then
     write(*,*) 'MPI library supports MPI_THREAD_MULTIPLE'
  else if (provided == MPI_THREAD_SERIALIZED) then
     write(*,*) 'MPI library supports MPI_THREAD_SERIALIZED'
  else if (provided == MPI_THREAD_FUNNELED) then
     write(*,*) 'MPI library supports MPI_THREAD_FUNNELED'
  else
     write(*,*) 'No multithreading support'
  end if
  
  print *, 'Hello world! by thread ', tid, ' in process ', my_id
  !$omp end parallel
  

  
  call MPI_Finalize(rc)
end program hello
