program multiple
  use mpi
  use omp_lib
  implicit none

  integer :: rc, task_id, thread_id, provided, my_id, ntasks, thread_tag, msg

  call mpi_init_thread(mpi_thread_multiple, provided rc)

  call mpi_comm_rank(mpi_comm_world, my_id, rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc

  !$omp parallel private(thread_id, th_id_tag, rc)
  thread_id = omp_get_thread_num()
  thread_tag = 2**10 + thread_id

  !$omp task single

  if (thread_id == 0) then

     do i = 0, ntasks-1
        call mpi_send(thread_id, 1, mpi_integer, i, &
             thread_tag, mpi_comm_world, rc)
     end do
     
  else

     call mpi_recv(msg, 1, mpi_integer, 0, thread_tag, &
          mpi_comm_world, mpi_status_ignore

     write
  end if

  !$omp end task

  !$omp end parallel

  call mpi_finalize(rc)

end program multiple
