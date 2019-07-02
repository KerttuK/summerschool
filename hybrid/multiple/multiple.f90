program multiple
  use mpi
  use omp_lib
  implicit none

  integer :: rc, task_id, thread_id, provided
  integer :: i, my_id, ntasks, thread_tag, msg

  call mpi_init_thread(mpi_thread_multiple, provided, rc)

  call mpi_comm_rank(mpi_comm_world, my_id, rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)

  if ( ntasks < 2 ) then
     write (*, *) 'Call this program using at least 2 tasks'
     call mpi_abort(mpi_comm_world, 5,  rc)
  end if

  !$omp parallel default(shared) private(thread_id, thread_tag, i, msg)
  thread_id = omp_get_thread_num()
  thread_tag = 2**10 + thread_id

  !rank 0 sends thread id to corresponding threads in other tasks
  if (my_id == 0) then

     do i = 1, ntasks-1
        call mpi_send(thread_id, 1, mpi_integer, i, &
             thread_tag, mpi_comm_world, rc)
     end do

  !other ranks receive the thread ids
  else
     call mpi_recv(msg, 1, mpi_integer, 0, thread_tag, &
          mpi_comm_world, mpi_status_ignore, rc)
     write(*, '(A,I3,A,I3,A,I3)') 'Rank: ', my_id,&
          ' thread: ', thread_id, ' received: ', msg
  end if

  !$omp end parallel

  call mpi_finalize(rc)

end program multiple
