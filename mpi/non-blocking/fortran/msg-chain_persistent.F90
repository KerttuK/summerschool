program basic
  use mpi
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: msgsize = 10000000
  integer :: rc, myid, ntasks
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)
  integer :: status(mpi_status_size)
  integer :: recv_obj
  integer :: send_obj 
  
  real(REAL64) :: t0, t1

  integer :: source, destination
  integer :: count
  type(mpi_request) :: requests(2)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! Set source and destination ranks
  if (myid < ntasks-1) then
     destination = myid + 1
  else
     destination = MPI_PROC_NULL
  end if
  if (myid > 0) then
     source = myid - 1
  else
     source = MPI_PROC_NULL
  end if

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()

  ! TODO: Implement the message passing using non-blocking
  !       sends and receives

 
  call mpi_send_init(message, msgsize, mpi_integer, destination,&
       myid+1, mpi_comm_world, send_obj, rc)
     
  call mpi_recv_init(receiveBuffer, msgsize, mpi_integer, source, &
       mpi_any_tag, mpi_comm_world, recv_obj, rc)

  call mpi_start(recv_obj, rc)
  call mpi_start(send_obj, rc)

  call mpi_wait(send_obj, mpi_status_ignore, rc)
  call mpi_wait(recv_obj, mpi_status_ignore, rc)

  call mpi_request_free(recv_obj, rc)
  call mpi_request_free(send_obj, rc)

  

  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
       ' Sent elements: ', msgsize, &
       '. Tag: ', myid + 1, '. Receiver: ', destination

  ! TODO: Add here a synchronization call so that you can be sure
  !       that the message has been received

  !call mpi_waitall(2, requests, status, rc)

  call mpi_get_count(status, MPI_INTEGER, count, rc)
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Receiver: ', myid, &
       'received elements: ', count, &
       '. Tag: ', status(mpi_tag), &
       '. Sender:   ', status(MPI_SOURCE)

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)
end program basic
