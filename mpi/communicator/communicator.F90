program communicator
  use mpi
  implicit none

  integer, parameter :: n_mpi_tasks = 4
  integer :: ierr, ntasks, rank, color, newcomm
  integer, dimension(2*n_mpi_tasks) :: sendbuf, recvbuf
  integer, dimension(2*n_mpi_tasks**2) :: printbuf
  !type(mpi_comm) :: newcomm

  call mpi_init(ierr)
  call mpi_comm_size(mpi_comm_world, ntasks, ierr)
  call mpi_comm_rank(mpi_comm_world, rank, ierr)
  
  call init_buffers

  if (rank < 2) then
     color = 1
  else
     color = 2
  end if

  call mpi_comm_split(mpi_comm_world, color, rank, newcomm, ierr)

  call mpi_reduce(sendbuf, recvbuf, 2*n_mpi_tasks, mpi_integer, mpi_sum, 0, newcomm, ierr)
  

  call print_buffers(recvbuf)

  call mpi_finalize(ierr)



contains

  subroutine init_buffers
    implicit none

    integer :: i
    !integer, parameter :: n_mpi_tasks
    !integer, dimension(2*n_mpi_tasks) :: sendbuf, recvbuf

    do i=1, 2*n_mpi_tasks
       recvbuf(i) = -1
       sendbuf(i) = i + 2*n_mpi_tasks * rank - 1
    end do
  end subroutine init_buffers

   subroutine print_buffers(buffer)
    implicit none
    integer, dimension(:), intent(in) :: buffer
    integer, parameter :: bufsize = 2*n_mpi_tasks
    integer :: i
    character(len=40) :: pformat

    write(pformat,'(A,I3,A)') '(A4,I2,":",', bufsize, 'I3)'

    call mpi_gather(buffer, bufsize, MPI_INTEGER, &
         & printbuf, bufsize, MPI_INTEGER, &
         & 0, MPI_COMM_WORLD, ierr)

    if (rank == 0) then
       do i = 1, ntasks
          write(*,pformat) 'Task', i - 1, printbuf((i-1)*bufsize+1:i*bufsize)
       end do
       print *
    end if
  end subroutine print_buffers

  

end program communicator
