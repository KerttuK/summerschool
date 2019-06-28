program communicator
  use mpi
  implicit none

  integer, parameter :: n_mpi_tasks
  integer :: ierr, size, rank
  integer, dimension(2*n_mpi_tasks) :: sendbuf, recvbuf

  call mpi_comm_size(mpi_comm_world, size, ierr)
  call mpi_comm_rank(mpi_comm_world, rank, ierr)

  call mpi_init(ierr)

  call mpi_finalize(ierr)



contains

  subroutine initialize
    implicit none

    do i=1, 2*n_mpi_tasks
       recvbuf(i) = -1
       sendbuf(i) = i + 2*n_mpi_tasks * rank - 1
    end do
  end subroutine initialize
  

end program communicator
