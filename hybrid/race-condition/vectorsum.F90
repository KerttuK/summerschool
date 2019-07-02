program vectorsum
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,*) 'Arithmetic sum formula (exact):                  ', sumex

 
  ! TODO: Parallelize the computation
   sum = 0
  !$omp parallel do default(shared) private(i)
  do i = 1, nx
     sum = sum + vecA(i)
  end do
  !$omp end parallel do
   write(*,*) 'Sum with data race: ', sum
  
  sum = 0
  !Compute sum using reduction
  !$omp parallel do default(shared) private(i) reduction(+:sum)
  do i = 1, nx
     sum = sum + vecA(i)
  end do
  !$omp end parallel do
  write(*,*) 'Sum using reduction: ', sum

  sum = 0
  !Compute sum using reduction
  !$omp parallel default(shared) private(i, psum)
  psum = 0
  !$omp do
  do i = 1, nx
     psum = psum + vecA(i)
  end do
  !$omp end do
  !$omp critical
  sum = sum + psum
  !$omp end critical
  !$omp end parallel
  write(*,*) 'Sum using critical section: ', sum

  
end program vectorsum
