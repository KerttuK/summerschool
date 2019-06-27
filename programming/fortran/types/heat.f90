module heat

  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: rp = real64
  
  type field
    
     integer, allocatable :: nx, ny
     real(kind=rp), allocatable :: dx, dy
     real(kind=rp), dimension(:,:), allocatable :: A

  end type field

contains

  subroutine initialize(nx, ny, field0)

    implicit none

    integer :: nx, ny
    type(field) :: field0

    field0%dx = 0.01
    field0%dy = 0.01
    field0%nx = nx
    field0%ny = ny
    


  end subroutine initialize

    




end module
