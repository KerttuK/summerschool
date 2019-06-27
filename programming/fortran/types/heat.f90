module heat

  use iso_fortran_env, only :: real64

  type field

     integer, parameter :: rp = real64
     integer, allocatable :: nx, ny
     real(rp), allocatable :: dx, dy
     real(rp), dimension(:,:), allocatable :: A
  
