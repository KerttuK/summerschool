module vector_algebra
  use iso_fortran_env, only : REAL64
  implicit none
  
  type vector_t
     real(REAL64) :: x, y, z
  end type vector_t

  ! TODO: overload operators needed by the parser

  ! ...

contains
  implicit none
  ! TODO: implement the corresponding functions

  function vector_sum(v1, v2) result(v3)
    type(vector_t), intent(in) :: v1, v2
    type(vector_t) :: v3
    v3%x = v1%x + v2%x
    v3%y = v1%y + v2%y
    v3%z = v1%z + v2%z
  end function vector_sum

  function vector_sub(v1, v2) result(v3)
    type(vector_t), intent(in) :: v1, v2
    type(vector_t) :: v3
    v3%x = v1%x - v2%x
    v3%y = v1%y - v2%y
    v3%z = v1%z - v2%z
  end function vector_sub

  function vector_dotproduct(v1, v2) result(v3)
    type(vector_t), intent(in) :: v1, v2
    type(vector_t) :: v3
    v3 = v1%x * v2%x + v1%y * v2%y + v1%z * v2%z
   
  end function vector_dotproduct

  function vector_crossproduct(v1, v2) result(v3)
    type(vector_t), intent(in) :: v1, v2
    type(vector_t) :: v3
    v3 = v1%x * v2%x + v1%y * v2%y + v1%z * v2%z
   
  end function vector_crossproduct

  ! ...

end module vector_algebra
