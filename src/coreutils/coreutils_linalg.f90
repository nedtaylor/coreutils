module coreutils__linalg
  !! Module contains various linear algebra procedures.
  use coreutils__kind, only: real32
  use coreutils__const, only: pi
  implicit none


  private

  public :: cross
  public :: outer_product
  public :: inverse_3x3



contains

!###############################################################################
  pure function cross(a,b)
    !! Return the cross product of two vectors.
    implicit none

    ! Arguments
    real(real32), dimension(3), intent(in) :: a, b
    !! Input vectors.
    real(real32), dimension(3) :: cross
    !! Output cross product.

    cross(1) = a(2) * b(3) - a(3) * b(2)
    cross(2) = a(3) * b(1) - a(1) * b(3)
    cross(3) = a(1) * b(2) - a(2) * b(1)

  end function cross
!###############################################################################


!###############################################################################
  pure function outer_product(a,b) result(c)
    !! Compute the outer product of two vectors
    implicit none

    ! Arguments
    real(real32), dimension(:), intent(in) :: a,b
    !! Input vectors
    real(real32), dimension(size(a),size(b)) :: c
    !! Outer product of the two vectors

    ! Local variables
    integer :: i,j
    !! Loop indices

    do concurrent(i = 1:size(a), j = 1:size(b))
       c(i,j) = a(i) * b(j)
    end do

  end function outer_product
!###############################################################################


!###############################################################################
  pure function inverse_3x3(mat) result(output)
    implicit none
    real(real32) :: det
    real(real32), dimension(3,3) :: output
    real(real32), dimension(3,3), intent(in) :: mat

    det = &
         mat(1,1) * mat(2,2) * mat(3,3) - mat(1,1) * mat(2,3) * mat(3,2) - &
         mat(1,2) * mat(2,1) * mat(3,3) + mat(1,2) * mat(2,3) * mat(3,1) + &
         mat(1,3) * mat(2,1) * mat(3,2) - mat(1,3) * mat(2,2) * mat(3,1)

    output(1,1) = +1._real32 / det * (mat(2,2) * mat(3,3) - mat(2,3) * mat(3,2))
    output(2,1) = -1._real32 / det * (mat(2,1) * mat(3,3) - mat(2,3) * mat(3,1))
    output(3,1) = +1._real32 / det * (mat(2,1) * mat(3,2) - mat(2,2) * mat(3,1))
    output(1,2) = -1._real32 / det * (mat(1,2) * mat(3,3) - mat(1,3) * mat(3,2))
    output(2,2) = +1._real32 / det * (mat(1,1) * mat(3,3) - mat(1,3) * mat(3,1))
    output(3,2) = -1._real32 / det * (mat(1,1) * mat(3,2) - mat(1,2) * mat(3,1))
    output(1,3) = +1._real32 / det * (mat(1,2) * mat(2,3) - mat(1,3) * mat(2,2))
    output(2,3) = -1._real32 / det * (mat(1,1) * mat(2,3) - mat(1,3) * mat(2,1))
    output(3,3) = +1._real32 / det * (mat(1,1) * mat(2,2) - mat(1,2) * mat(2,1))

  end function inverse_3x3
!###############################################################################

end module coreutils__linalg
