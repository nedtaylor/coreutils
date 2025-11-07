program test_geom
  use coreutils, only: real32, pi, test_error_handling
  use coreutils__geom
  implicit none

  logical :: success = .true.

  test_error_handling = .true.


  call test_get_distance(success)
  call test_get_angle_from_vectors(success)
  call test_get_angle_from_points(success)
  call test_get_dihedral_angle_from_vectors(success)
  call test_get_dihedral_angle_from_points(success)


  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_geom passed all tests'
  else
     write(0,*) 'test_geom failed one or more tests'
     stop 1
  end if

contains

  subroutine test_get_distance(success)
    logical, intent(inout) :: success
    real(real32), dimension(3) :: point1, point2
    real(real32) :: result
    point1 = [1.0_real32, 2.0_real32, 3.0_real32]
    point2 = [4.0_real32, 6.0_real32, 8.0_real32]
    result = get_distance(point1, point2)
    call assert_almost_equal_scalar( &
         result, 7.0710678118654755_real32, 1.E-6_real32, &
         "get_angle_from_vectors", success &
    )
  end subroutine test_get_distance

  subroutine test_get_angle_from_vectors(success)
    logical, intent(inout) :: success
    real(real32), dimension(3) :: vector1, vector2
    real(real32) :: result
    vector1 = [1.0_real32, 0.0_real32, 0.0_real32]
    vector2 = [0.0_real32, 1.0_real32, 0.0_real32]
    result = get_angle(vector1, vector2)
    call assert_almost_equal_scalar( &
         result, pi/2.0_real32, 1.E-6_real32, &
         "get_angle_from_vectors", success &
    )
  end subroutine test_get_angle_from_vectors

  subroutine test_get_angle_from_points(success)
    logical, intent(inout) :: success
    real(real32), dimension(3) :: point1, point2, point3
    real(real32) :: result
    point1 = [1.0_real32, 0.0_real32, 0.0_real32]
    point2 = [0.0_real32, 0.0_real32, 0.0_real32]
    point3 = [0.0_real32, 1.0_real32, 0.0_real32]
    result = get_angle(point1, point2, point3)
    call assert_almost_equal_scalar( &
         result, pi/2.0_real32, 1.E-6_real32, &
         "get_angle_from_points", success &
    )
  end subroutine test_get_angle_from_points

  subroutine test_get_dihedral_angle_from_vectors(success)
    logical, intent(inout) :: success
    real(real32), dimension(3) :: vector1, vector2, vector3
    real(real32) :: result
    vector1 = [1.0_real32, 0.0_real32, 0.0_real32]
    vector2 = [0.0_real32, 1.0_real32, 0.0_real32]
    vector3 = [1.0_real32, 0.0_real32, 0.0_real32]
    result = get_dihedral_angle(vector1, vector2, vector3)
    call assert_almost_equal_scalar( &
         result, pi/2.0_real32, 1.E-6_real32, &
         "get_dihedral_angle_from_vectors", success &
    )
  end subroutine test_get_dihedral_angle_from_vectors

  subroutine test_get_dihedral_angle_from_points(success)
    logical, intent(inout) :: success
    real(real32), dimension(3) :: point1, point2, point3, point4
    real(real32) :: result
    point1 = [1.0_real32, 0.0_real32, 0.0_real32]
    point2 = [0.0_real32, 0.0_real32, 0.0_real32]
    point3 = [0.0_real32, 1.0_real32, 0.0_real32]
    point4 = [1.0_real32, 0.0_real32, .0_real32]
    result = get_dihedral_angle(point1, point2, point3, point4)
    call assert_almost_equal_scalar( &
         result, pi/2.0_real32, 1.E-6_real32, &
         "get_dihedral_angle_from_points", success &
    )
  end subroutine test_get_dihedral_angle_from_points

  subroutine assert_almost_equal_scalar(actual, expected, tol, message, success)
    real(real32), intent(in) :: actual
    real(real32), intent(in) :: expected
    character(len=*), intent(in) :: message
    logical, intent(inout) :: success
    real(real32), intent(in) :: tol

    if( abs(actual - expected) .gt. tol ) then
       write(0,*) "Test failed: ", message
       success = .false.
    end if
  end subroutine assert_almost_equal_scalar

end program test_geom
