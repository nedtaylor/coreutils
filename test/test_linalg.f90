program test_linalg
  use coreutils, only: real32, pi, test_error_handling
  use coreutils__linalg
  use coreutils__linalg
  implicit none

  logical :: success = .true.

  test_error_handling = .true.


  call test_cross(success)
  call test_inverse_3x3(success)
  call test_get_transposed_index(success)


  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_linalg passed all tests'
  else
     write(0,*) 'test_linalg failed one or more tests'
     stop 1
  end if

contains

  subroutine test_cross(success)
    logical, intent(inout) :: success
    real(real32), dimension(3) :: a, b, result
    a = [1.0_real32, 0.0_real32, 0.0_real32]
    b = [0.0_real32, 1.0_real32, 0.0_real32]
    result = cross(a, b)
    call assert_almost_equal_vector( &
         result, [0.0_real32, 0.0_real32, 1.0_real32], 1.E-6_real32, &
         "cross", success &
    )
  end subroutine test_cross

  subroutine test_inverse_3x3(success)
    logical, intent(inout) :: success
    real(real32), dimension(3,3) :: matrix, result, expected
    matrix = reshape([ &
         4.0_real32, 3.0_real32, 0.0_real32, &
         3.0_real32, 2.0_real32, 1.0_real32, &
         0.0_real32, 1.0_real32, 1.0_real32 &
    ], [3,3])
    expected = reshape([ &
         -1.0_real32, 3.0_real32, -3.0_real32, &
         3.0_real32, -4.0_real32, 4.0_real32, &
         -3.0_real32, 4.0_real32, 1.0_real32 &
    ], [3,3])
    expected = expected / 5.0_real32
    result = inverse_3x3(matrix)
    call assert_almost_equal_matrix( &
         result, expected, 1.E-6_real32, "inverse_3x3", success &
    )
  end subroutine test_inverse_3x3

  subroutine test_get_transposed_index(success)
    logical, intent(inout) :: success
    integer :: rows, cols, i, j, transposed_index, expected_index

    rows = 2
    cols = 3

    do j = 1, cols
       do i = 1, rows
          transposed_index = get_transposed_index([rows, cols], i + (j - 1) * rows)
          expected_index = (i - 1) * cols + j
          if(transposed_index .ne. expected_index) then
             write(0,'("Test failed: get_transposed_index(", I0, ",", I0, ") = ", I0, " but expected ", I0)') i, j, transposed_index, expected_index
             success = .false.
          end if
       end do
    end do

  end subroutine test_get_transposed_index

  subroutine assert_almost_equal_vector(actual, expected, tol, message, success)
    real(real32), dimension(:), intent(in) :: actual
    real(real32), dimension(..), intent(in) :: expected
    character(len=*), intent(in) :: message
    logical, intent(inout) :: success
    real(real32), intent(in) :: tol

    select rank(expected)
    rank(0)
       if( any( abs(actual - expected) .gt. tol ) ) then
          write(0,*) "Test failed: ", message
          success = .false.
       end if
    rank(1)
       if( any( abs(actual - expected) .gt. tol ) ) then
          write(0,*) "Test failed: ", message
          success = .false.
       end if
    end select
  end subroutine assert_almost_equal_vector

  subroutine assert_almost_equal_matrix(actual, expected, tol, message, success)
    real(real32), dimension(:,:), intent(in) :: actual
    real(real32), dimension(..), intent(in) :: expected
    character(len=*), intent(in) :: message
    logical, intent(inout) :: success
    real(real32), intent(in) :: tol

    select rank(expected)
    rank(0)
       if( any( abs(actual - expected) .gt. tol ) ) then
          write(0,*) "Test failed: ", message
          success = .false.
       end if
    rank(2)
       if( any( abs(actual - expected) .gt. tol ) ) then
          write(0,*) "Test failed: ", message
          success = .false.
       end if
    end select
  end subroutine assert_almost_equal_matrix

end program test_linalg
