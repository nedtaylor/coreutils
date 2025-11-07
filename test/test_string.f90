program test_string
  use coreutils__string
  implicit none

  logical :: success = .true.


!-------------------------------------------------------------------------------
! tests
!-------------------------------------------------------------------------------
  call check_strings(to_upper('abc'), 'ABC')
  call check_strings(to_upper('AbC'), 'ABC')
  call check_strings(to_upper('123'), '123')

  call check_strings(to_lower('ABC'), 'abc')
  call check_strings(to_lower('AbC'), 'abc')
  call check_strings(to_lower('123'), '123')

  call test_icount('abc def ghi', ' ', 3)
  call test_icount('abc   def   ghi', ' ', 3)
  call test_icount('abc,def,ghi', ',', 3)
  call test_icount('abc,,,def,,,ghi', ',', 3)
  call test_icount('', ' ', 0)

  call test_to_upper(success)
  call test_to_lower(success)
  call test_strip_null(success)


!-------------------------------------------------------------------------------
! check for any failed tests
!-------------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_string passed all tests'
  else
     write(0,*) 'test_string failed one or more tests'
     stop 1
  end if

contains

!-------------------------------------------------------------------------------
! compare two strings
!-------------------------------------------------------------------------------
  subroutine check_strings(actual, expected)
    character(*), intent(in) :: actual
    character(*), intent(in) :: expected

    if(actual .ne. expected) then
       write(0,*) "Error: Strings are not equal."
       write(0,*) "Actual: ", actual
       write(0,*) "Expected: ", expected
       success = .false.
    end if
  end subroutine check_strings

!-------------------------------------------------------------------------------
! check counter returns correct number of words in string
!-------------------------------------------------------------------------------
  subroutine test_icount(full_line, tmpchar, expected)
    character(*), intent(in) :: full_line
    character(*), intent(in) :: tmpchar
    integer, intent(in) :: expected
    integer :: actual

    actual = Icount(full_line, tmpchar)

    if(actual .ne. expected) then
       write(0,*) "Error: Word counts are not equal."
       write(0,*) "Line: ", full_line
       write(0,*) "Char: ", tmpchar
       write(0,*) "Actual: ", actual
       write(0,*) "Expected: ", expected
       success = .false.
    end if
  end subroutine test_icount

  subroutine test_to_upper(success)
    implicit none
    logical, intent(inout) :: success
    character(len=10) :: str
    character(len=10) :: expected_str

    str = "hello"
    expected_str = "HELLO"
    str = to_upper(str)
    call assert(trim(str) .eq. trim(expected_str), "to_upper failed", success)
  end subroutine test_to_upper

  subroutine test_to_lower(success)
    implicit none
    logical, intent(inout) :: success
    character(len=10) :: str
    character(len=10) :: expected_str

    str = "HELLO"
    expected_str = "hello"
    str = to_lower(str)
    call assert(trim(str) .eq. trim(expected_str), "to_lower failed", success)
  end subroutine test_to_lower

  subroutine test_strip_null(success)
    implicit none
    logical, intent(inout) :: success
    character(len=16) :: str
    character(len=16) :: expected_str

    str = "hello"//char(0)//"world"
    expected_str = "hello"
    str = strip_null(str)
    call assert(trim(str) .eq. trim(expected_str), "strip_null failed", success)
  end subroutine test_strip_null

!###############################################################################

  subroutine assert(condition, message, success)
    implicit none
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    logical, intent(inout) :: success
    if (.not. condition) then
       write(0,*) "Test failed: ", message
       success = .false.
    end if
  end subroutine assert

end program test_string
