program test_file
  use coreutils__file
  implicit none

  logical :: success = .true.


  call test_grep(success)
  call test_jump(success)


  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_file passed all tests'
  else
     write(0,*) 'test_file failed one or more tests'
     stop 1
  end if

contains

  subroutine test_grep(success)
    implicit none
    logical, intent(inout) :: success
    integer :: unit
    logical :: success_tmp

    ! Create a temporary scratch file for testing
    open(newunit=unit, status='scratch', action='readwrite')
    write(unit, '(A)') 'This is a test line.'
    write(unit, '(A)') 'Another line with test pattern.'
    write(unit, '(A)') 'Yet another line.'
    rewind(unit)

    ! Test case 1: Pattern found in the middle of the line
    call grep(unit, 'test pattern', success=success_tmp)
    call assert(success_tmp, 'Pattern not found', success)
    rewind(unit)

    ! Test case 2: Pattern not found
    call grep(unit, 'nonexistent pattern', lstart=.true., success=success_tmp)
    call assert(.not. success_tmp, 'Nonexistent pattern found', success)
    rewind(unit)

    ! Test case 3: Pattern at the start of the line
    call grep(unit, 'This is', lline=.true., success=success_tmp)
    call assert(success_tmp, 'Pattern at start of line not found', success)
    close(unit, status='delete')

  end subroutine test_grep

  subroutine test_jump(success)
    implicit none
    logical, intent(inout) :: success
    integer :: i, j, unit

    open(newunit=unit, status='scratch', action='readwrite')
    do i = 1, 10
       write(unit, '(I0)') i
    end do

    ! Test case 1: Jump to the end of the file
    do i = 1, 10
       rewind(unit)
       call jump(unit, i)
       backspace(unit)
       read(unit, *) j
       call assert(j .eq. i, 'Jump failed', success)
    end do

  end subroutine test_jump

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

end program test_file
