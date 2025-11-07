program test_error
  use coreutils__error
  implicit none

  ! Test variables
  logical :: success = .true.
  character(100) :: message

  ! Test stop_program subroutine
  test_error_handling = .true.
  message = "Test error message"
  call stop_program(message)

  ! Test print_warning subroutine
  call print_warning("This is a test warning message")

  !-----------------------------------------------------------------------------
  ! check for any failed tests
  !-----------------------------------------------------------------------------
  write(*,*) "----------------------------------------"
  if(success)then
     write(*,*) 'test_error passed all tests'
  else
     write(0,*) 'test_error failed one or more tests'
     stop 1
  end if

end program test_error
