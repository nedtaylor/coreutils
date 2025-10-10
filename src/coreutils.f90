module coreutils
  !! This is the top-level module for the coreutils Fortran library.
  use coreutils__kinds, only: real32
  use coreutils__io_utils, only: stop_program, print_warning, test_error_handling
  implicit none

  private

  public :: real32
  public :: stop_program, print_warning, test_error_handling

end module coreutils
