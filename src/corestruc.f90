module corestruc
  !! This is the top-level module for the corestruc Fortran library.
  use corestruc__kinds, only: real32
  use corestruc__io_utils, only: stop_program, print_warning, test_error_handling
  implicit none

  private

  public :: real32
  public :: stop_program, print_warning, test_error_handling

end module corestruc
