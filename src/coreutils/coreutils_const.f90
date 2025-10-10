module coreutils__const
  !! This module contains parameters used in coreutils.
  !!
  !! This module enables the loading of common mathematical constants.
  use coreutils__kind, only: real32
  implicit none
  real(real32), parameter, public :: pi = 4._real32 * atan(1._real32)
  real(real32), parameter, public :: tau = 8._real32 * atan(1._real32)
  real(real32), parameter, public :: c = 0.26246582250210965422_real32
  real(real32), parameter, public :: INF = huge(0._real32)
  complex(real32), parameter, public :: imag=(0._real32, 1._real32)
end module coreutils__const
