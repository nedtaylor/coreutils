module coreutils__kind
  !! This module contains kinds used in coreutils.
  !!
  !! This module enables the loading of portable real kind precision.
  implicit none
  integer, parameter, public :: real32 = Selected_real_kind(6,37)
end module coreutils__kind
