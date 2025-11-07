module coreutils
  !! This is the top-level module for the coreutils Fortran library.
  use coreutils__kind, only: real32

  use coreutils__const, only: pi, tau, c, INF, imag, k_b, k_b_ev, hbar, hbar_ev, h, &
       atomic_mass, neutron_mass, electron_mass, elem_charge, avogadros, bohrtoang

  use coreutils__file, only: file_check, grep, jump, touch

  use coreutils__string, only: to_lower, to_upper, icount, flagmaker, strip_null

  use coreutils__array, only: set, swap, shuffle, sort1D, sort2D, sort_str, &
       sort_str_order

  use coreutils__linalg, only: cross, inverse_3x3

  use coreutils__geom, only: get_distance, get_angle, get_dihedral_angle, &
       get_improper_dihedral_angle

  use coreutils__error, only: stop_program, print_warning, test_error_handling, &
       suppress_warnings

  implicit none

  public

end module coreutils
