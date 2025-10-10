module coreutils__geom
  !! Module contains various geometry relationship procedures.
  use coreutils__kind, only: real32
  use coreutils__const, only: pi
  use coreutils__linalg, only: cross
  implicit none


  private

  public :: get_distance, get_angle, get_dihedral_angle
  public :: get_improper_dihedral_angle


  interface get_angle
     procedure get_angle_from_points, get_angle_from_vectors
  end interface get_angle

  interface get_dihedral_angle
     procedure get_dihedral_angle_from_points, get_dihedral_angle_from_vectors
  end interface get_dihedral_angle

  interface get_improper_dihedral_angle
     procedure get_improper_dihedral_angle_from_points, &
          get_improper_dihedral_angle_from_vectors
  end interface get_improper_dihedral_angle

contains

!###############################################################################
  pure function get_distance(point1, point2) result(distance)
    !! Return the distance between two points.
    implicit none

    ! Arguments
    real(real32), dimension(3), intent(in) :: point1, point2
    !! Input points.
    real(real32) :: distance
    !! Output distance.

    distance = norm2( point1 - point2 )

    return
  end function get_distance
!###############################################################################


!###############################################################################
  pure function get_angle_from_vectors(vector1, vector2) result(angle)
    !! Return the angle between two vectors.
    implicit none

    ! Arguments
    real(real32), dimension(3), intent(in) :: vector1, vector2
    !! Input vectors.
    real(real32) :: angle
    !! Output angle.

    angle =  dot_product(vector1,vector2) / &
         ( norm2(vector1) * norm2(vector2) )
    if(angle .ge. 1._real32)then
       angle = 0._real32
    elseif(angle .le. -1._real32)then
       angle = pi
    else
       angle = acos(angle)
    end if
  end function get_angle_from_vectors
!###############################################################################


!###############################################################################
  pure function get_angle_from_points(point1, point2, point3) result(angle)
    !! Return the angle formed by three points.
    !!
    !! The angle is formed by the path point1 -> point2 -> point3.
    implicit none

    ! Arguments
    real(real32), dimension(3), intent(in) :: point1, point2, point3
    !! Input points.
    real(real32) :: angle
    !! Output angle.

    angle = dot_product( point1 - point2, point3 - point2 ) / &
         ( norm2( point1 - point2 ) * norm2( point3 - point2 ) )
    if(angle .ge. 1._real32)then
       angle = 0._real32
    elseif(angle .le. -1._real32)then
       angle = pi
    else
       angle = acos(angle)
    end if
  end function get_angle_from_points
!###############################################################################


!###############################################################################
  pure function get_dihedral_angle_from_vectors( &
       vector1, vector2, vector3) result(angle)
    !! Return the dihedral angle between two planes.
    !!
    !! The dihedral angle is the angle between the plane defined by the cross
    !! product of two vectors and a third vector.
    !! i.e. ( vector1 x vector2 ) . vector3
    implicit none

    ! Arguments
    real(real32), dimension(3), intent(in) :: vector1, vector2, vector3
    !! Input vectors.
    real(real32) :: angle
    !! Output angle.

    angle = get_angle(cross(vector1, vector2), vector3)

  end function get_dihedral_angle_from_vectors
!###############################################################################


!###############################################################################
  pure function get_dihedral_angle_from_points( &
       point1, point2, point3, point4 &
  ) result(angle)
    !! Return the dihedral angle between two planes.
    !!
    !! The dihedral angle is the angle between the plane defined by four points.
    !! i.e. ( point2 - point1 ) x ( point3 - point2 ) . ( point4 - point2 )
    !! alt. angle between plane point1point2point3 and vector point2point4
    implicit none
    real(real32), dimension(3), intent(in) :: point1, point2, point3, point4
    real(real32) :: angle

    angle = get_angle(cross(point2 - point1, point3 - point2), point4 - point2)

  end function get_dihedral_angle_from_points
!###############################################################################


!###############################################################################
  pure function get_improper_dihedral_angle_from_vectors( &
       vector1, vector2, vector3 &
  ) result(angle)
    !! Return the improper dihedral angle between two planes.
    !!
    !! The improper dihedral angle is the angle between two planes made by
    !! three vectors.
    !! i.e. ( vector1 x vector2 ) . ( vector2 x vector3 )
    !! alt. angle between plane vector1vector2 and vector2vector3
    implicit none
    real(real32), dimension(3), intent(in) :: vector1, vector2, vector3
    real(real32) :: angle

    angle = get_angle( &
         cross(vector1, vector2), &
         cross(vector2, vector3) &
    )
    !! map angle back into the range [0, pi]
    if(angle .gt. pi) angle = 2._real32 * pi - angle


  end function get_improper_dihedral_angle_from_vectors
!###############################################################################


!###############################################################################
  pure function get_improper_dihedral_angle_from_points( &
       point1, point2, point3, point4 &
  ) result(angle)
    !! Return the improper dihedral angle between two planes.
    !!
    !! The dihedral angle is the angle between the plane defined by four points.
    !! i.e. ( point2 - point1 ) x ( point3 - point1 ) .
    !! ( point4 - point2 ) x ( point3 - point1 )
    !! alt. angle between plane point1point2point3 and point1point3point4
    implicit none
    real(real32), dimension(3), intent(in) :: point1, point2, point3, point4
    real(real32) :: angle

    angle = get_angle( &
         cross(point2 - point1, point3 - point1), &
         cross(point3 - point1, point4 - point1) &
    )

  end function get_improper_dihedral_angle_from_points
!###############################################################################

end module coreutils__geom
