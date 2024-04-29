subroutine calc_surfch(charge, a1, a2, area, surfch)
use prec, only: dp
implicit none
!
! written by Amy Miller on 20 Jul 2016
! this subprogram calculates the surface charge and area
! NB must have vectors in Angstroms. Both surfaces count for area.
! ASSUMES DOUBLE SIDED SLAB
!
double precision, intent(in):: charge
double precision, intent(in):: a1(3), a2(3)

double precision, intent(inout) :: area, surfch

! local variables
double precision :: cross_prod  ! NB only true if both vectors have no _k_ component

! check if vectors are perpendicular to c
if((a1(3) <= epsilon(a1(3))) .and. (a2(3) <= epsilon(a2(3)))) then
    ! calculate the magnitude of the cross product
    cross_prod = (a1(1) * a2(2)) - (a1(2) * a2(1))
    area = 2 * cross_prod
    surfch = charge / dble(area)
else 
    ! write warning
    write(*,*) "WARNING!!! v1 and v2 are not perpendicular to surface"
end if

return 
end subroutine calc_surfch
