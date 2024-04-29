subroutine find_pos(v1, n_size, vlim, pos)
use prec, only: dp
implicit none
!
! Subroutine to find the location where a vector reaches a certain value 

integer, intent(in) :: n_size                      ! size of vector to search
integer, intent(inout) :: pos                      ! position of element closest to limit
double precision, intent(in) :: v1(n_size)         ! vector to find limiting value in
double precision, intent(in) :: vlim               ! limit

integer, dimension (1) :: temp                     ! temorary array as minloc returns array
integer :: num_match                               ! number of values which match limit
                                                   
double precision :: vmatch                         ! element of array which is closest to limit
double precision, dimension (n_size) :: diffmod    ! absolute difference between v1 and limit

! vector of differences
diffmod = -999.0   ! initialise vector to negative values
diffmod = abs(v1 - vlim)

! find smallest magnitude of difference
temp = minloc(diffmod)
pos = temp(1)
vmatch = v1(pos)
num_match = count (abs(v1-vmatch) < epsilon(vmatch)) ! find number of matches

! tests 
! check arrays allocated correctly and diffmod all +ve
if (any(diffmod < 0.0_dp)) then
    write(*,*) "WARNING!!! diffmod incorrect should be +ve"

! check only 1 match
else if (num_match > 1) then
    write(*,*) "WARNING!!!  There are ", num_match, "matches within ", epsilon(vmatch), "of ", vmatch
!
! if passed tests continue
!else
    ! do nothing
end if

end subroutine find_pos
