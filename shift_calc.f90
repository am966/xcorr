subroutine shift_calc(en_m, en_p, msize, psize, imax, samplm, samplp, enshift)
use prec, only: dp
implicit none
!
! This subroutine calculates the maximum cross correlation
! as an index and the resultant energy shift
!
integer, intent(in) :: msize, psize
integer, intent(in) :: imax
double precision, intent(in) :: en_m(msize), en_p(psize)
double precision, intent(in)  :: samplm, samplp     ! energy sampling in eV
double precision, intent(inout) :: enshift

! local
integer :: n_small, num_match

double precision, dimension(:), allocatable :: en_diff, diff, absdiff
double precision :: sampl, en_offset
double precision :: eps   ! allowed difference in energy sampling between p and m vectors

n_small = min(msize, psize) 
allocate(en_diff(n_small))
allocate(diff(n_small))
allocate(absdiff(n_small))

! check sampling freq same for + and - charge
if (abs(samplm - samplp) < epsilon(samplm)) then
    sampl = samplm
    en_offset = en_p(1) - en_m(1)

    ! check that the difference between p and m vectors is always en_offset
    eps = sampl * 0.9_dp

    en_diff = en_p(1:n_small) - en_m(1:n_small) 
    diff = en_diff - en_offset
    absdiff = abs(diff)

    num_match = count(absdiff > eps)

    if(num_match == 0) then
        write(*,*) 'shift calculated okay'
        enshift = en_offset - (sampl * imax)
    else
        enshift = -9999999
        write(*,*) 'WARNING!!! sampling frequency incorrect for', num_match, 'elements'
    end if

!
else
    write(*,*) 'WARNING!!! sampling frequency differs with charge '
    write(*,*) 'sampling freq - charge : ', samplm, 'sampling freq + charge: ', samplp
end if

deallocate(en_diff, diff, absdiff)
return
end subroutine shift_calc
