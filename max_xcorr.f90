subroutine max_xcorr(xcorr, n_xcorr, nsize, imax)
use prec, only: dp
implicit none
!
! This subroutine calculates the maximum cross correlation
! as an index and the resultant energy shift
!
integer, intent(in) :: n_xcorr       ! number of elements in x_corr array NB from 1 ... n_xcorr
integer, intent(in) :: nsize         ! number of elements in DOS vectors 
integer, intent(inout) :: imax   ! shift 

double precision, intent(in) :: xcorr(n_xcorr)  ! cross correlation NB index all +ve

integer :: pos_max, num_match                   ! position of the maximum xcorr in terms of index >0
integer, dimension(1) :: itemp                  ! 1 dimensional vector to take output of maxloc

!double precision :: diff                        ! difference
double precision :: xcorrmax                    ! max xcorr dos


! calculate position of max xcorr
itemp = maxloc(xcorr)
pos_max = itemp(1)
xcorrmax = xcorr(pos_max)
write(*,*) xcorrmax
write(*,*) xcorr(1) - xcorrmax
write(*,*) epsilon(xcorrmax)
! check only one maximum
num_match = count (abs(xcorr-xcorrmax) < epsilon(xcorrmax))
write(*,*) "number of matches is : ", num_match
    
if (num_match .eq. 1) then
    imax = pos_max - nsize            ! in order to get the shift in index subtract nsize
!
else    ! if multiple maxima make code not work (need to inspect manually)
    write(*,*) 'number of matches : ', num_match, 'xcorr value : ', xcorr(pos_max)
    imax = -9999999
end if

return
end subroutine max_xcorr
