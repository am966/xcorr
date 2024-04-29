subroutine calc_xcorr(dos_m, dos_p, nsize, xcorr, n_xcorr, imax)
use prec, only: dp
implicit none
!
! written by Amy Miller on 1 March 2016
! this subprogram calculates the cross correlation
! of the DOS as a function of the sampling energy index
!
integer, intent(in) :: nsize, n_xcorr
integer, intent(inout) :: imax                        ! shift of indices for maximum cross correlation

double precision, intent(in):: dos_m(nsize)
double precision, intent(in):: dos_p(nsize)

double precision, intent(inout) :: xcorr(n_xcorr)
double precision, dimension(nsize) :: prod

! local variables
integer :: i  ! index to loop over
integer :: ishift, idxshift
integer :: minshift, maxshift

minshift = 1-nsize
maxshift = nsize-1

write(*,*) "start calc_xcorr subrout..."

write(*,*) "minshift :  ", minshift
write(*,*) "maxshift :  ", maxshift
write(*,*) "nsize    :  ", nsize
write(*,*) "n_xcorr  :  ", n_xcorr

write(*,*) "dos_m(nsize) ", dos_m(nsize)
write(*,*) "dos_p(nsize) ", dos_p(nsize)

do idxshift = 1, n_xcorr
!do ishift = minshift, maxshift
    ishift = idxshift - nsize
    prod(:) = 0.0_dp

    do i = 1, nsize
         if ((i + ishift >= 1) .and. (i + ishift <= nsize)) then
             !write(*,*) "i=", i, "shift=", ishift, "is in range  prod: ", dos_p(i) * dos_m(i + ishift) 
             prod(i) = dos_p(i) * dos_m(i + ishift)

         else 
             prod(i) = 0.0_dp
             !write(*,*) "i=", i, "shift=", ishift, "NOT IN RANGE!!!!!"

         end if
 
    end do
    
    xcorr(idxshift) = sum(prod)
    !write(*,*) "ishift", ishift, "sum", sum(prod)


end do

! call subroutine to find index shifted by for maximum cross correlation
call max_xcorr(xcorr, n_xcorr, nsize, imax)

return 
end subroutine calc_xcorr
