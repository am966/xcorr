subroutine write_xcorr(seedname, xcorr, n_xcorr, nsize)
!
use prec, only: dp
implicit none
!
! written by Amy Miller 1 March 2016
! This subprogram writes the cross correlation as a function of shift index
! to a file
!

!...! variables with various intent !
character(len=200),intent(in):: seedname

integer, intent(in) :: n_xcorr, nsize

double precision,intent(in) :: xcorr(n_xcorr)
!double precision,intent(in) :: dos_sampl                 ! sampling in eV

!...! local variables !
character(len=200) :: output_file

integer :: i ! index to loop over
integer :: idxshift ! index of lag value
integer :: ishift ! lag value

! define the name of the output file 
output_file = './'//trim(seedname)//'.xcorr'
!write(*,*) "output file", trim(output_file)

! open output file
open(14, file=trim(output_file))
!write(14,*) "index shifted by", "Energy shift", "Cross Correlation"  ! header
write(14,*) "index shifted by", "Cross Correlation"  ! header

! write xcorr to file for each value of the shift
do idxshift = 1, n_xcorr
    ishift = idxshift - nsize
    write(14,*) ishift, xcorr(idxshift)
end do

! close file and end cleanly
close(14)
return
end subroutine write_xcorr
