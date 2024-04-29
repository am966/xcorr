subroutine read_dos(filename, nbins, en, dos_raw, intdos)
!
use prec, only: dp
implicit none
!
! written by Amy Miller 2 March 2016
! This subprogram reads in the energy, dos and integrated dos
! from an optados file
!

!...! variables with various intent !
character(len=200),intent(in):: filename

integer, intent(in) :: nbins

double precision,intent(inout) :: en(nbins)
double precision,intent(inout) :: dos_raw(nbins)
double precision,intent(inout) :: intdos(nbins)

!...! local variables !
character :: dummy_character

integer :: i ! index to loop over


! open output file
open(11, file=trim(filename))
write(*,*) trim(filename)

read(11,21) dummy_character
21 format(11(1x/),a1)

! write xcorr to file for each value of the shift
do i = 1, nbins
    read(11,*) en(i), dos_raw(i), intdos(i)
end do

! close file and end cleanly
close(11)
return
end subroutine read_dos
