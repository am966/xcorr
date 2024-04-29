subroutine ef_n(ef_file, n_bins, en, intdos, ef, eln_num, dos_sampl)
use prec, only: dp
implicit none
!
! This subroutine reads in ef, N el and sampling frequency
!and calculates the fermi level based on Nel 
!
character(len=200),intent(in) :: ef_file          ! input file name to read in Ef and n elec
integer, intent(in) :: n_bins

double precision, intent(in) :: en(n_bins)
double precision, intent(in) :: intdos(n_bins)
double precision, intent(inout) :: ef
double precision, intent(inout) :: eln_num        ! number of electrons dble prec
double precision, intent(inout)  :: dos_sampl     ! dos sampling in eV

character :: dummychar          ! dummy char to skip lines
integer :: pos_ef                                 ! position of fermi level

! open input file and read in number of electrons
open(10, file=trim(ef_file))
read(10,*) dummychar
read(10,*) dummychar, ef
read(10,*) dummychar, eln_num
read(10,*) dummychar, dos_sampl
close(10)

! call subroutine to calculate Ef
call find_pos(intdos, n_bins, eln_num, pos_ef) 

write(*,*) "Ef optados: ", ef
write(*,*) "Ef calc: ", en(pos_ef)

return
end subroutine ef_n
