subroutine out_file(seedname, surfch, imax, shift, efm, efp, denom)
use prec, only: dp
implicit none
!
! written by Amy Miller 21 Jul 2016
! This subprogram writes the cross correlation variables and also the denominator
! to a file
!
character(len=200),intent(in):: seedname

integer :: imax

double precision, intent(in) :: surfch                ! surface charge in electrons per sq angstrom
double precision, intent(in) :: efm, efp              ! fermi level
double precision, intent(in) :: shift                 ! shift for max cross correlation
double precision, intent(in) :: denom                 ! corrected denominator of local softness in eV^-1 A^-3

! local variables

character(len=210) :: out_name


out_name = trim(seedname)//'.out'


open(12,file=trim(out_name))

write(12,*) 'output file for : ', trim(seedname), ' calculation'
write(12,*) 'surface charge : ', surfch

write(12,*) 'fermi level for +', surfch, ':', efp
write(12,*) 'fermi level for -', surfch, ':', efm

write(12,*) 'index shift to obtain max cross correlation  : ', imax
write(12,*) 'energy shift to obtain max cross correlation  : ', shift
write(12,*) 'corrected denominator of local softness : ', denom, 'eV^-1 A^-2'

close(12)

return 
end subroutine out_file
