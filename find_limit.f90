subroutine find_limit(file_min,en_limit)
use prec, only: dp
implicit none

character(len=200),intent(in) :: file_min                 ! name of file of min band energy for a given charge
character :: dummy1, dummy2, dummy3, dummy4               ! dummy variables
double precision, intent(inout) :: en_limit               ! position of Fermi- Dirac function for a given charge
double precision :: band_min                              ! minimum band energy for a given charge

open(13, file=file_min)
read(13,*) dummy1, dummy2, dummy3, dummy4, band_min

en_limit = band_min + 5.0_dp

return
end subroutine find_limit
