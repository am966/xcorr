subroutine xcorr_delegate(seedname, broadening, charge, charge_cha, surfch)
use prec, only: dp
implicit none

! This subroutine calls other subroutines in order to 
! calculate the cross correlation between two DOS data sets

character(len=200),intent(in) :: seedname        ! name of data set
character(len=10), intent(in) :: charge_cha      ! charge
character(len=10), intent(in) :: broadening      ! type of broadening

double precision, intent(in) :: charge, surfch    ! charge and surface charge

! local variables
character(len=200) :: mfile                      ! name of data set
character(len=200) :: pfile                      ! name of data set
character(len=200) :: ef_filem                   ! name of file of Ef and number of electrons - charge
character(len=200) :: ef_filep                   ! name of file of Ef and number of electrons + charge
character(len=200) :: file_min_m                 ! name of file of min band energy - charge
character(len=200) :: file_min_p                 ! name of file of min band energy + charge

integer :: dos_size        ! size read in using dos_size function
integer :: msize, psize          ! size of data sets for each charge -/+
integer :: i               ! dummy index to read in vectors

double precision :: denom                         ! denominator of local softness
double precision :: eln_numm, eln_nump            ! number of electrons
double precision :: efm, efp                      ! fermi level based on optados
double precision :: en_limit_m, en_limit_p        ! limit for Fermi- Dirac function
double precision :: samplm, samplp                ! optados energy sampling frequency in eV
double precision :: diff                          ! (eln_numm - eln_nump ) - charge

double precision,dimension(:),allocatable:: en_m, en_p
double precision,dimension(:),allocatable:: dos_raw_m, dos_raw_p
double precision,dimension(:),allocatable:: intdos_m, intdos_p

! define names of files for positive and negative charge
mfile='./'//trim(seedname)//'_m'//trim(charge_cha)//'.'//trim(broadening)//'.dat'
pfile='./'//trim(seedname)//'_p'//trim(charge_cha)//'.'//trim(broadening)//'.dat'
ef_filem = './'//trim(seedname)//'_m'//trim(charge_cha)//'.ef_out'
ef_filep = './'//trim(seedname)//'_p'//trim(charge_cha)//'.ef_out'
file_min_m = trim(seedname)//'_m'//trim(charge_cha)//'.min_out'
file_min_p = trim(seedname)//'_p'//trim(charge_cha)//'.min_out'

! call function to get size of arrays
msize = dos_size(mfile)
psize = dos_size(pfile)

allocate(en_m(msize))
allocate(dos_raw_m(msize))
allocate(intdos_m(msize))

allocate(en_p(psize))
allocate(dos_raw_p(psize))
allocate(intdos_p(psize))

! read in raw arrays
call read_dos(mfile, msize, en_m, dos_raw_m, intdos_m)
call read_dos(pfile, psize, en_p, dos_raw_p, intdos_p)

! calculate fermi level and find energy limit to cross correlate DOS
call ef_n(ef_filem, msize, en_m, intdos_m, efm, eln_numm, samplm)
call ef_n(ef_filep, psize, en_p, intdos_p, efp, eln_nump, samplp)

call find_limit(file_min_m,en_limit_m)
call find_limit(file_min_p,en_limit_p)

! test that the number of electrons used by optados was correct
diff = abs(abs(eln_numm-eln_nump) - (charge*2)) 
if (diff <= 0.000001_dp) then

    ! calculate cross correlation of DOS
    call wrap_xcorr(seedname, surfch, dos_raw_m, dos_raw_p, en_m, en_p, msize, psize, efm, efp, samplm, samplp, denom,&
    en_limit_m, en_limit_p)

else 
    ! write error messages if wrong number of electrons
    write(*,*) "WARNING!!!! input file has wrong charge/ number of electrons"
    write(*,*) "diff = (nelm - nelp) - charge :", diff, ">", 0.000001_dp
    write(*,*) "Check optados i/o including .bands has correct precision for number of electrons"
end if

! deallocate arrays!!!
deallocate(en_m, en_p)
deallocate(dos_raw_m, dos_raw_p)
deallocate(intdos_m, intdos_p)

return
end subroutine xcorr_delegate
