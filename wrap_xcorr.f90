subroutine wrap_xcorr(seedname, surfch, dos_raw_m, dos_raw_p, en_m, en_p, msize, psize, efm, efp, samplm, samplp, denom,&
en_limit_m, en_limit_p)
use prec, only: dp
implicit none
!
! written by Amy Miller on 1 March 2016
! this subprogram calculates the cross correlation
! of the DOS as a function of the sampling energy index
!
character(len=200),intent(in) :: seedname        ! name of data set

integer, intent(in) :: msize, psize
!
double precision, intent(in) :: surfch                 ! surface charge in electrons per sq angstrom
double precision, intent(in) :: efm, efp               ! fermi level
double precision, intent(in) :: en_limit_m, en_limit_p ! limit of energy for Fermi Dirac function
double precision, intent(in) :: samplm, samplp         ! sampling frequency

double precision, intent(in) :: dos_raw_m(msize)       ! DOS - charge        \
double precision, intent(in) :: dos_raw_p(psize)       ! DOS + charge         |
double precision, intent(in) :: en_m(msize)            ! energy - charge      | NB -/+ differ in size
double precision, intent(in) :: en_p(psize)            ! energy + charge     /

double precision, intent(inout) :: denom

! local variables
integer :: i       ! index over which energy is sampled
integer :: nsize   ! size of largest vector
integer :: n_xcorr ! size of vector of cross correlation data
integer :: imax    ! shift of index for max cross correlation
integer :: n_small ! size of smallest vector

double precision :: fd                                ! f-d function
double precision :: shift                             ! shift in energy for cross correlation maximum

double precision,dimension(:),allocatable :: fd_m     ! fermi dirac distrib
double precision,dimension(:),allocatable :: fd_p     ! fermi dirac distrib

double precision,dimension(:),allocatable :: dos_m
double precision,dimension(:),allocatable :: dos_p

double precision, dimension(:), allocatable :: xcorr  ! cross correlation vector

! allocate vector sizes for cross correlation
nsize = max(msize, psize)    ! 
allocate(fd_m(nsize))
allocate(fd_p(nsize))

allocate(dos_m(nsize))
allocate(dos_p(nsize))

n_xcorr = (2*nsize) - 1     ! size of cross correlation vector
allocate(xcorr(n_xcorr))

fd_m = 0.0_dp
fd_p = 0.0_dp

! write sizes to screen
write(*,*) "msize : ", msize, "psize : ", psize
n_small = min(msize, psize)

write(*,*) n_small, "th element of dos: - charge:", dos_raw_m(n_small) 
write(*,*) n_small, "th element of dos: + charge:", dos_raw_p(n_small) 
! populate vectors for the dos. pad shorter vector with zeros
! NB multiplying by fermi dirac distribution
do i = 1, nsize
    ! both have elements in this place
    if ((i <= msize) .and. (i <= psize)) then
        ! calculate f-d
        fd_m(i) = fd(en_m(i), en_limit_m)
        fd_p(i) = fd(en_p(i), en_limit_p)

        dos_m(i) = dos_raw_m(i) * fd(en_m(i), en_limit_m)
        dos_p(i) = dos_raw_p(i) * fd(en_p(i), en_limit_p)

    ! only + vector has element here (- vector is shorter so pad with 0s)
    else if ((i > msize) .and. (i <= psize)) then
        fd_p(i) = fd(en_p(i), en_limit_p)
        
        dos_p(i) = dos_raw_p(i) * fd(en_p(i), en_limit_p)
        dos_m(i) = 0.0_dp
        !write(*,*) "i=", i, "in p only"

    ! only - vector has element here (+ vector is shorter so pad with 0s)
    else if ((i > psize) .and. (i <= msize)) then
        fd_m(i) = fd(en_m(i), en_limit_m)

        dos_m(i) = dos_raw_m(i) * fd(en_m(i), en_limit_m)
        dos_p(i) = 0.0_dp
        !write(*,*) "i=", i, "in m only"

    else
        write(*,*) "Warning! Check size of arrays"
    end if
end do

! call subroutine to perform actual cross correlation
call calc_xcorr(dos_m, dos_p, nsize, xcorr, n_xcorr, imax)

! call subroutine to write cross correlation vector into output file
call write_xcorr(seedname, xcorr, n_xcorr, nsize)

! call subrout to calculate energy shift
call shift_calc(en_m, en_p, msize, psize, imax, samplm, samplp, shift)

! calculate denominator 
denom = dble((efp - efm) - shift)/ dble(2*surfch)

! write results to output file
call out_file(seedname, surfch, imax, shift, efm, efp, denom)

! deallocate arrays
deallocate(dos_m, dos_p)
deallocate(fd_m, fd_p)
deallocate(xcorr)

return
end subroutine wrap_xcorr
