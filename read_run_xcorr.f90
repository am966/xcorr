program read_run_xcorr
use prec, only: dp
implicit none

! Written by Amy Miller on 2 March 2016
! This program calculates the cross correlation of the DOS of different charges
! as a function of energy shift

! define parameter

character :: dummy_character
character(len=200) :: seedname
character(len=10) :: charge_cha
character(len=10) :: broadening

integer :: numcalc,i

double precision :: charge, area, surfch

double precision, dimension(3) :: a1
double precision, dimension(3) :: a2

! Read seed names from input file and perform cross correlation

! read in charge as double precision variable
! skip 9 lines
open(8,file='./calc_xcorr.run')
read(8,14) dummy_character
14 format(8(1x/),a1)
read(8,*) charge
close(8)

open(8,file='./calc_xcorr.run')
! skip two lines
read(8,15) dummy_character
15 format(1(1x/),a1)

read(8,*) dummy_character, a1(1:3)
read(8,*) dummy_character, a2(1:3)

! skip two lines
read(8,15) dummy_character

read(8,*) dummy_character, numcalc

! skip two lines
read(8,15) dummy_character

read(8,*) charge_cha

! skip two lines
read(8,15) dummy_character

read(8,*) broadening
write(*,*)"broadening type : ", broadening

! skip two lines
read(8,15) dummy_character

! calculate surface charge

call calc_surfch(charge, a1, a2, area, surfch)

write(*,*)'surface charge : ', surfch, ' electrons A^-2'
write(*,*)'calculating the cross correlation of', numcalc, 'DOS calculations'

do i=1,numcalc
    read(8,*) seedname
    write(*,*) '------------------------------------------------------------'
    write(*,*) '    '
    write(*,*) 'file number', i, trim(seedname)
    write(*,*) '------------------------------------------------------------'
    call xcorr_delegate(seedname, broadening, charge, charge_cha, surfch)
end do

close(8)

stop
end program read_run_xcorr
