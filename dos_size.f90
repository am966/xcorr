function dos_size(input_file)
use prec, only: dp
implicit none
!
! written by Amy Miller on 1 March 2016
! This function reads in dos array size
!
character(len=200), intent(in):: input_file
integer :: dos_size
integer:: n_bins
character:: dummy_character
character:: dummy1, dummy2, dummy3, dummy4, dummy5, dummy6

open(9,file=trim(input_file))
read(9,20)dummy_character !Read in dummy character to skip first 10 lines
20 format(9(1x/),a1)
!
! read dimensions of dos array
read(9,*) dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, n_bins
close(9)

dos_size = n_bins

end function dos_size

