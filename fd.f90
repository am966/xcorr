double precision function fd(enk, en_limit)
use prec, only: dp
implicit none
double precision, intent(in) :: enk, en_limit
double precision :: kappa, temp

kappa = 2.2_dp

temp = dble(1.0_dp) / dble(exp(kappa * (enk - en_limit)) + 1.0_dp)
fd = temp

return
end function fd

