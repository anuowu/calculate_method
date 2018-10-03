module det
contains
subroutine det_calu(A, n, value)
  use court_doolittle_split
  implicit none
  integer n,i
  real A(n,n), value
  real L(n,n), U(n,n)

  call cour_spli(A, n, L, U)
  
  value = 1
  do i =1, n
    value = value *L(i,i)
  end do
end subroutine

end module