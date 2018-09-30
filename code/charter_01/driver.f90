program  main
  use court_doolittle_split
  implicit none
  integer, parameter::n=4
  integer i
  real A(n,n), X(n), b(n)
  real L(n,n), U(n,n)
  
  open(23, file='parameter.dat')
  read(23,*)
  read(23, *) (A(i,:), i=1,n)
  read(23,*)
  read(23,*) b
  
  !A = (2 1 4 3;0 3 2 5;0 0 7 3; 0 0 0 2)
  !b = (50 49 53 12)
  !write(*,*) A(2,:)
  !write(*,*) b
    
  call lu_spli(A, b, n, X)
  !call down_tri_equ(A, B,n, X)
  write(*,*) X

end program  main