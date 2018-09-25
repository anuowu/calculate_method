! this is code solve Trigonometric equation

program  main
  implicit none
  integer, parameter::n=4
  integer i
  real A(n,n), X(n), b(n)
  
  open(23, file='parameter.dat')
  read(23,*)
  read(23, *) (A(i,:), i=1,n)
  read(23,*)
  read(23,*) b
  
  !A = (2 1 4 3;0 3 2 5;0 0 7 3; 0 0 0 2)
  !b = (50 49 53 12)
  !write(*,*) A(2,:)
  !write(*,*) b
  call up_tri_equ(A, b, n, X)
  !call down_tri_equ(A, B,n, X)
  write(*,*) X

end program  main

subroutine up_tri_equ(A, b, n, X)
  implicit none
  integer n, i, j
  real A(n,n), b(n), X(n)
  real sum

  X(n) = b(n)/A(n,n)
  do i = n-1 , 1, -1
    sum = 0.0
    do j = i+1, n
      sum = sum + A(i, j)*X(j)
    end do
    X(i) = (b(i)-sum)/A(i,i) 
  end do

end subroutine

subroutine down_tri_equ(A, b, n, X)
  implicit none
  integer n, i,j
  real A(n,n), b(n), X(n)
  real sum

  X(1) = b(1)/A(1,1)
  do i =2, n
    sum = 0.0
    do j = 1,i-1
      sum = sum+A(i,j)*X(j)
    end do
    X(i) = (b(i)-sum)/A(i,i)
  end do

end subroutine

