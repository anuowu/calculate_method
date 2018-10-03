module chase_method
contains

subroutine chas_meth(A, b, n, X)
  use triangle_equation
  implicit none
  integer n,i,j
  real A(n,n), b(n), X(n)
  real L(n,n),U(n,n), y(n)
  
  U(:,:) = 0.0
  L(:,:) = 0.0
  do i = 1,n
    L(i,i) = 1
  end do
  do i = 1,n-1
    U(i,i+1) = A(i,i+1)
  end do
  
  U(1,1) = A(1,1)
  do i =2,n
    L(i,i-1) = A(i, i-1)/U(i-1, i-1)
    U(i,i) =A(i,i)-L(i,i-1)*U(i-1,i)
  end do

  do i =1,n
    write(*,*) L(i,:)
  end do
  do i =1,n
    write(*,*) U(i,:)
  end do
  
  call down_tri_equ(L, b, n, y)
  call up_tri_equ(U, y, n, X)

end subroutine

end module