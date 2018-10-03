module cholesky_split
contains

subroutine chol_spli(A, n, L)
  implicit none
  integer n, i, j, k, value
  real A(n,n), L(n,n), L_r(n,n)
  real sum1, sum2

  value = 1
  do i = 1, n
    do j= 1,n
      if (A(i,j) /= A(j,i)) then
        value = 0 
      end if
    end do
  end do
  
  if (value == 1) then
    L(:,:) = 0.0
    L(1,1) = sqrt(A(1,1))
    do i = 2,n
      L(i,1) = A(i,1)/L(1,1)
    end do

    do j = 2, n
      sum1 = 0.0    
      do k = 1,j-1
        sum1 = sum1 + L(j,k)**2
      end do
      L(j,j) = sqrt(A(j,j)-sum1)
      
      do i = j+1,n
        sum2 = 0.0
        do k = 1,j-1
          sum2 = sum2 +L(i,k)*L(j,k)
        end do
        L(i,j) = (A(i,j)-sum2)/L(j,j)
      end do
    end do

    
  else
    write(*,*) 'MATRIX A is not symmterical'
  end if

end subroutine

subroutine chol_spli_equa(A,b,n,X)
  use triangle_equation
  implicit none
  integer n, i, j
  real A(n,n), b(n), X(n)
  real L(n, n), L_r(n,n), y(n)

  call chol_spli(A, n, L)
  do i=1,n
    do j =1, n
      L_r(i,j) = L(j,i)
    end do
  end do
  
  call down_tri_equ(L, b, n, y)
  call up_tri_equ(L_r, y, n, X)


end subroutine

end module