! this is code solve Trigonometric equation

module triangle_equation
  contains
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

end module