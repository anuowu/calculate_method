
module gaussian_elimination
  contains
  subroutine gaus_elim(A, b, n, X)
    USE module triangle_equation
    implicit none 
    integer n, i, j
    real A(n,n), b(n), X(n), Au(n, n+1)

    Au(:, 1:n) = A 
    Au(:, n+1) = b
    
    do i =1, n
      do j = i+1, n
        Au(j,:) = Au(j,:)*(-Au(i,i)/Au(j,i)) + Au(i,:)
      end do
    end do

    do i=1,n
      write(*,*) Au(i,:)
    end do
    
    A = Au(:, 1:n)
    b = Au(:, n+1)
    call up_tri_equ(A, b, n, X)
  end subroutine

end module