
module elect_primary
  contains
  
  subroutine elec_prim(A, b, n, X)
    use triangle_equation
    implicit none
    integer n, i, k, po
    real A(n,n),b(n), X(n), Au(n, n+1), temp(n+1)
    real maxa

    Au(:,1:n) = A
    Au(:,n+1) = b

    do i = 1, n-1
      maxa = abs(Au(i,i)) !attenton here is absolute value
      po = i
      do k = i, n
        if (abs(Au(k,i))>maxa) then
          maxa = Au(k,i)
          po = k
        end if
      end do
      
      ! attention here temp is (n+1)
      temp = Au(i,:)
      Au(i,:) = Au(po,:)
      Au(po,:) = temp

      do k = i+1, n
        Au(k,:) = Au(i,:)*(-Au(k,i)/Au(i,i)) + Au(k,:)
      end do
    end do
    
    do i = 1, n
      write(*,*) Au(i,:)
    end do

    A = Au(:,1:n)
    b = Au(:,n+1) 
    call up_tri_equ(A, b, n, X)
    
  end subroutine

end module