module matrix_equation
contains
subroutine matr_equa(A, n,m,X, B)
  use triangle_equation
  implicit none
  integer n, m, i,j,pos
  real A(n,n), X(n,m), B(n,m)
  real T(n,n+m), temp(n+m)
  real maxt

  T(:,1:n)=A(:,:)
  T(:,n+1:n+m)=B(:,:)
  !! test
  do i=1,n
    write(*,*) T(i,:)
  end do

  do i = 1,n-1
    pos = i
    maxt = abs(T(i,i))
    do j = i+1,n
      if (abs(T(j,i)) > maxt) then
        maxt = abs(T(j,i))
        pos = j
      end if
    end do
    temp = T(i,:)
    T(i,:) = T(pos,:)
    T(pos,:) = temp
    
    do j = i+1, n
      T(j,:) = T(j,:) - T(i,:)*T(j,i)/T(i,i)
    end do
  end do
  !! test
  do i=1,n
    write(*,*) T(i,:)
  end do

  A(:,:) = T(:,1:n)
  B(:,:) = T(:,n+1:n+m)
  
  do i = 1,m
    call  up_tri_equ(A, B(:,i), n, X(:,i))
    write(*,*) X(:,i)
  end do
   
end subroutine
end module