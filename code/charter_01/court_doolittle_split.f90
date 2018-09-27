module court_doolittle_split
contains

subroutine cour_spli(A, n, L, U)
  implicit none
  integer n,i,j,k
  real A(n,n), L(n,n), U(n,n)
  real sum
  
  L(:,:) = 0.0
  U(:,:) = 0.0
  do i =1,n
    U(i,i) = 1.0
  end do

  L(:,1) = A(:,1)
  U(1,:) = A(1,:)/L(1,1)
  do i = 2,n
    do j = i,n
      sum = 0.0
      do k = 1,i-1
        sum = sum + L(j,k)*U(k,i)
      end do
      L(j,i) = A(j,i)- sum
    end do

    do j = i+1, n
      sum = 0.0
      do k = 1,i-1
        sum = sum + L(i,k)*U(k,j)
      end do
      U(i,j) = (A(i,j) -sum)/L(i,i)
    end do
  end do

  do i=1,n
    write(*,*) L(i,:)
  end do
  do i=1,n
    write(*,*) U(i,:)
  end do
end subroutine

subroutine dool_spli(A, n, L, U)
  implicit none
  integer n,i,k,j
  real*8 A(n,n), L(n,n), U(n,n)
  real*8 sum

  L(:,:) = 0.0
  U(:,:) = 0.0
  do i =1,n
    L(i,i) = 1
  end do

  U(1,:) = A(1,:)
  L(:,1) = A(:,1)/U(1,1)
  do i =2,n
    do j = i,n
      sum = 0.0
      do k = 1,i-1
        sum = sum + L(i,k)*U(k,j)
      end do
      U(i,j) = A(i,j) -sum
    end do

    do j = i+1, n
      sum = 0.0
      do k =1,i-1
        sum = sum + L(j,k)*U(k,i)
      end do
      L(j,i) = (A(j,i) - sum)/U(i,i)
    end do
  end do

  do i=1,n
    write(*,*) L(i,:)
  end do
  do i=1,n
    write(*,*) U(i,:)
  end do
end subroutine

end module