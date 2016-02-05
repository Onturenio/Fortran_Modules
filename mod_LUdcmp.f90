module lu_decomp
use Kinds

private
public :: lu_solve, lu_invert

CONTAINS

  ! perform LU DECOMPOSITION
  SUBROUTINE ludcmp(a,indx,d)
  !USE nrutil, ONLY : assert_eq,nrerror, outerprod, swap, imaxloc
  IMPLICIT NONE
  REAL(KREAL4), DIMENSION(:,:), INTENT(INOUT) :: a
  INTEGER, DIMENSION(:), INTENT(OUT) :: indx
  REAL(KREAL4), INTENT(OUT) :: d
  REAL(KREAL4), DIMENSION(size(a,1)) :: vv
  REAL(KREAL4), PARAMETER :: TINY=1.0e-20_KREAL4
  INTEGER :: j,n,imax
  REAL(KREAL4), DIMENSION(size(a,2)) :: tmp
  INTEGER, dimension(1) :: imaxloc

  n = size(indx)
  !n=assert_eq(size(a,1),size(a,2),size(indx),'ludcmp')
  if (size(a,1) .ne. n .or. size(a,2) .ne. n) then
    write (*,*) 'nrerror: matrix sizes not correct for routine ludcmp'
    STOP 'program terminated by ludcmp'
  endif


  d=1.0
  vv=maxval(abs(a),dim=2)
  if (any(vv == 0.0_KREAL4)) then
    write(*,*) 'singular matrix in ludcmp'
    STOP 'program terminated by ludcmp'
  endif
  vv=1.0_KREAL4/vv
  do j=1,n
    imaxloc = maxloc(vv(j:n)*abs(a(j:n,j)))
    imax=(j-1)+imaxloc(1)
    if (j /= imax) then
      tmp = a(imax,:)
      a(imax,:) = a(j,:)
      a(j,:) = tmp
      d=-d
      vv(imax)=vv(j)
    end if
    indx(j)=imax
    if (a(j,j) == 0.0_KREAL4) a(j,j)=TINY
    a(j+1:n,j)=a(j+1:n,j)/a(j,j)
    a(j+1:n,j+1:n)=a(j+1:n,j+1:n) - &
      spread(a(j+1:n,j), dim=2, ncopies=(n-j)) * &
      spread(a(j,j+1:n),dim=1, ncopies=(n-j))
  end do
  END SUBROUTINE ludcmp


  ! Perform LU BACK SUBSTITUION
  SUBROUTINE lubksb(a,indx,b)
  IMPLICIT NONE
  REAL(KREAL4), DIMENSION(:,:), INTENT(IN) :: a
  INTEGER, DIMENSION(:), INTENT(IN) :: indx
  REAL(KREAL4), DIMENSION(:), INTENT(INOUT) :: b
  INTEGER :: i,n,ii,ll
  REAL(KREAL4) :: summ
  
  n = size(indx)
  !n=assert_eq(size(a,1),size(a,2),size(indx),'lubskb')
  if (size(a,1) .ne. n .or. size(a,2) .ne. n) then
    write (*,*) 'nrerror: matrix sizes not correct for routine lubskb'
    STOP 'program terminated by lubskb'
  endif


  ii=0
  do i=1,n
    ll=indx(i)
    summ=b(ll)
    b(ll)=b(i)
    if (ii /= 0) then
      summ=summ-dot_product(a(i,ii:i-1),b(ii:i-1))
    else if (summ /= 0.0) then
      ii=i
    end if
    b(i)=summ
  end do
  do i=n,1,-1
    b(i) = (b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
  end do
  END SUBROUTINE lubksb


  function LU_SOLVE(A, b, index, decomp) RESULT(x)
  ! Return the LU Decomposition of A, in A, if index not set.
  ! Return x = A^-1 b either way
 
  real(KREAL4), dimension(:,:), intent(inout) :: A
  real(KREAL4), dimension(:), intent(in) :: b
  real(KREAL4), dimension(size(b))  :: x
  integer, dimension(:), intent(inout), OPTIONAL :: index
  logical, intent(in), OPTIONAL :: decomp
  
  logical :: lud
  real(KREAL4) :: d
  integer, dimension(size(b)) :: indx

  lud = .FALSE.
  if (present(decomp)) lud = decomp

  if (present(index)) indx = index
  
  if (lud) then
    ! must get the LU decomposition of A
    CALL ludcmp(A, indx, d)
    if (present(index)) index = indx
  endif
      
  x = b ! initialize x
  CALL lubksb(A, indx, x)

  end function LU_SOLVE

  function LU_INVERT(A) RESULT(Ainv)
  ! Invert A using the LU Decomposition technique
 
  real(KREAL4), dimension(:,:), intent(in) :: A
  real(KREAL4), dimension(size(A,1), size(A,2)) :: Ainv, Alu

  real(KREAL4) :: d
  integer, dimension(size(A,1)) :: indx
! real(KREAL4), dimension(size(A,1), size(A,2)) :: E

  integer :: i, N

  Alu = A
  N = size(A,1)
  Ainv = 0.0_KREAL4
  do i = 1, N
    Ainv(i,i) = 1.0_KREAL4
  enddo 

  ! must get the LU decomposition of A
  CALL ludcmp(Alu, indx, d)

  ! now do the backsubstitution
  do i = 1, N
    CALL lubksb(Alu, indx, Ainv(:,i))
  enddo

  end function LU_INVERT

end MODULE LU_DECOMP
