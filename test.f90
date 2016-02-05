program test
  use Random
  use Timers
  use Sorting
  use Kinds
  use Cmdline

  implicit none

  integer(KINT4) :: i, j, idum=-234123512
  integer(KINT4) :: n
  real(KREAL4), dimension(:), allocatable :: a
  integer(KINT4), dimension(:), allocatable :: indx
  real(KREAL4) :: b

  type(Timer) :: crono1, crono2, crono3
  type(UGenerator) :: Uniform
  type(NGenerator) :: Gausian

  type(CommandLine) :: line
  character(len=:), allocatable :: fnamein

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  EXAMPLE OF HOW TO USE THE ARGUMENTS MODULE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call line%ReadCommandLine()
  do i=1, line%NArgs()
    call line%RetrieveCommand(fnamein, i)
    print*, fnamein
  enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  call Uniform%Randomize()
!  call Uniform%Randomize(idum)

  n=10000
  allocate(a(n), indx(n))

  print*, 'Generating', n, 'uniformly distributed numbers'
  call crono1%Tic()
  do i=1, n
    a(i)=Uniform%Getnumber()
  enddo
  call Sort(a, indx, .false.)
  print*, a(indx(1:3))
  call crono1%Tac()


  print*, 'Generating', n, 'normal distributed numbers'
  call crono2%Tic()
  do i=1, n
    a(i)=Gausian%Getnumber()
  enddo
  call Sort(a, indx, .false.)
  print*, a(indx(1:3))
  call crono2%Tac()

  print*,
  print*, 'Finish Example Program'
end program test
