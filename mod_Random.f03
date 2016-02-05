!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Short Documentation:
!
! This module provies two objects:
! - UGenerator: generates Uniformly distributed numbers
! - NGenerator: generates starndard normal distributed numbers
!
! Each objects provides two methods:
! - Getnumber: provides a number and updates seed
! - Randomize: sets the sed, either to a given number (optional) or randomly
!              according to the time and PID
!
! JJ 2016
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module Random
  use Kinds
  implicit none
  private
  public :: UGenerator, NGenerator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  ABSTRACT TYPE USED TO STORE IDUM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type Generator
    private
    integer(KINT4) :: idum
  contains
    procedure, public :: Randomize
    procedure, public :: Getnumber
  end type Generator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  OBJECT TO CONTAIN UNIFORM GENERATORS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type, extends (Generator):: UGenerator
  end type UGenerator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  OBJECT TO CONTAIN NORMAL GENERATORS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type, extends (Generator):: NGenerator
  end type NGenerator

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    GETTER OF RANDOM NUMBER AND CHANE IDUM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function Getnumber (self)
    class (Generator), intent(inout) :: self

    select type (self)
    type is (UGenerator)
      Getnumber=ran1(self%idum)
    type is (NGenerator)
      Getnumber=gasdev(self%idum)
    end select
    return
  end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  INITIALIZES EITHER TO A GIVEN NUMBER OR A RANDOM ONE (TIME AND PID BASED)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine Randomize (self, seed)
    class (Generator), intent(inout) :: self
    integer(KINT4), optional, intent(in) :: seed
    integer(KINT4) :: s

    if (present(seed))then
      self%idum=seed
    else
      call system_clock(s)
      self%idum = -100*abs( mod((s*181)*((getpid()-83)*359), 104729) )
    endif
  end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  LECACY SUBROUTINES FROM NUMERICAL RECIPES IN FORTRAN 77
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      FUNCTION gasdev(idum) result (x)
      INTEGER idum  
      REAL x
      INTEGER iset  
      REAL fac,gset,rsq,v1,v2
      SAVE iset,gset  
      DATA iset/0/  
      if (iset.eq.0) then  
1       v1=2.*ran1(idum)-1.  
        v2=2.*ran1(idum)-1.  
        rsq=v1**2+v2**2  
        if(rsq.ge.1..or.rsq.eq.0.)goto 1  
        fac=sqrt(-2.*log(rsq)/rsq)  
        gset=v1*fac  
        x=v2*fac  
        iset=1  
      else  
        x=gset  
        iset=0  
      endif  
      END function

      FUNCTION ran1(idum) result (x) 
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV  
      REAL x,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)  
      INTEGER j,k,iv(NTAB),iy  
      SAVE iv,iy  
      DATA iv /NTAB*0/, iy /0/  
      if (idum.le.0.or.iy.eq.0) then  
        idum=max(-idum,1)  
        do 11 j=NTAB+8,1,-1  
          k=idum/IQ  
          idum=IA*(idum-k*IQ)-IR*k  
          if (idum.lt.0) idum=idum+IM  
          if (j.le.NTAB) iv(j)=idum  
11      continue  
        iy=iv(1)  
      endif  
      k=idum/IQ  
      idum=IA*(idum-k*IQ)-IR*k  
      if (idum.lt.0) idum=idum+IM  
      j=1+iy/NDIV  
      iy=iv(j)  
      iv(j)=idum  
      x=min(AM*iy,RNMX)  
      return  
      END function

end module Random
