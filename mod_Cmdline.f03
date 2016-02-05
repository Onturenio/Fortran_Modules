!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Short Documentation:
!
! This module provies one object:
! - CommandLine: contains an array of strings with the commands. Element 0 is
!   program execution, the rest are additional options
!
! The object provides three methods:
! - ReadCommandLine: is the constructor that reads the command line
! - NArgs(CommandLine): is a getter for number of arguments
! - RetrieveCommand(CommandLine, integer n): is a getter for argument number n
!
! JJ 2016
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module Cmdline
  implicit none
  private
  public :: CommandLine

  type StringType
    private
    character(len=1), dimension(:), allocatable :: char
  end type StringType

  type CommandLIne
    private
    type(StringType), dimension(:), allocatable :: string
    contains
      procedure, public :: RetrieveCommand, ReadCommandLine, NArgs
  end type CommandLIne

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  OBTAINS THE NUMBER OF ARGUMENTS INTRODUCED WHEN RUNNING THE PROGRAM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer function NArgs(self)
    class(CommandLIne), intent(in) :: self
    NArgs=size(self%string)
  end function NArgs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  CONSTRUCTOR THAT POPULATES THE COMMAND LINE VARIABLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine ReadCommandLine(self)
    class(CommandLIne), intent(out) :: self
    character(len=:), allocatable :: buffer
    integer :: nargs, i, length
    logical :: vflag

    nargs=command_argument_count()
    allocate(self%string(nargs))

    do i=1, nargs
      call get_command_argument(i, length=length)
      if (allocated(buffer)) deallocate(buffer)
      allocate(character(length) :: buffer)
      call get_command_argument(i, buffer)
      call ArchiveCommand(self, buffer, i)
    enddo

  end subroutine ReadCommandLine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!PUTS A STRING IN SLOT 'i' IN THE COMMANDLINE VARIABLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine ArchiveCommand(self, inputstring, i)
    class(CommandLIne), intent(inout) :: self
    character(len=*),intent(in) :: inputstring
    integer, intent(in) :: i
    integer :: j,n

    n=len(inputstring)
    if(allocated(self%string(i)%char)) deallocate(self%string(i)%char)
    allocate(self%string(i)%char(n))
    do j=1, n
      self%string(i)%char(j)=inputstring(j:j)
    enddo

  end subroutine ArchiveCommand

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!OBTAINS THE STRING IN SLOT 'i' IN THE COMMANDLINE VARIABLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine RetrieveCommand(self, outputstring, i)
    class(CommandLIne), intent(in) :: self
    character(len=:), allocatable, intent(out) :: outputstring
    integer, intent(in) :: i
    integer :: j, n

    n=size(self%string(i)%char)
    allocate(character(n) :: outputstring)
    do j=1, n
      outputstring(j:j)=self%string(i)%char(j)
    enddo

  end subroutine RetrieveCommand

end module Cmdline
