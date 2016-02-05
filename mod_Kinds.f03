!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Short Documentation:
!
! This module provides the variables to define variables in other modules and programs
!
! KINT4:  regular integer
! KINT8:  double integer
! KREAL4: regular real
! KREAL4: double precission real
!
! JJ 2016
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module Kinds
  implicit none
  public

  integer, parameter ::  KINT4 = selected_int_kind(6)
  integer, parameter ::  KINT8 = selected_int_kind(15)
  integer, parameter ::  KREAL4 = selected_real_kind(6)
  integer, parameter ::  KREAL8 = selected_real_kind(15)
end module Kinds
