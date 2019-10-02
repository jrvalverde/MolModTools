
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uustv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uustv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "uustv.web"
      blockdata uustv
      implicit none
      double precision F100,F20i,F42,F6i,Four,Half,One,Onept5,Pt5,R1,R2,
     &R3,R3ov2,R4,Root15,Root3,Root5,Ten,Three,Two
      double precision Xint,Z1,Z2,Z3,Zero,Zero1
      integer Lbound,N10ord,N5ord,N6ord,N7ord,Nordr
      integer Ubound,Ulpure
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/int/Zero1,Xint(12)
      common/intcon/F6i,F20i,F100
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      data Zero1/0.0D0/
      data Zero/0.0D0/,Half/0.5D0/,One/1.0D0/,Onept5/1.5D0/,Two/2.0D0/,T
     &hree/3.0D0/,Four/4.0D0/,Ten/10.0D0/,F42/42.0D0/
      data Xint/1.0D0,2.0D0,3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0,9.0D0,10
     &.0D0,11.0D0,12.0D0/
      data Pt5/0.5D0/
      data F100/100.0D0/
      data Ubound/1,4,10,20/,Ulpure/1,4,9,17/,Lbound/5*1,2,5,11,1,1,5,11
     &/,N6ord/1,2,3,4,5,6,7,8,9,10/,N5ord/1,2,3,4,5,6,7,8,9/,N10ord/11,1
     &2,13,14,15,16,17,18,19,20/,N7ord/11,12,13,14,15,16,17/
      end
C* :1 * 
      
