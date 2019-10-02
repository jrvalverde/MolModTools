
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 defunt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "defunt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "defunt.web"
      subroutine defunt
      implicit none
      integer In,Iout,Ipunch,Iunit
      common/io/In,Iout,Ipunch
      common/munit/Iunit(20)
      
      
      
      
      
      
      In=5
      Iout=6
      Ipunch=7
      Iunit(2)=In
      Iunit(3)=Iout
      Iunit(4)=Ipunch
      Iunit(5)=9
      Iunit(6)=8
      Iunit(12)=3
      
      return
      
      end
C* :1 * 
      
