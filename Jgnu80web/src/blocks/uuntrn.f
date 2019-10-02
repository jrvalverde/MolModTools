
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uuntrn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uuntrn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "uuntrn.web"
      blockdata uuntrn
      implicit none
      integer Idum,Io,Maxpos,Nunits,Units
      real Wait
      logical Print,Syncs
      common/ntr/Wait(3,4),Io(3,4),Units(4),Nunits,Maxpos(4),Print(4),Sy
     &ncs(4),Idum
      data Nunits/0/,Units/-1,-1,-1,-1/,Wait/12*0./,Io/12*0/
      data Print,Syncs/8*.FALSE./
      end
C* :1 * 
      
