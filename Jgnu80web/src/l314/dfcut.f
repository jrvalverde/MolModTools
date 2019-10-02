
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dfcut"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dfcut.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "dfcut.web"
      subroutine dfcut(IOP)
      implicit none
      double precision Fillct,Pqcut1,Pqcut2,Pqcut3,Ten,Xint,Zero
      integer Idmp,Idump
      integer IOP(*)
      common/dump/Idmp,Idump
      common/dfcuts/Pqcut1,Pqcut2,Pqcut3,Fillct
      common/int/Zero,Xint(12)
      equivalence(Ten,Xint(10))
      
      
99001 format(3D20.10)
99002 format(8H PQCUTS:,3D20.10)
      
      if(IOP(29).NE.0)then
      
      read(5,99001)Pqcut1,Pqcut2,Pqcut3
      if(Idump.EQ.11)write(6,99002)Pqcut1,Pqcut2,Pqcut3
      else
      Pqcut1=1.0D-10
      Pqcut2=1.0D-20
      Pqcut3=42.0D0
      endif
      
      
      return
      
      end
C* :1 * 
      
