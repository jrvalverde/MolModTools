
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 timer"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "timer.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "timer.web"
      subroutine timer(I,MSG)
      implicit none
      integer I,In,Iout,Iprint,Ipunch,iq
      real t
      character*1 MSG(6)
      real Told,tnew,cputim
      common/io/In,Iout,Ipunch
      common/ctimer/Told
      common/print/Iprint
      
      
      
      
      
      
      
      
99001 format(' TIME TAKEN BY ',6A1,' IS ',f6.2,' SEC.')
      
      tnew=cputim(iq)
      if(I.GT.1)then
      
      t=(tnew-Told)
      if(Iprint.GE.0)write(Iout,99001)MSG,t
      endif
      Told=tnew
      
      return
      
      end
C* :1 * 
      
