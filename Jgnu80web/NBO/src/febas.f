
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 febas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "febas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "febas.web"
      subroutine febas(NSHELL,NEXP,ISCR)
      implicit none
      integer ISCR,len,NEXP,nfile,NSHELL
      dimension ISCR(*)
      
      
      nfile=5
      call nbinqr(nfile)
      if(nfile.GT.0)then
      call nbread(ISCR,1,nfile)
      NSHELL=ISCR(1)
      NEXP=ISCR(2)
      len=1+(3*NSHELL+1)/2+5*NEXP
      call nbread(ISCR,len,nfile)
      else
      NSHELL=0
      endif
      return
      end
C* :1 * 
      
