
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "putf.web"
      subroutine putf(NZ,LBL,LALPHA,LBETA,NPARM,NVAR,F,FORCE,IPRINT)
      implicit none
      double precision dx,F,FORCE
      integer i,ialpha,ibeta,ibl,In,Iout,IPRINT,Ipunch,j,LALPHA,LBETA,LB
     &L,NPARM,NVAR,NZ
      dimension LBL(*),LALPHA(*),LBETA(*),F(*),FORCE(*)
      common/io/In,Iout,Ipunch
      
      
99001 format(' FROM PUTF, CONTENTS OF FORCE:')
99002 format(1x,i3,d20.10)
      
      call aclear(NVAR,FORCE)
      if(NZ.GE.2)then
      do 50 i=2,NZ
      ibl=iabs(LBL(i))
      if(ibl.NE.0)then
      dx=F(i-1)
      if(LBL(i).LT.0)dx=-dx
      FORCE(ibl)=FORCE(ibl)+dx
      endif
50    continue
      
      if(NZ.GE.3)then
      j=NZ-3
      do 60 i=3,NZ
      ialpha=iabs(LALPHA(i))
      if(ialpha.NE.0)then
      dx=F(i+j)
      if(LALPHA(i).LT.0)dx=-dx
      FORCE(ialpha)=FORCE(ialpha)+dx
      endif
60    continue
      
      if(NZ.GE.4)then
      j=NZ+NZ-6
      do 70 i=4,NZ
      ibeta=iabs(LBETA(i))
      if(ibeta.NE.0)then
      dx=F(i+j)
      if(LBETA(i).LT.0)dx=-dx
      FORCE(ibeta)=FORCE(ibeta)+dx
      endif
70    continue
      endif
      endif
      endif
      
      if(IPRINT.GT.0)then
      write(Iout,99001)
      write(Iout,99002)(i,FORCE(i),i=1,NVAR)
      endif
      return
      
      end
C* :1 * 
      
