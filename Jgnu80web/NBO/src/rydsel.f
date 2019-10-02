
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rydsel"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rydsel.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "rydsel.web"
      subroutine rydsel(LSTEMT,NEMT,NSEL1,LIST1,NSEL2,LIST2,WT)
      implicit none
      integer i,iryd,Ispin,LIST1,LIST2,LSTEMT,Munit,Mxao,Mxaolm,Mxbo,Nat
     &oms,Nbas,Ndim,NEMT,NSEL1,NSEL2
      double precision one,WT,wtthr
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension WT(Ndim),LIST1(Nbas),LIST2(Nbas),LSTEMT(Nbas)
      data one,wtthr/1.0D0,1.0D-4/
      
      
      
      
      
      NSEL1=0
      NSEL2=0
      do 100 i=1,NEMT
      iryd=LSTEMT(i)
      if(WT(iryd).LT.wtthr)then
      NSEL2=NSEL2+1
      LIST2(NSEL2)=iryd
      WT(iryd)=one
      else
      NSEL1=NSEL1+1
      LIST1(NSEL1)=iryd
      endif
100   continue
      return
      end
C* :1 * 
      
