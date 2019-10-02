
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rfld"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rfld.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "rfld.web"
      subroutine rfld(REAL,ERROR)
      implicit none
      integer i,Icd,Ipt,isexp,j,k,Length,Lfn,lk,Look,nchar,ndec,Nexp
      double precision one,REAL,sign,ten,zero
      logical ERROR,expsgn,mantis
      
      common/nbcrd1/Icd(80),Look(80),Length,Ipt,Lfn,Nexp
      common/nbcrd2/Point,End,Next,Exp
      logical Point,End,Next,Exp
      
      dimension nchar(15)
      
      data nchar/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H.,1H+,1H-,1HD
     &,1HE/
      data zero,one,ten/0.0D0,1.0D0,10.0D0/
      
      
      REAL=zero
      sign=one
      ndec=0
      isexp=1
      Nexp=0
      expsgn=.FALSE.
      Exp=.FALSE.
      Point=.FALSE.
      ERROR=.FALSE.
      mantis=.FALSE.
      End=.FALSE.
      
      
      if(Next)call fndfld
      if(.NOT.(End))then
      if(Length.NE.0)then
      
      
      do 40 j=1,Length
      lk=Look(j)
      do 10 i=1,15
      if(lk.EQ.nchar(i))goto 20
10    continue
      goto 100
20    k=i-11
      if(k.LT.0)then
      
      
      if(Exp)then
      
      
      Nexp=Nexp*10+(i-1)
      else
      
      
      mantis=.TRUE.
      REAL=REAL*ten+float(i-1)
      
      
      if(Point)ndec=ndec+1
      endif
      elseif(k.EQ.0)then
      
      
      if(Point)goto 100
      Point=.TRUE.
      
      
      elseif(k.EQ.2)then
      
      
      if(j.NE.1)then
      isexp=-1
      if(.NOT.(expsgn))then
      expsgn=.TRUE.
      Exp=.TRUE.
      endif
      else
      sign=-one
      endif
      elseif(k.EQ.3.OR.k.EQ.4)then
      
      
      if(Exp)goto 100
      Exp=.TRUE.
      
      
      elseif(j.NE.1)then
      if(.NOT.(expsgn))then
      expsgn=.TRUE.
      Exp=.TRUE.
      endif
      endif
40    continue
      
      
      if(Exp.AND..NOT.mantis)REAL=one
      REAL=REAL*sign*(ten**(-ndec+isexp*Nexp))
      Next=.TRUE.
      return
      endif
      endif
      
      
100   ERROR=.TRUE.
      REAL=-ten
      if(End)REAL=ten
      return
      end
C* :1 * 
      
