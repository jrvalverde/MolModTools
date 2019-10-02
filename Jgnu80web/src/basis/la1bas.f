
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 la1bas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "la1bas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "la1bas.web"
      subroutine la1bas(MDIM,ITYPE,E,CS,CP,CD,IA,NCONT,NGAUSS,ISHT,ISHC)
      implicit none
      real*8 CD,CP,CS,E,sc1s,sc2sp
      integer i,IA,In,Iout,Ipunch,ISHC,ISHT,ITYPE,MDIM,NCONT,NGAUSS,ngst
     &o
      
      dimension E(*),CS(*),CP(*),CD(*),NGAUSS(*),ISHT(*),ISHC(*)
      
      
      
      dimension sc1s(10),sc2sp(10)
      common/io/In,Iout,Ipunch
      save ngsto,sc1s,sc2sp
      data ngsto/3/,sc1s/1.24D0,1.69D0,2.69D0,3.68D0,4.68D0,5.67D0,6.67D
     &0,7.66D0,8.65D0,9.64D0/,sc2sp/0.00D0,0.00D0,0.80D0,1.15D0,1.50D0,1
     &.72D0,1.95D0,2.25D0,2.55D0,2.88D0/
99001 format(' Atomic Number',i3,' not in LANL set.')
      
      call aclear(MDIM,E)
      call aclear(MDIM,CS)
      call aclear(MDIM,CP)
      call aclear(MDIM,CD)
      if((IA.GE.58.AND.IA.LE.71).OR.IA.GT.83)write(Iout,99001)IA
      if((IA.GE.58.AND.IA.LE.71).OR.IA.GT.83)call lnk1e
      
      if((ITYPE.EQ.1).OR.(ITYPE.EQ.3).OR.(IA.GE.11))then
      
      if(IA.GE.11)then
      
      
      
      if(IA.LE.18)then
      call la2nd(ITYPE,E,CS,CP,CD,IA,NCONT,NGAUSS,ISHT,ISHC)
      return
      elseif(IA.LE.36)then
      call la3rd(ITYPE,E,CS,CP,CD,IA,NCONT,NGAUSS,ISHT,ISHC)
      return
      elseif(IA.LE.54)then
      call la4th(ITYPE,E,CS,CP,CD,IA,NCONT,NGAUSS,ISHT,ISHC)
      return
      elseif(IA.LE.83)then
      call la5th(ITYPE,E,CS,CP,CD,IA,NCONT,NGAUSS,ISHT,ISHC)
      return
      endif
      elseif(ITYPE.EQ.3)then
      if(IA.GT.2)then
      call lpeone(E,CS,CP,IA)
      NCONT=2
      NGAUSS(1)=3
      NGAUSS(2)=1
      ISHT(1)=1
      ISHT(2)=1
      return
      else
      call ezero(E,CS,NGAUSS,1,IA)
      NCONT=2
      NGAUSS(1)=3
      NGAUSS(2)=1
      ISHT(1)=0
      ISHT(2)=0
      return
      endif
      else
      call d95v(E,CS,CP,IA,NCONT,NGAUSS,ISHT,ISHC)
      return
      endif
      elseif(ITYPE.EQ.2)then
      NCONT=1
      NGAUSS(1)=ngsto
      ISHT(1)=0
      if(IA.GE.3)ISHT(1)=1
      call s1s(E,CS,ngsto)
      if(IA.GE.3)call s2sp(E,CS,CP,ngsto)
      do 50 i=1,ngsto
      if(IA.GE.3)then
      E(i)=E(i)*sc2sp(IA)**2
      else
      E(i)=E(i)*sc1s(IA)**2
      endif
50    continue
      return
      else
      NCONT=1
      if(IA.GE.3)NCONT=2
      NGAUSS(1)=ngsto
      NGAUSS(2)=ngsto
      ISHT(2)=1
      ISHT(1)=0
      call s1s(E,CS,ngsto)
      if(IA.GE.3)call s2sp(E(ngsto+1),CS(ngsto+1),CP(ngsto+1),ngsto)
      do 100 i=1,ngsto
      E(i)=E(i)*sc1s(IA)**2
      if(IA.GT.2)E(i+ngsto)=E(i+ngsto)*sc2sp(IA)**2
100   continue
      return
      endif
      write(Iout,*)'Sorry, no basis for atoms heavier than Bismuth ',IA
      call lnk1e
      return
      end
C* :1 * 
      
