
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 asgsym"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "asgsym.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "asgsym.web"
      subroutine asgsym(NBASIS,A,B,MDIM)
      implicit none
      double precision A,a0,B,Big,Crit,Degen,Dumscr,E,fmax,Four,Fuzzy,ga
     &bs,gmax1,gsqrt,One,Onept5,Pt5,S,Small,Span
      double precision Sthrs,Three,Two,Zero
      integer i,i1,ichk,Idscr,In,Ior,Iou,Iout,Ipunch,Isym,Itcnt,Iuo,j,Ke
     &y,Ksm,Kspin,Ksw,MDIM,minprt,NBASIS
      integer Nesk,Nest,Nest1,Nse,Nsep,nsym
      logical Cmp,Rhf,Skpsym
      dimension A(MDIM,*),B(MDIM,*)
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scr/S(140),Iou(70),Iuo(70),Ior(70,2),E(140),Key,Itcnt,Crit,
     &Skpsym,Idscr,Dumscr(5209)
      common/symmet/Isym(140)
      common/fuzzyf/Fuzzy,Degen,Sthrs,Span
      common/io/In,Iout,Ipunch
      
      
      
      call ilsw(2,21,minprt)
      
      do 100 i=1,NBASIS
      S(i)=A(i,i)
100   continue
      fmax=Zero
      
      
      if(Cmp)then
      
      do 150 i=2,NBASIS
      i1=i-1
      do 120 j=1,i1
      a0=gsqrt(A(j,i)**2+A(j,i+MDIM)**2)
      if(a0.LT.Fuzzy)then
      A(i,j)=Zero
      A(j,i)=Zero
      A(i,j+MDIM)=Zero
      A(j,i+MDIM)=Zero
      endif
      B(i,j)=a0
      B(j,i)=a0
      fmax=gmax1(fmax,a0)
120   continue
150   continue
      else
      do 200 i=2,NBASIS
      i1=i-1
      do 160 j=1,i1
      a0=gabs(A(j,i))
      if(a0.LT.Fuzzy)then
      A(i,j)=Zero
      A(j,i)=Zero
      endif
      B(i,j)=a0
      B(j,i)=a0
      fmax=gmax1(fmax,a0)
160   continue
200   continue
      endif
      if(fmax.LT.Sthrs)return
      
      do 300 i=1,NBASIS
      Iou(i)=0
300   continue
      
      nsym=0
      do 400 i=1,NBASIS
      ichk=0
      do 350 j=1,NBASIS
      if(gabs(S(i)-S(j)).GT.Degen.AND.B(j,i).GT.Span)then
      if(Iou(j)+Iou(i).NE.0)then
      
      if(ichk.EQ.0)ichk=Iou(j)
      if(Iou(j).EQ.0)Iou(j)=ichk
      if(ichk.NE.Iou(j))goto 900
      Iou(i)=ichk
      else
      nsym=nsym+1
      Iou(i)=nsym
      Iou(j)=nsym
      ichk=nsym
      endif
      endif
350   continue
400   continue
      
      do 500 i=1,NBASIS
      do 450 j=i,NBASIS
      if(Iou(j).EQ.0)then
      if(gabs(S(i)-S(j)).LE.Degen)Iou(j)=Iou(i)
      endif
450   continue
500   continue
      
      do 600 i=1,NBASIS
      if(Iou(i).LE.0)then
      nsym=nsym+1
      Iou(i)=nsym
      endif
600   continue
      
      if(.NOT.(Skpsym))then
      do 650 i=1,NBASIS
      if(Iou(i).NE.Isym(i+Nest))goto 700
650   continue
      if(minprt.NE.0)return
      return
      endif
      
700   do 800 i=1,NBASIS
      Isym(i+Nest)=Iou(i)
800   continue
      return
      
900   do 1000 i=1,NBASIS
      Isym(i+Nest)=0
1000  continue
      return
      
      end
C* :1 * 
      
