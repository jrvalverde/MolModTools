
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aufbau"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aufbau.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "aufbau.web"
      subroutine aufbau(NBASIS,B,MDIM)
      implicit none
      double precision a0,a1,B,Big,Dumscr,E,Four,One,Onept5,Pt5,S,Small,
     &Three,Two,Zero
      integer i,im,Ior,Iou,iov,ism,Isym,Iuo,j,jj,jm,k,Ksm,Kspin,Ksw,MDIM
     &,n1,n2,NBASIS,Nesk
      integer Nest,Nest1,Nse,Nsep
      logical Cmp,Rhf
      dimension B(MDIM,*)
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scr/S(140),Iou(70),Iuo(70),Ior(70,2),E(140),Dumscr(5212)
      common/symmet/Isym(140)
      
      
      n1=1
      n2=Nse
      do 100 iov=1,2
      do 50 i=n1,n2
      a0=Big
      do 20 j=i,n2
      a1=E(j+Nest)
      if(a1.LT.a0)then
      a0=a1
      jj=j
      endif
20    continue
      if(i.NE.jj)then
      E(jj+Nest)=E(i+Nest)
      E(i+Nest)=a0
      ism=Isym(jj+Nest)
      Isym(jj+Nest)=Isym(i+Nest)
      Isym(i+Nest)=ism
      if(Cmp)then
      im=i+MDIM
      jm=jj+MDIM
      do 25 k=1,NBASIS
      a0=B(k,jm)
      B(k,jm)=B(k,im)
      B(k,im)=a0
25    continue
      endif
      do 30 k=1,NBASIS
      a0=B(k,jj)
      B(k,jj)=B(k,i)
      B(k,i)=a0
30    continue
      endif
50    continue
      n1=Nse+1
      n2=NBASIS
100   continue
      return
      
      end
C* :1 * 
      
