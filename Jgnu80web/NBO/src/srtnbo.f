
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 srtnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "srtnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "srtnbo.web"
      subroutine srtnbo(T,BNDOCC)
      implicit none
      double precision BNDOCC,T,temp
      integer i,Iathy,Ibxm,Ichoos,icnt,ii,Iprint,Ipseud,Ispin,itemp,Iw3c
     &,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab
      integer Iwtnao,Iwtnbo,j,Jcore,Jprint,k,Kopt,l,l3c,Label,Larc,lbd,l
     &bl1,lbl2,lbl3,lbl4,lbl5,lbl6,lstar,Lstocc
      integer m,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,name,Natoms,Nbas,Nb
     &otyp,Nbouni,nctr,Ndim
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      
      dimension T(Ndim,Ndim),BNDOCC(Ndim)
      dimension name(3)
      
      data lbd,l3c,name,lstar/'BD','3C','CR','LP','RY','*'/
      
      
      
      do 300 i=1,Nbas
      nctr=0
      do 50 j=4,6
      if(Label(i,j).NE.0)then
      nctr=nctr+1
      Larc(nctr)=Label(i,j)
      endif
50    continue
      do 100 j=1,nctr-1
      do 60 k=1,nctr-j
      if(Larc(k).GT.Larc(k+1))then
      itemp=Larc(k)
      Larc(k)=Larc(k+1)
      Larc(k+1)=itemp
      endif
60    continue
100   continue
      do 150 j=1,nctr
      Label(i,j+3)=Larc(j)
150   continue
      do 200 j=nctr+1,3
      Label(i,j+3)=0
200   continue
300   continue
      
      
      icnt=0
      if(Jprint(10).EQ.0)then
      do 350 i=1,Natoms-1
      do 320 j=i+1,Natoms
      if(i.NE.j)then
      k=-1
305   k=k+1
      do 310 l=icnt+1,Nbas
      lbl1=Label(Ibxm(l),1)
      lbl2=Label(Ibxm(l),2)
      lbl3=Label(Ibxm(l),3)
      lbl4=Label(Ibxm(l),4)
      lbl5=Label(Ibxm(l),5)
      lbl6=Label(Ibxm(l),6)
      if((lbl1.EQ.lbd.OR.lbl1.EQ.l3c).AND.lbl2.NE.lstar)then
      if(lbl4.EQ.i.AND.lbl5.EQ.j.AND.lbl6.EQ.k)then
      icnt=icnt+1
      Label(Ibxm(l),1)=Label(Ibxm(icnt),1)
      Label(Ibxm(l),2)=Label(Ibxm(icnt),2)
      Label(Ibxm(l),3)=Label(Ibxm(icnt),3)
      Label(Ibxm(l),4)=Label(Ibxm(icnt),4)
      Label(Ibxm(l),5)=Label(Ibxm(icnt),5)
      Label(Ibxm(l),6)=Label(Ibxm(icnt),6)
      Label(Ibxm(icnt),1)=lbl1
      Label(Ibxm(icnt),2)=lbl2
      Label(Ibxm(icnt),3)=lbl3
      Label(Ibxm(icnt),4)=lbl4
      Label(Ibxm(icnt),5)=lbl5
      Label(Ibxm(icnt),6)=lbl6
      temp=BNDOCC(l)
      BNDOCC(l)=BNDOCC(icnt)
      BNDOCC(icnt)=temp
      do 306 m=1,Nbas
      temp=T(m,l)
      T(m,l)=T(m,icnt)
      T(m,icnt)=temp
306   continue
      endif
      endif
310   continue
      if(Iw3c.NE.0.AND.k.EQ.0)k=j
      if(k.GT.0.AND.k.LT.Natoms)goto 305
      endif
320   continue
350   continue
      endif
      
      
      do 500 ii=1,3
      do 400 i=1,Natoms
      do 380 j=icnt+1,Nbas
      lbl1=Label(Ibxm(j),1)
      lbl4=Label(Ibxm(j),4)
      if(lbl1.EQ.name(ii).AND.lbl4.EQ.i)then
      icnt=icnt+1
      do 355 k=1,6
      itemp=Label(Ibxm(j),k)
      Label(Ibxm(j),k)=Label(Ibxm(icnt),k)
      Label(Ibxm(icnt),k)=itemp
355   continue
      temp=BNDOCC(j)
      BNDOCC(j)=BNDOCC(icnt)
      BNDOCC(icnt)=temp
      do 360 k=1,Nbas
      temp=T(k,j)
      T(k,j)=T(k,icnt)
      T(k,icnt)=temp
360   continue
      endif
380   continue
400   continue
500   continue
      
      
      if(Jprint(10).EQ.0)then
      do 550 i=1,Natoms-1
      do 520 j=i+1,Natoms
      if(i.NE.j)then
      k=-1
      if(Iw3c.NE.0)k=j
505   k=k+1
      do 510 l=icnt+1,Nbas
      lbl1=Label(Ibxm(l),1)
      lbl2=Label(Ibxm(l),2)
      lbl3=Label(Ibxm(l),3)
      lbl4=Label(Ibxm(l),4)
      lbl5=Label(Ibxm(l),5)
      lbl6=Label(Ibxm(l),6)
      if((lbl1.EQ.lbd.OR.lbl1.EQ.l3c).AND.lbl2.EQ.lstar)then
      if(lbl4.EQ.i.AND.lbl5.EQ.j.AND.lbl6.EQ.k)then
      icnt=icnt+1
      Label(Ibxm(l),1)=Label(Ibxm(icnt),1)
      Label(Ibxm(l),2)=Label(Ibxm(icnt),2)
      Label(Ibxm(l),3)=Label(Ibxm(icnt),3)
      Label(Ibxm(l),4)=Label(Ibxm(icnt),4)
      Label(Ibxm(l),5)=Label(Ibxm(icnt),5)
      Label(Ibxm(l),6)=Label(Ibxm(icnt),6)
      Label(Ibxm(icnt),1)=lbl1
      Label(Ibxm(icnt),2)=lbl2
      Label(Ibxm(icnt),3)=lbl3
      Label(Ibxm(icnt),4)=lbl4
      Label(Ibxm(icnt),5)=lbl5
      Label(Ibxm(icnt),6)=lbl6
      temp=BNDOCC(l)
      BNDOCC(l)=BNDOCC(icnt)
      BNDOCC(icnt)=temp
      do 506 m=1,Nbas
      temp=T(m,l)
      T(m,l)=T(m,icnt)
      T(m,icnt)=temp
506   continue
      endif
      endif
510   continue
      if(k.GT.0.AND.k.LT.Natoms)goto 505
      endif
520   continue
550   continue
      endif
      return
      end
C* :1 * 
      
