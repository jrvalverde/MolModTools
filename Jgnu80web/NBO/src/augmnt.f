
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 augmnt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "augmnt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "augmnt.web"
      subroutine augmnt(P,BLK,C,EVAL,DM,TA,BORB,V,LARC,IA,NOCC,NORB)
      implicit none
      double precision b,BLK,BORB,C,diff,DM,eps,EVAL,one,P,prjmax,proj,p
     &t99,rnorm,sb,sum,TA,temp,V,zero
      integer i,i1,IA,iao,iaug,ib,imax,iproj,Ispin,itcol,itemp,j,jj,k,l,
     &LARC,lj,Munit,Mxao,Mxaolm
      integer Mxbo,Natoms,naug,naug1,Nbas,Ndim,NOCC,NORB
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension P(Mxao,Mxao),TA(Mxao,Mxao),DM(Ndim,Ndim),C(Mxbo,Mxbo),EV
     &AL(Mxbo),BORB(Mxbo),V(Mxbo),BLK(Mxbo,Mxbo),LARC(Nbas)
      data zero,eps,pt99,one/0.0D0,1.0D-5,0.99D0,1.0D0/
      naug=NORB-NOCC
      
      
      do 100 i=1,NORB
      LARC(i)=0
100   continue
      do 500 iproj=1,naug
      imax=0
      prjmax=zero
      do 150 iao=1,NORB
      if(LARC(iao).EQ.0)then
      proj=abs(P(iao,iao))
      if(proj.GT.pt99)goto 200
      if(proj.GE.prjmax)then
      prjmax=proj
      imax=iao
      endif
      endif
150   continue
      iao=imax
      proj=prjmax
200   sb=zero
      do 250 j=1,NORB
      b=P(iao,j)
      sb=sb+b*b
      BORB(j)=b
250   continue
      LARC(iao)=iproj
      rnorm=one/sqrt(sb)
      do 300 j=1,NORB
      BORB(j)=BORB(j)*rnorm
300   continue
      do 350 j=1,NORB
      C(j,iproj)=BORB(j)
350   continue
      if(iproj.NE.naug)then
      do 380 j=1,NORB
      do 360 i=1,j
      TA(i,j)=-BORB(i)*BORB(j)
      TA(j,i)=TA(i,j)
      if(i.EQ.j)TA(i,i)=TA(i,i)+one
360   continue
380   continue
      do 420 i=1,NORB
      do 390 j=1,NORB
      V(j)=zero
      do 385 l=1,NORB
      V(j)=V(j)+P(i,l)*TA(l,j)
385   continue
390   continue
      do 400 j=1,NORB
      P(i,j)=V(j)
400   continue
420   continue
      endif
500   continue
      iaug=0
      do 600 iao=1,NORB
      if(LARC(iao).NE.0)then
      iaug=iaug+1
      itcol=LARC(iao)
      do 520 j=1,NORB
      TA(j,iaug)=C(j,itcol)
520   continue
      endif
600   continue
      call load(DM,IA,0,0,BLK,NORB)
      do 800 ib=1,NORB
      do 650 j=1,naug
      sum=zero
      do 620 k=1,NORB
      sum=sum+BLK(ib,k)*TA(k,j)
620   continue
      V(j)=sum
650   continue
      do 700 j=1,naug
      BLK(ib,j)=V(j)
700   continue
800   continue
      do 1000 j=1,naug
      do 850 i=1,j
      sum=zero
      do 820 k=1,NORB
      sum=sum+TA(k,i)*BLK(k,j)
820   continue
      V(i)=sum
850   continue
      do 900 i=1,naug
      BLK(i,j)=V(i)
900   continue
1000  continue
      do 1100 j=1,naug
      jj=j-1
      do 1050 i=1,jj
      BLK(j,i)=BLK(i,j)
1050  continue
1100  continue
      call jacobi(naug,BLK,EVAL,C,Mxbo,Mxbo,1)
      do 1200 i=1,naug
      LARC(i)=i
1200  continue
      naug1=naug-1
      do 1300 i=1,naug1
      i1=i+1
      do 1250 j=i1,naug
      diff=EVAL(j)-EVAL(i)
      if(diff.GE.eps)then
      temp=EVAL(i)
      EVAL(i)=EVAL(j)
      EVAL(j)=temp
      itemp=LARC(i)
      LARC(i)=LARC(j)
      LARC(j)=itemp
      endif
1250  continue
1300  continue
      do 1400 j=1,naug
      lj=LARC(j)
      do 1350 i=1,NORB
      sum=zero
      do 1320 k=1,naug
      sum=sum+TA(i,k)*C(k,lj)
1320  continue
      BLK(i,j)=sum
1350  continue
1400  continue
      return
      end
C* :1 * 
      
