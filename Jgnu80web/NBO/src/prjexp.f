
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prjexp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prjexp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "prjexp.web"
      subroutine prjexp(BORB,IAT1,IAT2,IAT3,Q,P,PK,HYB,VA,VB,HYBEXP)
      implicit none
      double precision BORB,eps,HYB,HYBEXP,one,P,pav,PK,Q,s,VA,VB,zero
      integer i,i1,ia,iat,IAT1,IAT2,IAT3,Iatcr,Iathy,Iatno,Ibxm,Ill,Ino,
     &Ispin,Iul,Iznuc,j,k,kmax,kmin
      integer Label,Larc,Lstocc,MAXATM,MAXBAS,mj,Munit,Mxao,Mxaolm,Mxbo,
     &Naoctr,Naol,Natoms,Nbas,Ndim,nh,nl,Norbs,nu
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ill(MAXATM),
     &Iul(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension iat(3),HYB(Mxao),BORB(Mxbo),Q(Mxao,Ndim),P(Mxao,Mxao),PK
     &(Mxao,Mxao),VA(Mxao),VB(Mxao),HYBEXP(3)
      data zero,one,eps/0.0D0,1.0D0,1.0D-5/
      iat(1)=IAT1
      iat(2)=IAT2
      iat(3)=IAT3
      kmax=0
      do 100 i=1,3
      HYBEXP(i)=one
      ia=iat(i)
      nh=Ino(ia)
      if(ia.NE.0)then
      nu=Iul(ia)
      nl=Ill(ia)
      kmin=kmax+1
      kmax=kmax+nu-nl+1
      if(nh.NE.0)then
      mj=0
      do 10 k=kmin,kmax
      mj=mj+1
      HYB(mj)=BORB(k)
10    continue
      call frmpro(P,ia,Q,nh,PK,VA,VB)
      s=zero
      pav=zero
      do 20 i1=1,mj
      s=s+HYB(i1)**2
      do 15 j=1,mj
      pav=pav+HYB(i1)*P(i1,j)*HYB(j)
15    continue
20    continue
      if(s.GE.eps)HYBEXP(i)=dabs(pav)/s
      endif
      endif
100   continue
      return
      end
C* :1 * 
      
