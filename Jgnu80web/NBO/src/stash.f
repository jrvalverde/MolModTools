
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 stash"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "stash.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "stash.web"
      subroutine stash(BORB,IBD,IAT1,IAT2,IAT3,POL,Q,HYB)
      implicit none
      double precision BORB,HYB,p,POL,psq,Q,zero
      integer i,ia,iat,IAT1,IAT2,IAT3,Iatcr,Iathy,Iatno,IBD,Ibxm,Ill,Ino
     &,Ispin,Iul,Iznuc,j,k,kmax,kmin
      integer Label,Larc,Lstocc,MAXATM,MAXBAS,mj,Munit,Mxao,Mxaolm,Mxbo,
     &Naoctr,Naol,Natoms,Nbas,ncol,Ndim,nh,nl,Norbs,nrow
      integer nu
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ill(MAXATM),
     &Iul(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension POL(Ndim,3),Q(Mxao,Ndim),BORB(Mxbo),iat(3),HYB(Mxao)
      data zero/0.0D0/
      iat(1)=IAT1
      iat(2)=IAT2
      iat(3)=IAT3
      kmax=0
      do 100 i=1,3
      ia=iat(i)
      if(ia.NE.0)then
      nu=Iul(ia)
      nl=Ill(ia)
      kmin=kmax+1
      kmax=kmax+nu-nl+1
      mj=0
      do 20 k=kmin,kmax
      mj=mj+1
      HYB(mj)=BORB(k)
20    continue
      psq=zero
      do 40 j=1,mj
      psq=psq+HYB(j)**2
40    continue
      p=dsqrt(psq)
      POL(IBD,i)=p
      Ino(ia)=Ino(ia)+1
      ncol=Ill(ia)+Ino(ia)-1
      nh=nu-nl+1
      do 60 nrow=1,nh
      if(p.EQ.zero)then
      Q(nrow,ncol)=zero
      else
      Q(nrow,ncol)=HYB(nrow)/p
      endif
60    continue
      Iathy(IBD,i)=Ino(ia)
      endif
100   continue
      return
      end
C* :1 * 
      
