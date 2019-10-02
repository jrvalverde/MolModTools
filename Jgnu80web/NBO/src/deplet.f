
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 deplet"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "deplet.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "deplet.web"
      subroutine deplet(DM,T,Q,POL,BORB,BNDOCC,NBD)
      implicit none
      double precision BNDOCC,BORB,DM,occ,p,POL,Q,T
      integer i,ia,iat,Iatcr,Iathy,Iatno,ibd,Ibxm,icol,ictr,ih,ihyb,il,I
     &ll,Ino,irow,Ispin,iu,Iul,Iznuc
      integer j,ja,jctr,jl,ju,Label,Larc,Lstocc,MAXATM,MAXBAS,Munit,Mxao
     &,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nbas,NBD,ncol
      integer nctr,Ndim,nelm,nh,Norbs,nrow
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ill(MAXATM),
     &Iul(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),Q(Mxao,Ndim),POL(Ndim,3),BORB
     &(Mxbo),BNDOCC(Ndim)
      dimension iat(3)
      do 100 j=1,Nbas
      do 50 i=1,j
      DM(i,j)=T(i,j)
      DM(j,i)=DM(i,j)
50    continue
100   continue
      do 400 ibd=1,NBD
      occ=BNDOCC(ibd)
      nctr=0
      do 150 j=1,3
      iat(j)=Label(ibd,j+3)
      if(iat(j).LE.0)goto 200
      nctr=nctr+1
150   continue
200   nelm=0
      do 250 ictr=1,nctr
      ia=iat(ictr)
      ihyb=Iathy(ibd,ictr)+Ill(ia)-1
      p=POL(ibd,ictr)
      nh=Norbs(ia)
      do 220 ih=1,nh
      nelm=nelm+1
      BORB(nelm)=p*Q(ih,ihyb)
220   continue
250   continue
      nrow=0
      do 300 ictr=1,nctr
      ia=iat(ictr)
      iu=Iul(ia)
      il=Ill(ia)
      do 280 irow=il,iu
      nrow=nrow+1
      ncol=0
      do 260 jctr=1,nctr
      ja=iat(jctr)
      ju=Iul(ja)
      jl=Ill(ja)
      do 255 icol=jl,ju
      ncol=ncol+1
      DM(irow,icol)=DM(irow,icol)-occ*BORB(nrow)*BORB(ncol)
255   continue
260   continue
280   continue
300   continue
400   continue
      return
      end
C* :1 * 
      
