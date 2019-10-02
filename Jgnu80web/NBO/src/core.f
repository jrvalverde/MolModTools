
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 core"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "core.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "core.web"
      
      
      
      subroutine core(DM,T,BORB,POL,Q,HYB,BNDOCC,IBD,DETAIL,LFNPR)
      implicit none
      double precision BNDOCC,BORB,DM,HYB,occ,one,POL,Q,T,zero
      integer i,iac,iang,iat,Iatcr,Iatno,IBD,iblk,Ibxm,ichcor,ichval,ico
     &r,icore,iecp,il,Ill,Ino,Iprin,iryd,Ispin
      integer ityp,Iul,ival,Iznuc,l,la,Label,Larc,Lbl,LFNPR,lm,lnum,Lorb
     &,Lorbc,Lstocc,Ltyp,m,MAXATM,MAXBAS,morb
      integer Munit,Mxao,Mxaolm,Mxbo,n,na,nameat,Naoctr,Naol,Natoms,nb,N
     &bas,Nbotyp,Nbouni,nctr,Ndim,norb,Norbs
      
      
      logical DETAIL,first
      parameter(MAXATM=99,MAXBAS=500)
      common/nbnao/Naoctr(MAXBAS),Naol(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS
     &)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ill(MAXATM),
     &Iul(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),BORB(Mxbo),POL(Ndim,3),Q(Mxao
     &,Ndim),HYB(Mxao),BNDOCC(Ndim),icore(4),ival(4),iang(5)
      data zero,one/0.0D0,1.0D0/
      data iblk,icor,iryd/'  ','CR','Ryd'/
      data ichcor,ichval/'Cor','Val'/
      data iang/'s','p','d','f','g'/
      
      
      do 100 i=1,Nbas
      Ltyp(i)=iryd
100   continue
      iecp=0
      do 200 nctr=1,Natoms
      call cortbl(nctr,icore,iecp)
      call valtbl(nctr,ival)
      do 150 l=0,3
      ityp=iang(l+1)
      lnum=2*l+1
      if(icore(l+1).GT.0)then
      do 110 m=1,icore(l+1)
      do 105 la=1,lnum
      morb=0
      occ=-1.0
      do 102 n=1,Nbas
      lm=Naol(n)
      norb=lm/100
      il=iang(norb+1)
      na=mod(Naol(n),50)
      if(Naoctr(n).EQ.nctr.AND.il.EQ.ityp.AND.DM(n,n).GT.occ.AND.Ltyp(n)
     &.EQ.iryd.AND.la.EQ.na)then
      morb=n
      occ=DM(n,n)
      endif
102   continue
      if(morb.EQ.0)then
      write(LFNPR,99005)ityp,nameat(Iatno(nctr)),nctr,(icore(i),i=1,4),m
     &,la
      stop
      endif
      Ltyp(morb)=ichcor
105   continue
110   continue
      endif
      if(ival(l+1).GT.0)then
      do 120 m=1,ival(l+1)
      do 115 la=1,lnum
      morb=0
      occ=-1.0
      do 112 n=1,Nbas
      lm=Naol(n)
      norb=lm/100
      il=iang(norb+1)
      na=mod(Naol(n),50)
      if(Naoctr(n).EQ.nctr.AND.il.EQ.ityp.AND.DM(n,n).GT.occ.AND.Ltyp(n)
     &.EQ.iryd.AND.la.EQ.na)then
      morb=n
      occ=DM(n,n)
      endif
112   continue
      if(morb.EQ.0)then
      write(LFNPR,99006)ityp,nameat(Iatno(nctr)),nctr,(ival(i),i=1,4),m,
     &la
      stop
      endif
      Ltyp(morb)=ichval
115   continue
120   continue
      endif
150   continue
200   continue
      
      
      do 300 iat=1,Natoms
      nb=Iul(iat)-Ill(iat)+1
      iac=0
      first=.TRUE.
      do 250 n=Ill(iat),Iul(iat)
      if(Ltyp(n).EQ.ichcor)then
      if(DETAIL.AND.first)then
      first=.FALSE.
      write(LFNPR,99001)iat
      endif
      iac=iac+1
      IBD=IBD+1
      do 210 i=1,nb
      BORB(i)=zero
210   continue
      BORB(n-Ill(iat)+1)=one
      call stash(BORB,IBD,iat,0,0,POL,Q,HYB)
      Label(IBD,1)=icor
      Label(IBD,2)=iblk
      Label(IBD,3)=iac
      Label(IBD,4)=iat
      BNDOCC(IBD)=DM(n,n)
      if(DETAIL)write(LFNPR,99002)iac,BNDOCC(IBD)
      if(DETAIL)write(LFNPR,99003)(BORB(i),i=1,nb)
      if(DETAIL)write(LFNPR,99004)IBD,(Label(IBD,i),i=1,3)
      endif
250   continue
300   continue
      
      
      call deplet(DM,T,Q,POL,BORB,BNDOCC,IBD)
      return
      
99001 format(/,1x,'Search of DM block for core orbitals on atom:',i4)
99002 format(6x,'Eigenvector (',i2,') has occupancy ',f9.6,':')
99003 format(11x,8F7.4)
99004 format(11x,'*** NBO accepted: Number',i3,'.   Label:',a2,a1,'(',i2
     &,')')
99005 format(/1x,'Subroutine CORE could not find a ',a1,'-type ','core o
     &rbital on atom ',a2,i2,'.',/,1x,'ICORE :',4I3,'     M :',i3,'     
     &LA :',i3)
99006 format(/1x,'Subroutine CORE could not find a ',a1,'-type ','valenc
     &e orbital on atom ',a2,i2,'.',/,1x,'IVAL :',4I3,'     M :',i3,'   
     &  LA :',i3)
      end
C* :1 * 
      
