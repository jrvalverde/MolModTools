
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 load"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "load.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "load.web"
      subroutine load(DM,IAT1,IAT2,IAT3,BLK,NB)
      implicit none
      double precision BLK,DM,zero
      integer i,ia,iat,IAT1,IAT2,IAT3,Iatcr,Iatno,icol,il,Ill,Ino,irow,I
     &spin,iu,Iul,Iznuc,j,ja,jl
      integer ju,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,NB,Nbas,nco
     &l,Ndim,Norbs,nrow
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ill(MAXATM),
     &Iul(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension BLK(Mxbo,Mxbo),DM(Ndim,Ndim),iat(3)
      data zero/0.0D0/
      iat(1)=IAT1
      iat(2)=IAT2
      iat(3)=IAT3
      do 100 i=1,Mxbo
      do 50 j=1,Mxbo
      BLK(i,j)=zero
50    continue
100   continue
      nrow=0
      ncol=0
      do 200 i=1,3
      ia=iat(i)
      if(ia.NE.0)then
      iu=Iul(ia)
      il=Ill(ia)
      do 120 irow=il,iu
      nrow=nrow+1
      ncol=0
      do 110 j=1,3
      ja=iat(j)
      if(ja.NE.0)then
      ju=Iul(ja)
      jl=Ill(ja)
      do 102 icol=jl,ju
      ncol=ncol+1
      BLK(nrow,ncol)=DM(irow,icol)
102   continue
      endif
110   continue
120   continue
      endif
200   continue
      NB=nrow
      return
      end
C* :1 * 
      
