
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rediag"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rediag.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "rediag.web"
      subroutine rediag(DM,T,TPNAO,EVAL,BLK,C,IRANK,IRPNAO)
      implicit none
      double precision BLK,C,DM,EVAL,T,TPNAO
      integer iadd,Iatcr,Iatno,il,ilbl,imax,Ino,iorb,IRANK,IRPNAO,Ispin,
     &Iznuc,jorb,jorbl,Larc,Lbl,Ldeg,Ll,Lorb,Lorbc
      integer Lstemt,Lstocc,Lu,m,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Na
     &octr,Naol,Natoms,Nbas,nc,Ndim,nf,nl,nm,Norbs
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Ldeg(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MAX
     &BAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(MA
     &XBAS)
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),TPNAO(Ndim,Ndim),C(Mxaolm,Mxa
     &olm),EVAL(Ndim),BLK(Mxaolm,Mxaolm),IRANK(Nbas)
      
      
      
      if(IRPNAO.EQ.1)call fepnao(TPNAO)
      nf=0
      iorb=0
      nl=1
100   iorb=iorb+nl
      if(iorb.LE.Nbas)then
      nl=1
      ilbl=Naoctr(iorb)
      il=Naol(iorb)/100
      nm=il*2+1
      imax=Nbas-iorb
      do 150 iadd=1,imax
      jorb=iorb+iadd
      jorbl=Naol(jorb)/100
      if((Naoctr(jorb).NE.ilbl).OR.(jorbl.NE.il))goto 200
      nl=nl+1
150   continue
200   nc=nl/nm
      if(nc.EQ.1)then
      do 220 m=1,nm
      nf=nf+1
220   continue
      else
      call redblk(T,TPNAO,il,DM,BLK,EVAL,C,nf,iorb,nc,IRANK,IRPNAO)
      endif
      goto 100
      endif
      if(IRPNAO.EQ.0)return
      
      
      call svpnao(TPNAO)
      return
      end
C* :1 * 
      
