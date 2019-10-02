
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repol"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repol.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "repol.web"
      subroutine repol(DM,Q,POL,BLK,EVAL,C,NBD)
      implicit none
      double precision BLK,C,cri,csj,dij,DM,EVAL,POL,Q,zero
      integer i,ia,iab,Iatcr,Iathy,Iatno,ib,Ibxm,Ill,Ino,ir,irp,Ispin,Iu
     &l,Iznuc,j,ja,js,jsp,Label
      integer Larc,lstar,Lstocc,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,n3c
     &b,Naoctr,Naol,Natoms,Nbas,NBD,nbond,nctr,Ndim,nhi,nhj
      integer Norbs
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ill(MAXATM),
     &Iul(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension DM(Ndim,Ndim),Q(Mxao,Ndim),POL(Ndim,3),BLK(Mxbo,Mxbo),EV
     &AL(Mxbo),C(Mxbo,Mxbo)
      data zero/0.0D0/
      data lstar/1H*/
      
      
      nbond=0
      n3cb=0
      do 100 ib=1,Nbas
      if(Label(ib,2).NE.lstar)then
      if(Label(ib,5).NE.0)then
      nbond=nbond+1
      if(Label(ib,6).NE.0)n3cb=n3cb+1
      endif
      endif
100   continue
      
      iab=Nbas-nbond-n3cb
      
      do 200 ib=1,NBD
      if(Label(ib,2).NE.lstar)then
      nctr=1
      if(Label(ib,5).GT.0)nctr=2
      if(Label(ib,6).GT.0)nctr=3
      if(nctr.NE.1)then
      do 110 i=1,nctr
      ia=Label(ib,i+3)
      nhi=Norbs(ia)
      do 105 j=1,i
      ja=Label(ib,j+3)
      nhj=Norbs(ja)
      dij=zero
      do 104 ir=1,nhi
      irp=Ill(ia)+ir-1
      cri=Q(ir,Ill(ia)+Iathy(ib,i)-1)
      do 102 js=1,nhj
      jsp=Ill(ja)+js-1
      csj=Q(js,Ill(ja)+Iathy(ib,j)-1)
      dij=dij+cri*csj*DM(irp,jsp)
102   continue
104   continue
      BLK(i,j)=dij
      BLK(j,i)=dij
105   continue
110   continue
      call jacobi(nctr,BLK,EVAL,C,Mxbo,Mxbo,0)
      call rank(EVAL,nctr,Mxbo,Larc)
      do 120 i=1,nctr
      POL(ib,i)=C(i,Larc(1))
120   continue
      iab=iab+1
      do 130 i=1,nctr
      POL(iab,i)=C(i,Larc(2))
130   continue
      if(nctr.EQ.3)then
      iab=iab+1
      do 135 i=1,nctr
      POL(iab,i)=C(i,Larc(3))
135   continue
      endif
      endif
      endif
200   continue
      return
      end
C* :1 * 
      
