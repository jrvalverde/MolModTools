
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lblnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lblnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "lblnbo.web"
      subroutine lblnbo
      implicit none
      integer i,Iaolbl,Iatcr,Iatno,ib,iblnk,Ibxm,ic,icr,ihyp,il,ileft,il
     &p,inbo,Ino,ip,ir,iright,Ispin,istar
      integer iy,Iznuc,Label,Larc,lbl,Lbl1,Ll,Lorb,Lorbc,Lstocc,Lu,MAXAT
     &M,MAXBAS,MAXD,Munit,Mxao,Mxaolm,Mxbo,nameat,Naolbl
      integer Natoms,Nbas,Nbolbl,Nbotyp,Nbouni,nctr,nd,Ndim,Nholbl,Nlew,
     &Norbs,Nval
      parameter(MAXD=2)
      integer istr(MAXD),ibyte(4)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl1(MAXBAS),Lorbc(MAXBAS),Lorb(
     &MAXBAS)
      common/nblbl/Nlew,Nval,Iaolbl(10,MAXBAS),Naolbl(10,MAXBAS),Nholbl(
     &10,MAXBAS),Nbolbl(10,MAXBAS)
      
      data iblnk,ic,il,ip,ir,iy,istar,ihyp/' ','c','l','p','r','y','*','
     &-'/
      data icr,ilp/'CR','LP'/
      data ileft,iright/'(',')'/
      
      do 100 inbo=1,Nbas
      do 50 i=1,10
      Nbolbl(i,inbo)=iblnk
50    continue
      ib=Ibxm(inbo)
      nctr=1
      if(Label(ib,5).NE.0)nctr=2
      if(Label(ib,6).NE.0)nctr=3
      
      
      if(nctr.EQ.1)then
      lbl=nameat(Iatno(Label(ib,4)))
      call debyte(lbl,ibyte)
      Nbolbl(1,inbo)=ibyte(1)
      Nbolbl(2,inbo)=ibyte(2)
      call idigit(Label(ib,4),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nbolbl(4,inbo)=istr(1)
      else
      Nbolbl(3,inbo)=istr(1)
      Nbolbl(4,inbo)=istr(2)
      endif
      Nbolbl(5,inbo)=ileft
      if(Label(ib,1).EQ.icr)then
      Nbolbl(6,inbo)=ic
      Nbolbl(7,inbo)=ir
      Nbolbl(8,inbo)=iright
      elseif(Label(ib,1).EQ.ilp)then
      Nbolbl(6,inbo)=il
      Nbolbl(7,inbo)=ip
      if(Label(ib,2).EQ.istar)then
      Nbolbl(8,inbo)=istar
      Nbolbl(9,inbo)=iright
      else
      Nbolbl(8,inbo)=iright
      endif
      else
      Nbolbl(6,inbo)=ir
      Nbolbl(7,inbo)=iy
      Nbolbl(8,inbo)=istar
      Nbolbl(9,inbo)=iright
      endif
      
      
      elseif(nctr.EQ.2)then
      lbl=nameat(Iatno(Label(ib,4)))
      call debyte(lbl,ibyte)
      Nbolbl(1,inbo)=ibyte(1)
      Nbolbl(2,inbo)=ibyte(2)
      call idigit(Label(ib,4),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nbolbl(4,inbo)=istr(1)
      else
      Nbolbl(3,inbo)=istr(1)
      Nbolbl(4,inbo)=istr(2)
      endif
      Nbolbl(5,inbo)=ihyp
      lbl=nameat(Iatno(Label(ib,5)))
      call debyte(lbl,ibyte)
      Nbolbl(6,inbo)=ibyte(1)
      Nbolbl(7,inbo)=ibyte(2)
      call idigit(Label(ib,5),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nbolbl(9,inbo)=istr(1)
      else
      Nbolbl(8,inbo)=istr(1)
      Nbolbl(9,inbo)=istr(2)
      endif
      Nbolbl(10,inbo)=Label(ib,2)
      
      
      else
      call idigit(Label(ib,4),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nbolbl(2,inbo)=istr(1)
      else
      Nbolbl(1,inbo)=istr(1)
      Nbolbl(2,inbo)=istr(2)
      endif
      Nbolbl(3,inbo)=ihyp
      call idigit(Label(ib,5),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nbolbl(5,inbo)=istr(1)
      else
      Nbolbl(4,inbo)=istr(1)
      Nbolbl(5,inbo)=istr(2)
      endif
      Nbolbl(6,inbo)=ihyp
      call idigit(Label(ib,6),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nbolbl(8,inbo)=istr(1)
      else
      Nbolbl(7,inbo)=istr(1)
      Nbolbl(8,inbo)=istr(2)
      endif
      Nbolbl(9,inbo)=Label(ib,2)
      endif
100   continue
      return
      end
C* :1 * 
      
