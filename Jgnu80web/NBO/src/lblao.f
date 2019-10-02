
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lblao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lblao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "lblao.web"
      subroutine lblao
      implicit none
      integer i,iao,Iaolbl,Iatcr,Iatno,iblnk,ileft,Ino,iright,Ispin,Iznu
     &c,l,Lang,lbl,Lctr,Ll,Lu,m,MAXATM,MAXBAS
      integer MAXD,Munit,Mxao,Mxaolm,Mxbo,nameat,Naolbl,Natoms,Nbas,Nbol
     &bl,nd,Ndim,Nholbl,Nlew,Norbs,Nval
      parameter(MAXD=2)
      integer istr(MAXD),iang(5),ixyz(3),ibyte(4)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbao/Lctr(MAXBAS),Lang(MAXBAS)
      common/nblbl/Nlew,Nval,Iaolbl(10,MAXBAS),Naolbl(10,MAXBAS),Nholbl(
     &10,MAXBAS),Nbolbl(10,MAXBAS)
      
      data iblnk/' '/
      data iang/'s','p','d','f','g'/
      data ixyz/'x','y','z'/
      data ileft,iright/'(',')'/
      
      do 100 iao=1,Nbas
      do 50 i=1,10
      Iaolbl(i,iao)=iblnk
50    continue
      lbl=nameat(Iatno(Lctr(iao)))
      call debyte(lbl,ibyte)
      Iaolbl(1,iao)=ibyte(1)
      Iaolbl(2,iao)=ibyte(2)
      call idigit(Lctr(iao),istr,nd,MAXD)
      if(nd.EQ.1)then
      Iaolbl(4,iao)=istr(1)
      else
      Iaolbl(3,iao)=istr(1)
      Iaolbl(4,iao)=istr(2)
      endif
      Iaolbl(6,iao)=ileft
      l=Lang(iao)/100
      Iaolbl(7,iao)=iang(l+1)
      if(l.EQ.0)then
      Iaolbl(8,iao)=iright
      elseif(l.EQ.1)then
      m=mod(Lang(iao),10)
      Iaolbl(8,iao)=ixyz(m)
      Iaolbl(9,iao)=iright
      elseif(l.EQ.2.OR.l.EQ.3)then
      Iaolbl(8,iao)=mod(Lang(iao),10)+48
      Iaolbl(9,iao)=iright
      endif
100   continue
      return
      end
C* :1 * 
      
