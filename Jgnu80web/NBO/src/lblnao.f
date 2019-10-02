
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lblnao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lblnao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "lblnao.web"
      subroutine lblnao
      implicit none
      integer i,Iaolbl,Iatcr,Iatno,iblnk,ileft,inao,Ino,Iprin,iright,Isp
     &in,Iznuc,l,lbl,Ll,Ltyp,Lu,m,MAXATM,MAXBAS
      integer MAXD,Munit,Mxao,Mxaolm,Mxbo,nameat,Naoctr,Naol,Naolbl,Nato
     &ms,Nbas,Nbolbl,nd,Ndim,Nholbl,Nlew,Norbs,Nval
      parameter(MAXD=2)
      integer istr(MAXD),iang(5),ixyz(3),ibyte(4)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbnao/Naoctr(MAXBAS),Naol(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS
     &)
      common/nblbl/Nlew,Nval,Iaolbl(10,MAXBAS),Naolbl(10,MAXBAS),Nholbl(
     &10,MAXBAS),Nbolbl(10,MAXBAS)
      
      data iblnk/' '/
      data iang/'s','p','d','f','g'/
      data ixyz/'x','y','z'/
      data ileft,iright/'(',')'/
      
      do 100 inao=1,Nbas
      do 50 i=1,10
      Naolbl(i,inao)=iblnk
50    continue
      lbl=nameat(Iatno(Naoctr(inao)))
      call debyte(lbl,ibyte)
      Naolbl(1,inao)=ibyte(1)
      Naolbl(2,inao)=ibyte(2)
      call idigit(Naoctr(inao),istr,nd,MAXD)
      if(nd.EQ.1)then
      Naolbl(4,inao)=istr(1)
      else
      Naolbl(3,inao)=istr(1)
      Naolbl(4,inao)=istr(2)
      endif
      Naolbl(5,inao)=ileft
      call idigit(Iprin(inao),istr,nd,MAXD)
      if(nd.EQ.1)then
      Naolbl(7,inao)=istr(1)
      else
      Naolbl(6,inao)=istr(1)
      Naolbl(7,inao)=istr(2)
      endif
      l=Naol(inao)/100
      Naolbl(8,inao)=iang(l+1)
      if(l.EQ.1)then
      m=mod(Naol(inao),10)
      Naolbl(9,inao)=ixyz(m)
      elseif(l.EQ.2.OR.l.EQ.3)then
      Naolbl(9,inao)=mod(Naol(inao),10)+48
      endif
      Naolbl(10,inao)=iright
100   continue
      return
      end
C* :1 * 
      
