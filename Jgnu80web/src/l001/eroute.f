
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 eroute"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "eroute.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 193 "eroute.web"
      
      
      subroutine eroute(NONSTD)
      implicit none
      integer*4 Cnoe,Coord,dash,Direct,Dummy,gparse,Grdsym,i,Ialt,Ibas1,
     &Ibas2,Ibas3,Ibas4,Icard,Icmplx,Id2e,Ifau,Ifrad,Iges,imax
      integer*4 In,incov,Intsym,iolbl,Ioptyp,Iout,iovdat,iover,iovr,Iprc
     &1,Iprc2,Iprc3,Iprc4,Ipseud,Ipunch,Iraff,Irtcrd,Iscfdm,Itest,Ititle
      integer*4 Its,itst,Itype,jmp,jmpdat,jmploc,jtype,Labl,last,lim,n1,
     &n10,n2,n3,n5,n7,nc,ncards,ncover,ncovr
      integer*4 Ndchg,ndsh,Noextr,Nondef,NONSTD,nop,Nopop,Nostd,Nosym,no
     &ver,nrtcrd,number,Optcyc,Prtges,Rpac,Savbas,Savfc,Savmo,Scfcyc,Stb
     &int
      integer*4 Stbopt,Stbout,Stbrxt,Stbsym,Stccxt,Stronl,Synch,Units,Vs
     &hift
      
      common/ertcrd/Icard(80)
      
      common/label/Labl(1000),Ititle(100),Irtcrd(100)
      
      common/ertgen/Itype,Ibas1,Ibas2,Ibas3,Ibas4,Iprc1,Iprc2,Iprc3,Iprc
     &4,Iges,Savfc,Savbas,Savmo,Ialt,Iscfdm,Its,Ipseud,Noextr,Iraff,Nopo
     &p,Itest,Synch,Nosym,Scfcyc,Nostd,Stbsym,Stbopt,Stbout,Stbint,Stbrx
     &t,Stccxt,Prtges,Icmplx,Ioptyp,Id2e,Intsym,Grdsym,Ifau,Ifrad,Units,
     &Optcyc,Coord,Stronl,Nondef,Ndchg,Cnoe,Rpac,Vshift,Direct,Dummy(51)
      dimension nover(26),jmploc(26),jmpdat(26)
      dimension iovdat(25,26)
      dimension number(50),incov(50)
      common/io/In,Iout,Ipunch
      
      data ncovr/0/
      data iolbl/502/
      data dash/'-'/
      
      data nover/6,0,8,0,0,16,0,19,0,21,0,21,15,19,0,13,0,13,0,4,0,7,0,1
     &1,8,10/
      
      data jmpdat/0,0,0,0,0,-4,0,-4,0,-7,0,-7,-3,-5,0,-2,0,-3,0,0,0,0,0,
     &0,0,0/
      
      data jmploc/0,0,0,0,0,13,0,16,0,18,0,18,11,15,0,10,0,11,0,0,0,0,0,
     &0,0,0/
      
      data iovdat/1,2,3,4,5,6,19*0,25*0,1,2,3,4,5,6,8,9,17*0,25*0,25*0,1
     &,2,3,4,5,6,7,1,2,3,5,7,1,3,5,6,9*0,25*0,1,2,3,4,5,6,8,9,10,7,1,2,3
     &,5,7,1,3,5,6,6*0,25*0,1,2,3,4,5,8,9,10,7,1,2,3,5,8,9,10,7,1,3,5,6,
     &4*0,25*0,1,2,3,4,5,8,9,10,7,1,2,3,5,8,9,10,7,1,3,5,6,4*0,1,2,3,4,5
     &,6,1,2,3,5,1,2,3,5,6,10*0,1,2,3,4,5,6,8,9,1,2,3,5,8,9,1,2,3,5,6,6*
     &0,25*0,1,2,3,4,5,6,2,3,5,2,3,5,6,12*0,25*0,1,2,3,4,5,6,2,3,5,6,2,1
     &4*0,25*0,1,2,3,4,21*0,25*0,1,2,3,4,5,7,6,18*0,25*0,1,2,3,4,5,8,9,1
     &0,7,1,6,14*0,1,2,3,4,5,8,9,6,17*0,1,2,3,4,5,8,9,6,5,6,15*0/
      
      
      nrtcrd=100
      call rdrout(Icard,Irtcrd,nrtcrd)
      if(nrtcrd.GT.400)write(Iout,99002)
      if(nrtcrd.GT.400)call lnk1e
      
      ndsh=min0(nrtcrd,76)
      write(Iout,99003)(dash,i=1,ndsh)
      imax=nrtcrd/4+1
      write(Iout,99001)(Irtcrd(i),i=1,imax)
      write(Iout,99003)(dash,i=1,ndsh)
      
      call twrite(iolbl,Labl,600,1,600,1,0)
      
      call ilsw(1,25,0)
      i=gparse(Irtcrd,nrtcrd)
      if(i.EQ.0)then
      
      NONSTD=0
      if(Nostd.NE.0)NONSTD=1
      if(Nostd.NE.0)return
      
      call rtedef(jtype)
      if(jtype.LE.0)write(Iout,99005)
      
      ncover=0
      lim=nover(jtype)
      jmp=jmpdat(jtype)
      itst=jmploc(jtype)
      
      
      
      if(lim.GT.0)then
      do 20 i=1,lim
      iover=iovdat(i,jtype)
      
      if(Nopop.NE.1.OR.iover.NE.6)then
      
      ncovr=ncovr+1
      number(ncovr)=iover
      incov(ncovr)=0
      if(i.EQ.itst)incov(ncovr)=jmp
      endif
20    continue
      ncovr=ncovr+1
      number(ncovr)=0
      ncards=ncovr
      
      nc=0
      nop=0
      n5=0
      n1=0
      n2=0
      n3=0
      n10=0
      n7=0
      call fillrt(0,0,0)
      
      
      
40    nc=nc+1
      iovr=number(nc)
      if(iovr.EQ.0)then
      
      if(Nondef.NE.0)call nondf
      call fillrt(6,Cnoe,0)
      call arcset
      if(Cnoe.NE.0)write(Iout,99004)
      return
      else
      
      if(iovr.EQ.2)then
      
      
      call fillrt(2,2,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      n2=n2+1
      call fillrt(4,2,nop)
      if(n2.GT.1.AND.jtype.EQ.16)call fillrt(3,1,5)
      if(n2.GT.1.AND.jtype.EQ.18)call fillrt(3,2,5)
      
      if(Nosym.NE.0)call fillrt(3,1,15)
      
      if(Coord.NE.0)call fillrt(3,1,29)
      elseif(iovr.EQ.3)then
      
      
      call fillrt(2,3,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      n3=n3+1
      call fillrt(4,1,nop)
      call fillrt(3,Ibas1,5)
      call fillrt(3,Ibas2,6)
      call fillrt(3,Ibas3,7)
      call fillrt(3,Ibas4,8)
      if(Direct.NE.0)call fillrt(3,Direct,45)
      if(Direct.NE.0)Iraff=0
      if(Itest.NE.0.AND.n3.EQ.1)call fillrt(3,1,26)
      
      if(n3.GT.1.AND.Ibas1.EQ.7)call fillrt(3,1,6)
      
      if(n3.NE.3)call fillrt(4,2,nop)
      
      if(n3.NE.2.AND.jtype.NE.20.AND.Nopop.EQ.0)call fillrt(4,3,nop)
      
      if(Rpac.EQ.1)Iraff=0
      call fillrt(3,Iraff,11)
      
      if(Ibas1.EQ.6)Ipseud=1
      if(Ibas1.EQ.6)call fillrt(3,3,17)
      
      if(Ipseud.NE.0)then
      if(n3.NE.3)call fillrt(4,5,nop)
      if(n3.NE.3)call fillrt(4,6,nop)
      if(Ipseud.EQ.1)call fillrt(3,1,16)
      if(Ipseud.EQ.2)call fillrt(3,2,16)
      endif
      
      if(n3.NE.3.AND.jtype.NE.20)call fillrt(4,11,nop)
      
      call fillrt(4,14,nop)
      
      last=14
      call fillrt(3,last,25)
      
      if(Intsym.EQ.1)call fillrt(3,1,30)
      if(Grdsym.EQ.2)call fillrt(3,1,31)
      
      if(jtype.EQ.24)call fillrt(4,7,nop)
      if(jtype.EQ.24)call fillrt(4,16,nop)
      
      if(Id2e.EQ.1.AND.n3.EQ.1)call fillrt(4,7,nop)
      if(Id2e.EQ.1.AND.n3.EQ.1)call fillrt(4,16,nop)
      if(Ioptyp.EQ.1.AND.(Iprc1.EQ.5.OR.Iprc1.EQ.2).AND.n3.NE.3)call fil
     &lrt(4,7,nop)
      if(Ioptyp.EQ.1.AND.(Iprc1.EQ.5.OR.Iprc1.EQ.2).AND.n3.NE.3)call fil
     &lrt(4,16,nop)
      
      if(Ioptyp.EQ.0.OR.n3.EQ.3)then
      
      if(Savbas.NE.0)call fillrt(3,1,32)
      endif
      elseif(iovr.EQ.4)then
      
      
      call fillrt(2,4,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      call fillrt(4,1,nop)
      if(Prtges.EQ.1)call fillrt(3,1,33)
      if(Ipseud.NE.0.AND.Iges.NE.1)Iges=2
      call fillrt(3,Iges,5)
      if(jtype.EQ.20)call fillrt(3,1,33)
      if(Ialt.NE.0)call fillrt(3,Ialt,8)
      if(Iprc2.EQ.2.AND.Icmplx.EQ.0)call fillrt(3,2,7)
      if(Iprc2.EQ.2.AND.Icmplx.NE.0)call fillrt(3,4,7)
      if(Iprc2.EQ.1.AND.Icmplx.EQ.0)call fillrt(3,1,7)
      if(Iprc2.EQ.1.AND.Icmplx.NE.0)call fillrt(3,3,7)
      if(Iprc2.EQ.3.AND.Icmplx.NE.0)call fillrt(3,5,7)
      if(Iprc2.EQ.4)call fillrt(3,2,7)
      
      if(Icmplx.EQ.0.AND.Iscfdm.EQ.0)call fillrt(3,1,16)
      elseif(iovr.EQ.5)then
      
      
      call fillrt(2,5,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      n5=n5+1
      
      if((jtype.EQ.25.OR.jtype.EQ.26).AND.n5.EQ.2)Iscfdm=1
      
      if(n5.EQ.3)call fillrt(3,1,12)
      
      
      if(nc.LE.5.AND.Vshift.NE.0)call fillrt(3,Vshift,9)
      
      
      if(jtype.NE.1.AND.Iprc2.NE.4)call fillrt(3,7,6)
      if(Iprc2.EQ.4.AND.jtype.NE.1)call fillrt(3,6,6)
      
      if(Iprc2.EQ.4.AND.n5.GT.1)call fillrt(3,1,18)
      
      if(Noextr.EQ.1)call fillrt(3,2,11)
      
      if(Iscfdm.NE.0.OR.Icmplx.EQ.1)then
      
      call fillrt(3,2,21)
      call fillrt(4,3,nop)
      if(Icmplx.EQ.1.AND.Iscfdm.EQ.0)call fillrt(3,2,8)
      
      if(Scfcyc.EQ.0)call fillrt(3,64,7)
      else
      if(Iprc2.EQ.1.OR.Iprc2.EQ.3)call fillrt(4,1,nop)
      if(Iprc2.EQ.2.OR.Iprc2.EQ.3)call fillrt(4,2,nop)
      if(Iprc2.EQ.4)call fillrt(4,5,nop)
      endif
      
      if(Scfcyc.NE.0)call fillrt(3,Scfcyc,7)
      
      if(Ioptyp.EQ.0.OR.n5.EQ.3.OR.jtype.EQ.22)then
      if(Savmo.NE.0)call fillrt(3,1,32)
      endif
      elseif(iovr.EQ.6)then
      
      
      call fillrt(2,6,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      call fillrt(4,1,nop)
      if(Nopop.EQ.2)call fillrt(3,2,7)
      if(Nopop.EQ.2.OR.Rpac.NE.0)call fillrt(3,2,8)
      if(Nopop.EQ.2.OR.Rpac.NE.0)call fillrt(3,2,9)
      if(Nopop.EQ.2)call fillrt(3,2,10)
      if(Rpac.NE.0)then
      call fillrt(4,2,nop)
      call fillrt(3,1,13)
      endif
      elseif(iovr.EQ.7)then
      
      
      call fillrt(2,7,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      n7=n7+1
      
      if(Grdsym.EQ.2)call fillrt(3,1,30)
      
      if(jtype.NE.24)then
      if(Id2e.NE.1.OR.n7.NE.1)then
      
      if(Iprc1.NE.2.AND.Iprc1.NE.5)call fillrt(4,1,nop)
      if(Iprc1.NE.2.AND.Iprc1.NE.5)call fillrt(3,1,27)
      if(Iprc1.NE.2.AND.Iprc1.NE.5)call fillrt(4,2,nop)
      if(Iprc1.NE.2.AND.Iprc1.NE.5)call fillrt(4,3,nop)
      if(Iprc1.EQ.2.OR.Iprc1.EQ.5)call fillrt(3,5,29)
      if(Ipseud.NE.0)call fillrt(4,5,nop)
      call fillrt(4,16,nop)
      goto 40
      endif
      endif
      
      call fillrt(4,7,nop)
      call fillrt(4,8,nop)
      call fillrt(4,9,nop)
      call fillrt(4,16,nop)
      if(jtype.EQ.24)call fillrt(3,1,8)
      call fillrt(3,1,25)
      call fillrt(3,5,29)
      elseif(iovr.EQ.8)then
      
      
      call fillrt(2,8,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      call fillrt(4,1,nop)
      call fillrt(4,2,nop)
      call fillrt(4,3,nop)
      if(Iprc4.EQ.1)call fillrt(3,1,10)
      if(Iprc1.EQ.3)call fillrt(3,2,6)
      if(Iprc1.EQ.4.AND.Iprc3.EQ.5)call fillrt(3,2,6)
      if(Iprc1.EQ.4.AND.Iprc3.EQ.4)call fillrt(3,3,6)
      if(Iprc1.EQ.4.AND.Iprc3.EQ.6)call fillrt(3,4,6)
      if(Iprc1.EQ.6)call fillrt(3,2,6)
      if(Iprc1.EQ.5.AND.Iprc3.EQ.2.AND.jtype.NE.10.AND.jtype.NE.12)call 
     &fillrt(3,2,6)
      if(Iprc1.EQ.5.AND.Iprc3.EQ.2.AND.(jtype.EQ.10.OR.jtype.EQ.12))call
     & fillrt(3,4,6)
      if(Iprc1.EQ.5.AND.Iprc3.EQ.3)call fillrt(3,3,6)
      if(jtype.EQ.25.OR.jtype.EQ.26)call fillrt(3,1,6)
      
      if(jtype.EQ.10.OR.jtype.EQ.12.OR.jtype.EQ.23)call fillrt(3,4,6)
      
      if(jtype.EQ.8.OR.jtype.EQ.24)call fillrt(3,3,6)
      elseif(iovr.EQ.9)then
      
      
      call fillrt(2,9,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      call fillrt(4,1,nop)
      if(jtype.NE.24)then
      if(jtype.EQ.25.OR.jtype.EQ.26)then
      
      call fillrt(4,2,nop)
      if(jtype.EQ.26)call fillrt(4,18,nop)
      if(Stbsym.EQ.2)call fillrt(3,1,13)
      if(jtype.EQ.25)call fillrt(3,4,6)
      if(jtype.EQ.25)call fillrt(3,2,9)
      if(jtype.NE.25)then
      
      call fillrt(3,Stbout-1,6)
      call fillrt(3,2,9)
      call fillrt(3,Stbint,10)
      call fillrt(3,Stbrxt,11)
      call fillrt(3,Stccxt,12)
      endif
      else
      if(Iprc1.GT.2)then
      do 42 i=9,13
      call fillrt(4,i,nop)
42    continue
      endif
      
      
      if(Iprc1.EQ.3)call fillrt(3,2,5)
      
      if(Iprc1.EQ.4.AND.Iprc3.NE.5.AND.Iprc3.NE.6)call fillrt(3,4,5)
      if(Iprc1.EQ.4.AND.Iprc3.EQ.6)call fillrt(3,5,5)
      
      if(Iprc3.EQ.2)call fillrt(3,1,5)
      
      if(Iprc1.EQ.4.AND.Iprc3.EQ.5)call fillrt(3,3,5)
      
      if(Iprc1.EQ.5.AND.(jtype.EQ.10.OR.jtype.EQ.12))call fillrt(3,1,15)
      
      
      if(Iprc1.EQ.6.AND.Iprc3.NE.3)call fillrt(3,6,5)
      endif
      endif
      elseif(iovr.EQ.10)then
      
      
      call fillrt(2,10,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      n10=n10+1
      if(Itype.EQ.2.AND.Iprc1.GE.2)call fillrt(4,1,nop)
      call fillrt(4,2,nop)
      if(n10.EQ.1.AND.Id2e.EQ.1)call fillrt(3,1,6)
      if(Itype.EQ.6)call fillrt(3,1,6)
      if(Iprc1.EQ.2.AND.(jtype.EQ.10.OR.jtype.EQ.12.OR.jtype.EQ.23))call
     & fillrt(3,1,5)
      if(Iprc1.EQ.5.AND.(jtype.EQ.10.OR.jtype.EQ.12.OR.jtype.EQ.23))call
     & fillrt(3,2,5)
      else
      
      
      call fillrt(2,1,0)
      if(incov(nc).NE.0)call fillrt(5,incov(nc),0)
      n1=n1+1
      if(n1.EQ.1)then
      
      call fillrt(4,1,nop)
      
      if(Id2e.EQ.2)call fillrt(3,3,10)
      
      if(Units.NE.0)call fillrt(3,Units,20)
      
      if(Ndchg.NE.0)call fillrt(3,1,31)
      
      if(Optcyc.NE.0.AND.Ioptyp.EQ.1)call fillrt(3,Optcyc,6)
      if(Optcyc.NE.0.AND.Ioptyp.EQ.4)call fillrt(3,Optcyc,6)
      
      if(jtype.EQ.24)call fillrt(3,4,10)
      if(jtype.EQ.24)call fillrt(3,7,15)
      
      if(Id2e.EQ.1)call fillrt(3,4,10)
      endif
      
      if(jtype.GE.6.AND.jtype.LE.12.AND.Ioptyp.NE.4)call fillrt(4,3,nop)
      if(jtype.GE.6.AND.jtype.LE.12.AND.Ioptyp.EQ.4)call fillrt(4,5,nop)
      if(jtype.EQ.22.OR.jtype.EQ.24)call fillrt(4,3,nop)
      
      if(jtype.EQ.24.AND.n1.EQ.2)call fillrt(3,1,30)
      
      if(Ioptyp.EQ.1.AND.Stronl.NE.0)call fillrt(3,1,18)
      
      if(Coord.NE.0)call fillrt(3,1,29)
      
      
      if(Its.EQ.1)call fillrt(3,1,5)
      
      if(Ioptyp.EQ.2)call fillrt(4,2,nop)
      
      if(n1.EQ.2.AND.jtype.EQ.24.AND.Savfc.NE.0)call fillrt(3,1,32)
      if(n1.EQ.3.AND.Ioptyp.EQ.1.AND.Savfc.NE.0)call fillrt(3,1,32)
      if(n1.EQ.3.AND.Ioptyp.EQ.4.AND.Savfc.NE.0)call fillrt(3,1,32)
      endif
      goto 40
      endif
      endif
      endif
      write(Iout,99006)
      call lnk1e
      stop 13
99001 format(2x,19A4)
99002 format(' ROUTE SPECIFICATION TOO LONG.')
99003 format(2x,128A1)
99004 format(' C.N.O.E.... COMPLETE NEGLECT OF EVERYTHING.',/' TASK SUCC
     &ESFULLY PERFORMED.')
99005 format(' ','  JTYPE ZERO.')
99006 format('  SYNTAX ERROR IN ROUTE SPECIFICATION.')
      end
C* :1 * 
      
