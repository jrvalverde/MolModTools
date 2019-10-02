
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fwgrp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fwgrp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 34 "fwgrp.web"
      subroutine fwgrp(MAXAP3,NGRP,NATOMS,ICHARG,MULTIP,IAN,NOP,MAXOP,NP
     &ERM,A,B,D,MOLFOR,LENFOR,FWG,LENGTH)
      implicit none
      double precision A,B,D,four,gatan,one,pi,t,theta,v
      integer i,i1,IAN,iat,iatflg,iblnk,ichar,ICHARG,iord,itst,jchar,LEN
     &FOR,LENGTH,MAXAP3,MAXOP,MOLFOR,MULTIP,NATOMS,NGRP,nhalf
      integer NOP,norder,NPERM,num,numer
      integer FWG(*)
      dimension A(MAXAP3,3),NGRP(4),IAN(*),B(MAXAP3,3),D(MAXAP3,3)
      dimension MOLFOR(*),NPERM(*)
      dimension t(3,3),v(3),iatflg(100)
      data iblnk/' '/
      data one,four/1.0D0,4.0D0/
      
      
      
      
      
      
      
      pi=four*gatan(one)
      
      
      
      
      LENGTH=0
      LENFOR=0
      do 100 i=1,100
      if(i.LE.30)MOLFOR(i)=iblnk
      FWG(i)=iblnk
      iatflg(i)=2
100   continue
      
      
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENFOR,MOLFOR,0,3)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENFOR,MOLFOR,0,0)
      
      
      LENFOR=LENFOR-3
      do 200 i=1,LENFOR
      MOLFOR(i)=MOLFOR(i+1)
200   continue
      
      
      if(ICHARG.NE.0)then
      call cram(203,MOLFOR,LENFOR)
      num=iabs(ICHARG)
      call cram(num,MOLFOR,LENFOR)
      if(ICHARG.LT.0)call cram(219,MOLFOR,LENFOR)
      if(ICHARG.GT.0)call cram(218,MOLFOR,LENFOR)
      if(MULTIP.EQ.1)then
      call cram(204,MOLFOR,LENFOR)
      else
      call cram(216,MOLFOR,LENFOR)
      call cram(MULTIP,MOLFOR,LENFOR)
      call cram(204,MOLFOR,LENFOR)
      endif
      
      
      elseif(MULTIP.NE.1)then
      call cram(203,MOLFOR,LENFOR)
      call cram(MULTIP,MOLFOR,LENFOR)
      call cram(204,MOLFOR,LENFOR)
      endif
      
      
      
      LENGTH=0
      do 300 i=1,4
      LENGTH=LENGTH+1
      FWG(LENGTH)=NGRP(i)
300   continue
      call cram(220,FWG,LENGTH)
      call cram(201,FWG,LENGTH)
      
      
      do 400 iat=1,NATOMS
      iatflg(iat)=0
400   continue
      
      
      ichar=NGRP(1)
      if(ichar.EQ.iord('C'))then
      
      
      jchar=NGRP(2)
      
      
      
      if(jchar.EQ.iord('*'))then
      call cram(211,FWG,LENGTH)
      call cram(208,FWG,LENGTH)
      do 420 iat=1,NATOMS
      iatflg(iat)=2
420   continue
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,1)
      
      
      elseif(jchar.NE.iord('0').OR.NGRP(3).NE.iord('1'))then
      
      
      if(jchar.EQ.iord('S'))then
      call ssssig(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      
      
      elseif(jchar.EQ.iord('I'))then
      call ssso(MAXAP3,NATOMS,iatflg,A,itst)
      if(itst.NE.0)then
      call cram(209,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      
      
      elseif(jchar.NE.iord('0').OR.NGRP(3).NE.iord('2').OR.NGRP(4).NE.io
     &rd('V'))then
      
      
      jchar=NGRP(4)
      norder=numer(NGRP)
      if(jchar.EQ.iblnk)then
      
      
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(norder,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,1)
      endif
      
      
      elseif(jchar.EQ.iord('H'))then
      call ssso(MAXAP3,NATOMS,iatflg,A,itst)
      if(itst.NE.0)then
      call cram(209,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(norder,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call cram(213,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      
      
      elseif(mod(norder,2).EQ.0)then
      
      
      nhalf=norder/2
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(norder,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,1)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(nhalf,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(214,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      theta=pi/norder
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call ssssig(MAXAP3,NATOMS,iatflg,B,1,itst)
      if(itst.NE.0)then
      call cram(nhalf,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(215,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      else
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(norder,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,1)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(norder,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(214,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      endif
      else
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,1)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call cram(214,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,2,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call cram(214,FWG,LENGTH)
      call cram(206,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      endif
      endif
      elseif(ichar.EQ.iord('S'))then
      
      
      call ssso(MAXAP3,NATOMS,iatflg,A,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      norder=numer(NGRP)/2
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(norder,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      elseif(ichar.EQ.iord('D'))then
      
      
      call ssso(MAXAP3,NATOMS,iatflg,A,itst)
      if(itst.NE.0)then
      call cram(209,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      
      if(NGRP(2).NE.iord('*'))then
      
      norder=numer(NGRP)
      nhalf=norder/2
      if(norder.NE.2.OR.NGRP(4).NE.iord('H'))then
      
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(norder,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      
      if(NGRP(4).NE.iblnk)then
      
      
      
      if(NGRP(4).EQ.iord('D'))then
      theta=pi/norder
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call sssc(MAXAP3,NATOMS,iatflg,B,2,itst)
      if(itst.NE.0)then
      call cram(norder,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call cram(206,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(norder,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(215,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      
      
      
      elseif(mod(norder,2).EQ.0)then
      
      
      
      call sssc(MAXAP3,NATOMS,iatflg,A,2,itst)
      if(itst.NE.0)then
      call cram(nhalf,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call cram(206,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      theta=pi/norder
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call sssc(MAXAP3,NATOMS,iatflg,B,2,itst)
      if(itst.NE.0)then
      call cram(nhalf,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call cram(207,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call cram(213,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(nhalf,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(214,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,B,1,itst)
      if(itst.NE.0)then
      call cram(nhalf,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(215,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,B,NATOMS,LENGTH,FWG,0,0)
      endif
      else
      call sssc(MAXAP3,NATOMS,iatflg,A,2,itst)
      if(itst.NE.0)then
      call cram(norder,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call cram(213,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(norder,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(214,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      endif
      elseif(mod(norder,2).EQ.0)then
      
      
      
      call sssc(MAXAP3,NATOMS,iatflg,A,2,itst)
      if(itst.NE.0)then
      if(nhalf.NE.1)call cram(nhalf,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call cram(206,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      theta=pi/norder
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call sssc(MAXAP3,NATOMS,iatflg,B,2,itst)
      if(itst.NE.0)then
      if(nhalf.NE.1)call cram(nhalf,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call cram(207,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,B,NATOMS,LENGTH,FWG,2,2)
      endif
      else
      
      
      call sssc(MAXAP3,NATOMS,iatflg,A,2,itst)
      if(itst.NE.0)then
      call cram(norder,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      endif
      else
      
      
      call sssc(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,1,2)
      endif
      call sssc(MAXAP3,NATOMS,iatflg,A,2,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call cram(206,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call cram(207,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,2,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call cram(206,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(212,FWG,LENGTH)
      call cram(207,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      endif
      else
      
      
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      call cram(211,FWG,LENGTH)
      call cram(208,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      elseif(ichar.EQ.iord('T'))then
      
      
      if(NGRP(2).NE.iord('H'))then
      
      call ssso(MAXAP3,NATOMS,iatflg,A,itst)
      if(itst.NE.0)then
      call cram(209,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      
      
      call move(MAXAP3,A,B,NATOMS)
      v(1)=one
      v(2)=one
      v(3)=one
      call put(MAXAP3,B,D,t,v,NATOMS,3)
      call sssc(MAXAP3,NATOMS,iatflg,B,3,itst)
      if(itst.NE.0)then
      call cram(4,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(3,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(3,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(2,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      if(NGRP(2).NE.iblnk)then
      
      
      theta=gatan(one)
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call ssssig(MAXAP3,NATOMS,iatflg,B,1,itst)
      if(itst.NE.0)then
      call cram(6,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(215,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      endif
      else
      LENGTH=LENGTH-2
      do 440 i=3,15
      FWG(i)=iblnk
440   continue
      return
      endif
      elseif(ichar.EQ.iord('O'))then
      
      
      call ssso(MAXAP3,NATOMS,iatflg,A,itst)
      if(itst.NE.0)then
      call cram(209,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      
      
      call sssc(MAXAP3,NATOMS,iatflg,A,3,itst)
      if(itst.NE.0)then
      call cram(3,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(4,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      call move(MAXAP3,A,B,NATOMS)
      v(1)=one
      v(2)=one
      v(3)=one
      call put(MAXAP3,B,D,t,v,NATOMS,3)
      call sssc(MAXAP3,NATOMS,iatflg,B,3,itst)
      if(itst.NE.0)then
      call cram(4,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(3,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,3,2)
      endif
      theta=gatan(one)
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call sssc(MAXAP3,NATOMS,iatflg,B,2,itst)
      if(itst.NE.0)then
      call cram(4,FWG,LENGTH)
      call cram(211,FWG,LENGTH)
      call cram(3,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,2,2)
      endif
      
      
      if(NGRP(2).EQ.iord('H'))then
      call ssssig(MAXAP3,NATOMS,iatflg,B,1,itst)
      if(itst.NE.0)then
      call cram(6,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(215,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call ssssig(MAXAP3,NATOMS,iatflg,A,1,itst)
      if(itst.NE.0)then
      call cram(3,FWG,LENGTH)
      call cram(212,FWG,LENGTH)
      call cram(213,FWG,LENGTH)
      call ssseq(MAXAP3,NATOMS,NOP,MAXOP,iatflg,NPERM)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      endif
      elseif(ichar.EQ.iord('I'))then
      
      
      LENGTH=LENGTH-2
      i1=LENGTH+1
      do 450 i=i1,15
      FWG(i)=iblnk
450   continue
      return
      else
      if(ichar.EQ.iord('K'))then
      endif
      
      
      LENGTH=LENGTH-2
      i1=LENGTH+1
      do 500 i=i1,15
      FWG(i)=iblnk
500   continue
      return
      endif
      
      
      num=0
      do 600 iat=1,NATOMS
      if(iatflg(iat).NE.1)then
      num=num+1
      iatflg(iat)=2
      endif
600   continue
      if(num.NE.0)then
      call cram(210,FWG,LENGTH)
      call stoich(MAXAP3,iatflg,IAN,A,NATOMS,LENGTH,FWG,0,0)
      endif
      call cram(217,FWG,LENGTH)
      call cram(202,FWG,LENGTH)
      
      return
      
      end
C* :1 * 
      
