
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 oper"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "oper.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "oper.web"
      subroutine oper(MAXAP3,NGRP,NATOMS,MAXOP,TRANS,NPERM,NOP,A,B,D,IDU
     &MP)
      implicit none
      double precision A,B,D,four,gatan,gfloat,one,t,theta,Tol2,Toler,TR
     &ANS,zero
      integer i,iat,iblnk,ichar,IDUMP,In,iord,Iout,Ipunch,j,j1,j2,jchar,
     &k,MAXAP3,MAXOP,NATOMS,NGRP,NOP,norder
      integer NPERM,numer
      dimension NGRP(4),TRANS(3,3,MAXOP),NPERM(MAXAP3,MAXOP)
      dimension A(MAXAP3,3),B(MAXAP3,3),D(MAXAP3,3)
      dimension t(3,3)
      common/io/In,Iout,Ipunch
      common/tol/Toler,Tol2
      data zero,one,four/0.0D0,1.0D0,4.0D0/
      data iblnk/' '/
      
      
      
      
      
      
      
99001 format(/)
99002 format(/1x,'TOTAL NUMBER OF SYMMETRY OPERATIONS: ',i3/)
99003 format(1x,'PERMUTATIONS OVER ATOMS:'/)
99004 format(1x,44I3)
99005 format(/1x,'TRANSFORMATION MATRICES:')
99006 format(/1x,12x,i3,3(31x,i3))
99007 format(1x,3(2(f8.5,2x),f8.5,6x),2(f8.5,2x),f8.5)
99008 format(/1x,'POINT GROUP ',4A1)
      
      
      
      NOP=1
      do 100 iat=1,NATOMS
      NPERM(iat,NOP)=iat
      do 50 i=1,3
      do 20 j=1,3
      TRANS(i,j,NOP)=zero
20    continue
      TRANS(i,i,NOP)=one
50    continue
100   continue
      
      call inirep(NGRP,NATOMS)
      
      
      ichar=NGRP(1)
      if(ichar.EQ.iord('C'))then
      
      jchar=NGRP(2)
      
      
      if(jchar.NE.iord('*').AND.NGRP(3).NE.iord('1'))then
      
      
      if(jchar.EQ.iord('S'))then
      call reflct(MAXAP3,A,B,NATOMS,t,3)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,A,B)
      
      
      elseif(jchar.NE.iord('I'))then
      
      
      norder=numer(NGRP)
      jchar=NGRP(4)
      if(jchar.EQ.iblnk)then
      call cnaxis(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B)
      
      
      elseif(jchar.NE.iord('V'))then
      
      
      call cnaxis(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B)
      call sighcn(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      else
      call cnaxis(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B)
      call vert(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      endif
      else
      call invert(MAXAP3,A,B,NATOMS,t)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,A,B)
      endif
      endif
      elseif(ichar.EQ.iord('S'))then
      
      
      
      norder=numer(NGRP)
      call snaxis(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      elseif(ichar.EQ.iord('D'))then
      
      norder=numer(NGRP)
      
      
      if(NGRP(2).EQ.iord('*'))then
      
      
      call invert(MAXAP3,A,B,NATOMS,t)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,A,B)
      elseif(NGRP(4).NE.iblnk)then
      
      
      if(NGRP(4).NE.iord('D'))then
      
      
      call cnaxis(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B)
      call c2axes(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D,zero)
      call vert(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      call sighcn(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      else
      theta=four*gatan(one)/gfloat(2*norder)
      call c2axes(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D,theta
     &)
      call vert(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      call snaxis(MAXAP3,2*norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      endif
      else
      call cnaxis(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B)
      call c2axes(MAXAP3,norder,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D,zero)
      endif
      elseif(ichar.EQ.iord('T'))then
      
      
      if(NGRP(2).NE.iord('H'))call tetra(MAXAP3,NGRP,NATOMS,NOP,MAXOP,TR
     &ANS,NPERM,A,B,D)
      elseif(ichar.EQ.iord('O'))then
      
      
      
      
      
      
      call octa(MAXAP3,NGRP,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      elseif(ichar.NE.iord('I'))then
      if(ichar.EQ.iord('K'))then
      endif
      endif
      
      
      
      call outrep(IDUMP)
      
      if(IDUMP.EQ.0)return
      write(Iout,99008)(NGRP(i),i=1,4)
      write(Iout,99002)NOP
      write(Iout,99003)
      j1=1
200   j2=min0(NOP,j1+43)
      write(Iout,99004)(j,j=j1,j2)
      write(Iout,99001)
      do 300 i=1,NATOMS
      write(Iout,99004)(NPERM(i,j),j=j1,j2)
300   continue
      if(j2.EQ.NOP)then
      
      write(Iout,99005)
      j1=1
350   j2=min0(NOP,j1+3)
      write(Iout,99006)(j,j=j1,j2)
      do 400 i=1,3
      write(Iout,99007)((TRANS(i,j,k),j=1,3),k=j1,j2)
400   continue
      if(j2.EQ.NOP)return
      j1=j2+1
      goto 350
      else
      j1=j2+1
      write(Iout,99001)
      write(Iout,99001)
      goto 200
      endif
      
      end
C* :1 * 
      
