
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ptgrp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ptgrp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 32 "ptgrp.web"
      subroutine ptgrp(MAXAP3,A,B,C,D,IAN,ATMCHG,NATOMS,IPRINT,IDUMP,NGR
     &P,TRVEC)
      implicit none
      double precision A,ATMCHG,B,C,D,four,gabs,gatan,halfpi,one,pi,piov
     &r4,praxes,prmom,t,theta,Tol2,Toler,TRVEC,tst1
      double precision tst2,tst3,two,v,zero
      integer i,IAN,iat,IDUMP,ihop,In,iord,Iout,IPRINT,Ipunch,itop,itst,
     &ixyz,iytst,iztst,j,MAXAP3,NATOMS,NGRP,norder
      integer npop,nset,num,numatm
      dimension A(MAXAP3,3),B(MAXAP3,3),C(100,3),D(MAXAP3,3)
      dimension ATMCHG(*),NGRP(4),TRVEC(3),IAN(*)
      dimension prmom(3),praxes(3,3)
      dimension npop(100),nset(100)
      dimension t(3,3),v(3)
      common/io/In,Iout,Ipunch
      common/tol/Toler,Tol2
      data zero,one,two,four/0.0D0,1.0D0,2.0D0,4.0D0/
      
      
      
      
      
      
      
99001 format(1x,'PTGRP-- TRANSLATION VECTOR:',3F12.6)
99002 format(1x,'PTGRP-- PRINCIPAL MOMENTS AND AXES OF CHARGE:'/1x,'    
     &    MOMENTS:',3F12.7,/1x,'        AXES   :',3F12.6/17x,3F12.6/17x,
     &3F12.6)
99003 format(1x,'PTGRP-- THE MOLECULE IS LINEAR')
99004 format(1x,'PTGRP-- THE MOLECULE IS NOT LINEAR')
99005 format(1x,'PTGRP-- THE MOLECULE IS AN ASYMMETRIC TOP')
99006 format(1x,'PTGRP-- THE MOLECULE IS A SYMMETRIC TOP')
99007 format(1x,'PTGRP-- THE MOLECULE IS A SPHERICAL TOP')
99008 format(1x,'PTGRP-- THE MOLECULE IS AN ACCIDENTAL SPHERICAL TOP',/1
     &x,'PTGRP-- SYMMETRY TURNED OFF')
99009 format(1x,'PTGRP-- THE MOLECULE IS AN ACCIDENTAL SYMMETRIC TOP',/1
     &x,'PTGRP-- SYMMETRY TURNED OFF')
      
      
      piovr4=gatan(one)
      pi=four*piovr4
      halfpi=two*piovr4
      
      
      numatm=NATOMS+3
      do 100 iat=1,NATOMS
      A(iat,1)=C(iat,1)
      A(iat,2)=C(iat,2)
      A(iat,3)=C(iat,3)
100   continue
      do 200 iat=1,3
      do 150 ixyz=1,3
      A(NATOMS+iat,ixyz)=zero
150   continue
200   continue
      A(NATOMS+1,1)=one
      A(NATOMS+2,2)=one
      A(NATOMS+3,3)=one
      
      
      call center(MAXAP3,NATOMS,A,ATMCHG,TRVEC)
      TRVEC(1)=-TRVEC(1)
      TRVEC(2)=-TRVEC(2)
      TRVEC(3)=-TRVEC(3)
      if(IPRINT.NE.0)write(Iout,99001)(TRVEC(i),i=1,3)
      if(IDUMP.NE.0)write(Iout,99010)
      do 300 iat=1,NATOMS
      A(iat,1)=A(iat,1)+TRVEC(1)
      A(iat,2)=A(iat,2)+TRVEC(2)
      A(iat,3)=A(iat,3)+TRVEC(3)
      if(IDUMP.NE.0)write(Iout,99011)(A(iat,i),i=1,3)
300   continue
      
99010 format(' PTGRP-- COORDINATES AFTER TRANSLATION: ')
99011 format(' PTGRP-- ',3G15.4)
      
      
      call secmom(MAXAP3,NATOMS,A,ATMCHG,prmom,praxes)
      if(IPRINT.NE.0)write(Iout,99002)(prmom(i),i=1,3),((praxes(j,i),i=1
     &,3),j=1,3)
      
      
      if(gabs(prmom(1)).GT.Tol2.OR.gabs(prmom(3)-prmom(2)).GT.Tol2)then
      
      
      if(IDUMP.NE.0)write(Iout,99004)
      itop=0
      tst1=prmom(2)-prmom(3)
      tst2=prmom(1)-prmom(3)
      tst3=prmom(1)-prmom(2)
      if(gabs(tst1).LT.Tol2)itop=itop+1
      if(gabs(tst2).LT.Tol2)itop=itop+1
      if(gabs(tst3).LT.Tol2)itop=itop+1
      if(itop.NE.3)itop=itop+1
      if(itop.EQ.2)then
      
      
      
      
      if(IDUMP.NE.0)write(Iout,99006)
      if(gabs(tst1).LT.Tol2)ixyz=1
      if(gabs(tst2).LT.Tol2)ixyz=2
      if(gabs(tst3).LT.Tol2)ixyz=3
      call put(MAXAP3,A,B,t,praxes(1,ixyz),numatm,3)
      call oraxis(MAXAP3,A,B,NATOMS,ATMCHG,3)
      
      
      call findcn(MAXAP3,NATOMS,A,B,D,ATMCHG,npop,nset,3,norder)
      if(norder.GT.1)then
      
      
      
      theta=pi/norder
      call rotate(MAXAP3,A,B,NATOMS,t,3,theta)
      call reflct(MAXAP3,B,D,NATOMS,t,3)
      call equiv(MAXAP3,A,D,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      call findc2(MAXAP3,A,B,D,npop,nset,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      
      call findv(MAXAP3,A,B,D,NATOMS,npop,nset,ATMCHG,itst)
      if(itst.EQ.0)then
      
      call reflct(MAXAP3,A,B,NATOMS,t,3)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      NGRP(1)=iord('C')
      NGRP(2)=num(norder,1)
      NGRP(3)=num(norder,2)
      if(itst.NE.0)NGRP(4)=iord('H')
      call orcn(MAXAP3,A,B,D,D,ATMCHG,npop,nset,NATOMS,IDUMP)
      else
      NGRP(1)=iord('C')
      NGRP(2)=num(norder,1)
      NGRP(3)=num(norder,2)
      NGRP(4)=iord('V')
      call ordn(MAXAP3,A,B,D,ATMCHG,npop,nset,NATOMS,norder,IDUMP)
      if(norder.EQ.2)call orc2v(MAXAP3,A,B,NATOMS,ATMCHG)
      endif
      else
      call reflct(MAXAP3,A,B,NATOMS,t,3)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      NGRP(1)=iord('D')
      NGRP(2)=num(norder,1)
      NGRP(3)=num(norder,2)
      call ordn(MAXAP3,A,B,D,ATMCHG,npop,nset,NATOMS,norder,IDUMP)
      else
      NGRP(1)=iord('D')
      NGRP(2)=num(norder,1)
      NGRP(3)=num(norder,2)
      NGRP(4)=iord('H')
      call ordn(MAXAP3,A,B,D,ATMCHG,npop,nset,NATOMS,norder,IDUMP)
      if(norder.EQ.2)call ord2h(MAXAP3,A,B,NATOMS,ATMCHG,IAN)
      endif
      endif
      else
      call findv(MAXAP3,A,B,D,NATOMS,npop,nset,ATMCHG,itst)
      if(itst.EQ.0)then
      
      NGRP(1)=iord('S')
      NGRP(2)=num(2*norder,1)
      NGRP(3)=num(2*norder,2)
      call orcn(MAXAP3,A,B,D,D,ATMCHG,npop,nset,NATOMS,IDUMP)
      else
      NGRP(1)=iord('D')
      NGRP(2)=num(norder,1)
      NGRP(3)=num(norder,2)
      NGRP(4)=iord('D')
      call ordn(MAXAP3,A,B,D,ATMCHG,npop,nset,NATOMS,norder,IDUMP)
      endif
      endif
      else
      call ilsw(1,26,1)
      write(Iout,99009)
      endif
      elseif(itop.EQ.3)then
      
      
      
      
      if(IPRINT.NE.0)write(Iout,99007)
      call sphere(MAXAP3,NATOMS,A,B,D,ATMCHG,nset,npop,norder,IDUMP)
      if(norder.NE.0)then
      
      ihop=norder-2
      if(ihop.EQ.2)then
      
      
      
      call invert(MAXAP3,A,B,NATOMS,t)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      NGRP(1)=iord('O')
      if(itst.NE.0)NGRP(2)=iord('H')
      call ordn(MAXAP3,A,B,D,ATMCHG,npop,nset,NATOMS,2,IDUMP)
      elseif(ihop.EQ.3)then
      
      
      
      call invert(MAXAP3,A,B,NATOMS,t)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      NGRP(1)=iord('I')
      if(itst.NE.0)then
      
      NGRP(2)=iord('H')
      call ordn(MAXAP3,A,B,D,ATMCHG,npop,nset,NATOMS,2,IDUMP)
      else
      call orcn(MAXAP3,A,B,D,D,ATMCHG,npop,nset,NATOMS,IDUMP)
      endif
      else
      
      
      
      call invert(MAXAP3,A,B,NATOMS,t)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      call findv(MAXAP3,A,B,D,NATOMS,npop,nset,ATMCHG,itst)
      NGRP(1)=iord('T')
      if(itst.NE.0)then
      NGRP(2)=iord('D')
      call rotate(MAXAP3,A,B,numatm,t,3,piovr4)
      call move(MAXAP3,B,A,numatm)
      endif
      else
      NGRP(1)=iord('T')
      NGRP(2)=iord('H')
      endif
      call ordn(MAXAP3,A,B,D,ATMCHG,npop,nset,NATOMS,2,IDUMP)
      endif
      else
      write(Iout,99008)
      call ilsw(1,26,1)
      endif
      else
      
      
      
      
      if(IDUMP.NE.0)write(Iout,99005)
      call put(MAXAP3,A,B,t,praxes(1,3),numatm,3)
      call oraxis(MAXAP3,A,B,NATOMS,ATMCHG,3)
      call secmom(MAXAP3,NATOMS,A,ATMCHG,prmom,praxes)
      theta=halfpi
      if(gabs(praxes(2,2)).GT.Tol2)theta=-gatan(praxes(1,2)/praxes(2,2))
      call rotate(MAXAP3,A,B,numatm,t,3,theta)
      call move(MAXAP3,B,A,numatm)
      call oraxis(MAXAP3,A,B,NATOMS,ATMCHG,2)
      call orptst(MAXAP3,A,NATOMS,ixyz)
      if(ixyz.NE.0)call orplan(MAXAP3,A,B,ATMCHG,numatm,prmom,praxes,ixy
     &z)
      
      
      call rotate(MAXAP3,A,B,NATOMS,t,3,pi)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,iztst)
      call rotate(MAXAP3,A,B,NATOMS,t,2,pi)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,iytst)
      itst=2*iztst+iytst+1
      if(itst.EQ.1)then
      
      
      call rotate(MAXAP3,A,B,NATOMS,t,1,pi)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      
      call invert(MAXAP3,A,B,NATOMS,t)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      do 302 i=1,3
      v(i)=zero
302   continue
      do 304 ixyz=1,3
      call reflct(MAXAP3,A,B,NATOMS,t,ixyz)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.NE.0)then
      v(ixyz)=one
      call put(MAXAP3,A,B,t,v,numatm,3)
      call oraxis(MAXAP3,A,B,NATOMS,ATMCHG,3)
      call orplan(MAXAP3,A,B,ATMCHG,numatm,prmom,praxes,3)
      goto 310
      endif
      
304   continue
      
      NGRP(1)=iord('C')
      NGRP(2)=iord('0')
      NGRP(3)=iord('1')
      else
      NGRP(1)=iord('C')
      NGRP(2)=iord('I')
      endif
      goto 400
      else
      call rotate(MAXAP3,A,B,numatm,t,2,halfpi)
      call oraxis(MAXAP3,B,A,NATOMS,ATMCHG,3)
      call orplan(MAXAP3,B,A,ATMCHG,numatm,prmom,praxes,3)
      call move(MAXAP3,B,A,numatm)
      goto 320
      endif
      
310   NGRP(1)=iord('C')
      NGRP(2)=iord('S')
      call orcn(MAXAP3,A,B,D,D,ATMCHG,npop,nset,NATOMS,IDUMP)
      elseif(itst.EQ.2)then
      
      
      call rotate(MAXAP3,A,B,numatm,t,1,halfpi)
      call oraxis(MAXAP3,B,A,NATOMS,ATMCHG,3)
      call orplan(MAXAP3,B,A,ATMCHG,numatm,prmom,praxes,3)
      call move(MAXAP3,B,A,numatm)
      goto 320
      elseif(itst.EQ.3)then
      goto 320
      else
      
      
      call invert(MAXAP3,A,B,NATOMS,t)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      NGRP(1)=iord('D')
      NGRP(2)=iord('0')
      NGRP(3)=iord('2')
      if(itst.NE.0)then
      NGRP(4)=iord('H')
      call ord2h(MAXAP3,A,B,NATOMS,ATMCHG,IAN)
      endif
      endif
      goto 400
      
      
320   call reflct(MAXAP3,A,B,NATOMS,t,3)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      call reflct(MAXAP3,A,B,NATOMS,t,2)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      NGRP(1)=iord('C')
      NGRP(2)=iord('0')
      NGRP(3)=iord('2')
      call orcn(MAXAP3,A,B,D,D,ATMCHG,npop,nset,NATOMS,IDUMP)
      else
      NGRP(1)=iord('C')
      NGRP(2)=iord('0')
      NGRP(3)=iord('2')
      NGRP(4)=iord('V')
      call orc2v(MAXAP3,A,B,NATOMS,ATMCHG)
      endif
      else
      NGRP(1)=iord('C')
      NGRP(2)=iord('0')
      NGRP(3)=iord('2')
      NGRP(4)=iord('H')
      call orcn(MAXAP3,A,B,D,D,ATMCHG,npop,nset,NATOMS,IDUMP)
      endif
      endif
      else
      
      
      if(IDUMP.NE.0)write(Iout,99003)
      call put(MAXAP3,A,B,t,praxes(1,1),numatm,3)
      call oraxis(MAXAP3,A,B,NATOMS,ATMCHG,3)
      call reflct(MAXAP3,A,B,NATOMS,t,3)
      call equiv(MAXAP3,A,B,ATMCHG,NATOMS,itst)
      if(itst.EQ.0)then
      
      NGRP(1)=iord('C')
      NGRP(2)=iord('*')
      NGRP(3)=iord('V')
      else
      NGRP(1)=iord('D')
      NGRP(2)=iord('*')
      NGRP(3)=iord('H')
      endif
      endif
      
      
400   call secmom(MAXAP3,NATOMS,A,ATMCHG,prmom,praxes)
      if(IPRINT.NE.0)write(Iout,99002)(prmom(i),i=1,3),((praxes(j,i),i=1
     &,3),j=1,3)
      return
      
      end
C* :1 * 
      
