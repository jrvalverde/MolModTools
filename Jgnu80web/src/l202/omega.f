
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 omega"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "omega.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 36 "omega.web"
      subroutine omega(MAXAP3,ISYMM,IFWG,NOP,TRANS,NPERM,MAXOP,NATOMS,IP
     &RINT,IDUMP,A,C,TRVEC,NGRP,IAN,ITRANS,TOANG)
      implicit none
      double precision A,C,cx,cy,cz,gabs,t,TOANG,Tol2,Toler,TRANS,TRVEC,
     &zero
      integer i,i2,IAN,iat,IDUMP,IFWG,In,iop,iord,Iout,IPRINT,Ipunch,ISY
     &MM,ITRANS,ixyz,j,j1,jop,jprint,jtrans
      integer jtst,len,Lenfor,Lenfwg,lenpr,MAXAP3,MAXOP,Molfor,mout,NATO
     &MS,ndof,neqatm,NGRP,NOP,nosym,NPERM,nsymop,numdof
      integer cin(132)
      integer Fwg
      dimension TRANS(3,3,MAXOP),NPERM(MAXAP3,MAXOP)
      dimension A(MAXAP3,3),C(100,3),TRVEC(*),NGRP(4),IAN(*)
      dimension ITRANS(3,MAXOP),jprint(100)
      dimension jtrans(3,8),neqatm(100,8),mout(846),t(3,3)
      common/cfwg/Lenfor,Molfor(30),Lenfwg,Fwg(100)
      common/io/In,Iout,Ipunch
      common/tol/Toler,Tol2
      equivalence(nsymop,mout(1))
      equivalence(jtrans(1,1),mout(2))
      equivalence(neqatm(1,1),mout(26))
      equivalence(t(1,1),mout(829))
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
99001 format(1x,'OMEGA--  SYMMETRY TURNED OFF')
99002 format(1x,' NON-TWO-FOLD OPERATIONS DELETED'/)
99003 format(1x,44I3)
99004 format(1x,(/1x,i2,':',3x,3I4))
99005 format(1x,'OMEGA-- NO USEABLE SYMMETRY')
99006 format(1x,'FINAL SET OF OPERATIONS'/)
99007 format(1x,18x,'STANDARD ORIENTATION:')
99008 format(1x,'PREVIOUS ROTATION MATRIX USED')
99009 format(/1x,'OMEGA--  *** WARNING *** CHANGE IN POINT GROUP ','DETE
     &CTED'/)
99010 format(1x,'ROTATION MATRIX:')
99011 format(1x,22x,3F12.6)
99012 format(1x,'NUMBER OF SYMMETRY OPERATIONS:',i3)
99013 format(1x,'STOICHIOMETRY    ',100A1)
99014 format(1x,'FRAMEWORK GROUP  ',100A1)
99015 format(1x,'DEG. OF FREEDOM  ',i3)
      
      
      call tquery(IFWG,len)
      if(len.NE.0)call tread(IFWG,cin,66,1,66,1,0)
      
      
      call twrite(IFWG,Lenfor,66,1,66,1,0)
      call noones(Lenfor,Molfor,lenpr,jprint)
      write(Iout,99013)(jprint(i),i=1,lenpr)
      call noones(Lenfwg,Fwg,lenpr,jprint)
      write(Iout,99014)(jprint(j),j=1,lenpr)
      
      
      ndof=numdof(Fwg,NATOMS)
      write(Iout,99015)ndof
      
      
      call ilsw(2,26,nosym)
      if(nosym.NE.1)then
      
      if(nosym.NE.2)then
      
      
      if(len.NE.0)then
      do 10 i=1,100
      if(cin(i+32).NE.Fwg(i))goto 100
10    continue
      endif
      
      
      if(NOP.GT.1)then
      
      
      
      do 20 iop=1,NOP
      do 15 ixyz=1,3
      ITRANS(ixyz,iop)=int(sngl(TRANS(ixyz,ixyz,iop)))
15    continue
20    continue
      
      
      nsymop=0
      do 30 iop=1,NOP
      jtst=iabs(ITRANS(1,iop))+iabs(ITRANS(2,iop))+iabs(ITRANS(3,iop))
      if(jtst.EQ.3)then
      nsymop=nsymop+1
      do 22 ixyz=1,3
      jtrans(ixyz,nsymop)=ITRANS(ixyz,iop)
22    continue
      do 24 iat=1,NATOMS
      neqatm(iat,nsymop)=NPERM(iat,iop)
24    continue
      endif
30    continue
      
      
      if(IDUMP.NE.0)then
      write(Iout,99002)
      write(Iout,99003)(i,i=1,nsymop)
      write(Iout,99003)
      do 35 iat=1,NATOMS
      write(Iout,99003)(neqatm(iat,iop),iop=1,nsymop)
35    continue
      do 40 iop=1,nsymop
      write(Iout,99004)iop,(jtrans(ixyz,iop),ixyz=1,3)
40    continue
      endif
      
      
      
      if(nsymop.LE.1)goto 200
      i2=nsymop-1
      do 50 iop=1,i2
      if(neqatm(1,iop).NE.-99)then
      j1=iop+1
      do 44 jop=j1,nsymop
      do 42 iat=1,NATOMS
      if(neqatm(iat,jop).NE.neqatm(iat,iop))goto 44
42    continue
      neqatm(1,jop)=-99
44    continue
      endif
50    continue
      
      
      iop=0
60    iop=iop+1
      if(iop.GT.nsymop)goto 200
70    if(neqatm(1,iop).NE.-99)goto 60
      nsymop=nsymop-1
      if(nsymop.LT.iop)goto 200
      do 90 jop=iop,nsymop
      do 75 ixyz=1,3
      jtrans(ixyz,jop)=jtrans(ixyz,jop+1)
75    continue
      do 80 iat=1,NATOMS
      neqatm(iat,jop)=neqatm(iat,jop+1)
80    continue
90    continue
      goto 70
      else
      call ilsw(1,26,1)
      write(Iout,99001)
      return
      endif
      
100   call ilsw(1,26,2)
      write(Iout,99009)
      write(Iout,99001)
      call tread(ISYMM,mout,423,1,423,1,0)
      nsymop=0
      else
      call tread(ISYMM,mout,423,1,423,1,0)
      write(Iout,99008)
      nsymop=0
      endif
      goto 400
      else
      do 150 i=1,3
      do 120 j=1,3
      t(i,j)=A(NATOMS+j,i)
120   continue
150   continue
      call fixrep(t)
      return
      endif
      
      
200   if(nsymop.GT.1)then
      
      
      if(IPRINT.NE.0)then
      write(Iout,99006)
      write(Iout,99003)(i,i=1,nsymop)
      write(Iout,99003)
      do 220 iat=1,NATOMS
      write(Iout,99003)(neqatm(iat,iop),iop=1,nsymop)
220   continue
      do 240 iop=1,nsymop
      write(Iout,99004)iop,(jtrans(ixyz,iop),ixyz=1,3)
240   continue
      endif
      
      
      do 300 i=1,3
      do 260 j=1,3
      t(i,j)=A(NATOMS+j,i)
260   continue
300   continue
      
      
      call twrite(ISYMM,mout,423,1,423,1,0)
      else
      do 350 i=1,3
      do 320 j=1,3
      t(i,j)=A(NATOMS+j,i)
320   continue
350   continue
      call fixrep(t)
      call ilsw(1,26,1)
      write(Iout,99005)
      return
      endif
      
      
      
      
400   do 500 iat=1,NATOMS
      do 450 ixyz=1,3
      C(iat,ixyz)=C(iat,ixyz)+TRVEC(ixyz)
450   continue
500   continue
      
      
      if(NGRP(1).NE.iord('C').OR.NGRP(2).NE.iord('I'))then
      do 550 iat=1,NATOMS
      cx=t(1,1)*C(iat,1)+t(1,2)*C(iat,2)+t(1,3)*C(iat,3)
      cy=t(2,1)*C(iat,1)+t(2,2)*C(iat,2)+t(2,3)*C(iat,3)
      cz=t(3,1)*C(iat,1)+t(3,2)*C(iat,2)+t(3,3)*C(iat,3)
      if(gabs(cx).LT.Tol2)cx=zero
      if(gabs(cy).LT.Tol2)cy=zero
      if(gabs(cz).LT.Tol2)cz=zero
      C(iat,1)=cx
      C(iat,2)=cy
      C(iat,3)=cz
550   continue
      endif
      
      
      write(Iout,99007)
      call corprt(100,NATOMS,IAN,C,TOANG)
      write(Iout,99012)nsymop
      
      
      if(IPRINT.EQ.0)return
      write(Iout,99010)
      do 600 i=1,3
      write(Iout,99011)(t(i,j),j=1,3)
600   continue
      return
      
      end
C* :1 * 
      
