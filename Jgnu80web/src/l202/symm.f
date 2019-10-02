
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 symm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "symm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 215 "symm.web"
      subroutine symm(IOP,NATOMS,MULTIP,ICHARG,IAN,CIN,ATMCHG,TOANG)
      implicit none
      double precision ATMCHG,c,CIN,cnew,cscr1,cscr2,TOANG,Tol2,Toler,tr
     &ans,trvec
      integer i,IAN,iat,iblnk,ICHARG,idump,ifau,ifwg,In,IOP,iord,Iout,ip
     &rint,Ipunch,isymm,j,k,Lenfor,Lenfwg,maxap3
      integer maxop,Molfor,MULTIP,NATOMS,ngrp,nop,nosyme,nperm
      integer Fwg
      dimension IAN(*),IOP(50),CIN(*),ATMCHG(*)
      dimension c(100,3)
      dimension ngrp(4),cnew(103,3),cscr1(103,3),cscr2(103,3)
      dimension trans(3,3,48),nperm(103,48),trvec(3)
      common/io/In,Iout,Ipunch
      common/tol/Toler,Tol2
      common/cfwg/Lenfor,Molfor(30),Lenfwg,Fwg(100)
      data c/300*0./,cnew/309*0./,cscr1/309*0./
      data cscr2/309*0./,trans/432*0./,nperm/4944*0/
      data trvec/3*0./
      data iblnk/1H /
      data maxop/48/,maxap3/103/,isymm/551/,ifwg/552/
      
      
      
99001 format(1x,'SYMM--  CANNOT COPE WITH DUMMY OR (SHIVER!) GHOST',' AT
     &OMS'/1x,'SYMM--  SYMMETRY TURNED OFF')
99002 format(1x,'SYMM--  SYMMETRY TURNED OFF FOR ATOMIC CALCULATION')
99003 format(1x,'SYMM--  SYMMETRY TURNED OFF BY EXTERNAL REQUEST')
      
      
      
      
      call ilsw(2,26,nosyme)
      if(nosyme.NE.0)write(Iout,99003)
      
      
      
      ngrp(1)=iblnk
      ngrp(2)=iblnk
      ngrp(3)=iblnk
      ngrp(4)=iblnk
      Toler=1.0D-05
      Tol2=2.5D-07
      
      
      call ilsw(2,18,ifau)
      if(ifau.NE.0)Toler=Toler/TOANG
      
      
      iprint=IOP(33)+IOP(34)
      idump=IOP(34)
      
      
      k=0
      do 100 i=1,NATOMS
      do 50 j=1,3
      k=k+1
      c(i,j)=CIN(k)
50    continue
100   continue
      
      
      if(NATOMS.GT.1)then
      
      
      do 150 iat=1,NATOMS
      if(IAN(iat).LE.0)then
      write(Iout,99001)
      call ilsw(1,26,1)
      goto 200
      endif
      
150   continue
      
      
      call ptgrp(maxap3,cnew,cscr1,c,cscr2,IAN,ATMCHG,NATOMS,iprint,idum
     &p,ngrp,trvec)
      else
      ngrp(1)=iord('K')
      ngrp(2)=iord('H')
      call ilsw(1,26,1)
      write(Iout,99002)
      endif
      
      
200   call oper(maxap3,ngrp,NATOMS,maxop,trans,nperm,nop,cnew,cscr1,cscr
     &2,idump)
      
      
      call fwgrp(maxap3,ngrp,NATOMS,ICHARG,MULTIP,IAN,nop,maxop,nperm,cn
     &ew,cscr1,cscr2,Molfor,Lenfor,Fwg,Lenfwg)
      
      
      call omega(maxap3,isymm,ifwg,nop,trans,nperm,maxop,NATOMS,iprint,i
     &dump,cnew,c,trvec,ngrp,IAN,cscr1,TOANG)
      
      
      k=0
      do 300 i=1,NATOMS
      do 250 j=1,3
      k=k+1
      CIN(k)=c(i,j)
250   continue
300   continue
      
      
      return
      
      end
C* :1 * 
      
