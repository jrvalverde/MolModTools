
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ilsw"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ilsw.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "ilsw.web"
      subroutine ilsw(IOPER,WHERE,WHAT)
      implicit none
      integer Basis,Bnread,Const,Conver,D5d6,F7f10,Guess,i0,Ifarch,Ifau,
     &Ifdon1,Ifdon2,Iffon1,Iffon2,Iffon3,Iffp,Iffpol,Ifgrd,Ifponh,Ifrc
      integer Ilsw1,In,ioc,ioilsw,Iop,IOPER,Iout,Ipunch,loc,loc1,Nosym,o
     &ldsw,Opclo,Polar,Prtoff,Psave,shift,Stabil,Symm,WHAT
      integer whats,WHERE
      integer not,or,and
      common/ilsw1/Ilsw1(2)
      common/iop/Iop(50)
      common/io/In,Iout,Ipunch
      common/il/Const(2),D5d6(2),Basis(2),Polar(2),Conver(2),Stabil(2),S
     &ymm(2),Guess(2),Bnread(2),Ifponh(2),Ifdon1(2),Ifdon2(2),Iffon1(2),
     &Iffon2(2),Iffon3(2),F7f10(2),Iffpol(2),Ifau(2),Iffp(2),Prtoff(2),P
     &save(2),Opclo(2),Ifgrd(2),Ifrc(2),Ifarch(2),Nosym(2)
      data ioilsw/998/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(1H0,'ILSW--  BEFORE: ',i12,' AFTER: ',i12,' IOPER: ',i4,' W
     &HERE: ',i4,' WHAT ',i4)
      
      
      loc1=2*WHERE
      loc=loc1-1
      i0=Ilsw1(1)
      oldsw=Ilsw1(1)
      if(IOPER.EQ.2)then
      
      whats=iand(i0,Const(loc))
      WHAT=shift(whats,-Const(loc1))
      else
      
      ioc=not(Const(loc))
      ioc=iand(i0,ioc)
      whats=shift(WHAT,Const(loc1))
      i0=ior(whats,ioc)
      Ilsw1(1)=i0
      endif
      if(Iop(34).NE.0)write(Iout,99001)oldsw,Ilsw1(1),IOPER,WHERE,WHAT
      if(IOPER.EQ.1)call twrite(ioilsw,Ilsw1,1,1,1,1,0)
      return
      
      end
C* :1 * 
      
