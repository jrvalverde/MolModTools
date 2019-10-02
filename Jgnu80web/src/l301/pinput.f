
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pinput"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pinput.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 71 "pinput.web"
      subroutine pinput(NATOMS,IAN,C,NAE,NBE,NE,ATMCHG)
      implicit none
      double precision ATMCHG,C,Clp,Zlp
      integer IAN,In,Iout,Iprin,Ipunch,iread,Jprinp,Jpseud,Jreadp,Jsubp,
     &Kfirst,Klast,Lmax,Lpskip,NAE,NATOMS,NBE,NE,Nfroz,Nlp
      dimension IAN(*),C(*),ATMCHG(*)
      common/io/In,Iout,Ipunch
      common/lp2/Nlp(400),Clp(400),Zlp(400),Kfirst(35,5),Klast(35,5),Lma
     &x(35),Lpskip(35),Nfroz(35)
      common/cprint/Iprin
      common/potpar/Jpseud,Jreadp,Jprinp,Jsubp
      
      
      
      
99001 format(1x,131(1H=),/,1x,26HPSEUDOPOTENTIAL PARAMETERS,/,1x,131(1H=
     &),/,2x,6HCENTER,5x,6HATOMIC,6x,7HVALENCE,6x,7HANGULAR,6x,5HPOWER,5
     &5x,11HCOORDINATES)
99002 format(2x,6HNUMBER,5x,6HNUMBER,5x,9HELECTRONS,5x,8HMOMENTUM,5x,4HO
     &F R,6x,8HEXPONENT,8x,11HCOEFFICIENT,16x,1HX,11x,1HY,11x,1HZ,/,1x,1
     &31(1H=))
99003 format(1x,131(1H=))
      
      
      iread=Jreadp
      if(iread.NE.7)then
      
      Iprin=0
      if(Jprinp.EQ.1)Iprin=1
      else
      Iprin=1
      if(Jprinp.EQ.9)Iprin=0
      endif
      if((Iprin.NE.0).OR.(iread.EQ.7))then
      write(Iout,99001)
      write(Iout,99002)
      endif
      call setpot(NATOMS,IAN,C,NAE,NBE,NE,ATMCHG)
      if((Iprin.NE.0).OR.(iread.EQ.7))write(Iout,99003)
      call twrite(12,Nlp,1210,1,1210,1,0)
      return
      
      end
C* :1 * 
      
