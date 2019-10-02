
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fpdump"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fpdump.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 17 "fpdump.web"
      subroutine fpdump
      implicit none
      double precision Alpha,Convrg,D1var,D1vold,D2var,Delvar,F,F1,Fzero
     &,H,Pool0,Pool1,Tcurcy,Telcur,Teltot,Tlstcy,Tmax,Vname,Xi,Yold
      integer i,Idone,Iflinf,Ihflag,In,Incldh,Index,Intent,Iout,Ipunch,I
     &sect,Istats,Ititle,Itype,K,Lambda,Mode,Ncyc,Ncycls,Ncytot
      integer Noinch,Noruns,Npar,Nstep,Nvar
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      common/io/In,Iout,Ipunch
      
      
      
      
99001 format(1x,8G15.8)
99002 format(1x,g15.8)
99003 format(1x,8I14)
99004 format(5x,i6,5x,a6,5x,i6)
99005 format(7H IDONE=,i1,1H.)
      
      do 100 i=1,50
      write(Iout,99001)Pool0(i),Pool1(i),Delvar(i),Yold(i),D1var(i),D2va
     &r(i),D1vold(i),Xi(i)
100   continue
      write(Iout,99001)Fzero,(F1(i),i=1,4),Alpha,Convrg
      write(Iout,99005)Idone
      call matout(H,30,30,30,30)
      return
      
      end
C* :1 * 
      
