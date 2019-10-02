
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 contt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "contt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "contt.web"
      subroutine contt(TOANG)
      implicit none
      double precision Alpha,Convrg,D1var,D1vold,D2var,Delvar,F,F1,Fzero
     &,gen,H,Pool0,Pool1,rmsf,rmsfp,Tcurcy,Telcur,Teltot,thrsh,Tlstcy
      double precision Tmax,TOANG,Vname,Xi,Yold
      integer Idone,Iflinf,Ifpgen,Ihflag,In,Incldh,Index,Intent,Iofp,Iog
     &en,Iout,Ipunch,Irmsf,Isect,Istats,Ititle,Itype,K,Lambda,Mode
      integer Ncyc,Ncycls,Ncytot,Noinch,Noruns,Npar,Nstep,Nvar
      dimension gen(47)
      common/fpinfo/Pool0(30),Pool1(30),Delvar(30),Yold(30),D1var(30),D2
     &var(30),D1vold(30),Xi(30),Fzero,F1(4),F,Alpha,Convrg,Teltot,Telcur
     &,Tlstcy,Tmax,Tcurcy,Iflinf,K,Nvar,Npar,Itype,Ncycls,Isect,Ncyc,Nor
     &uns,Ncytot,Ititle(8),Mode,Nstep,Index,Lambda,Ihflag,Idone,H(30,30)
     &,Noinch,Incldh,Vname(30),Istats,Intent
      common/irw/Iogen,Irmsf,Iofp,Ifpgen
      common/io/In,Iout,Ipunch
      data thrsh/0.0003D0/
      
      
      
      
      
      
99001 format(1x,'AT CYCLE',i3,' THE RMS FORCE IS ',f8.5,' HARTREE / ','B
     &OHR OR / RADIAN')
99002 format(1x,'FLETCHER-POWELL OPTIMIZATION TERMINATED')
99003 format(1x,39('='))
99004 format(1x,'FORCES BELOW THRESHOLD AFTER',i3,' CYCLES')
99005 format(1x,63('='))
99006 format(1x,'MAXIMUM NUMBER OF CYCLES EXCEEDED -- FORCES NOT ','BELO
     &W THRESHOLD')
99007 format(1x,'FLAG RESET TO TURN OFF ARCHIVING')
      
      
      if(Itype.EQ.1)then
      elseif(Itype.EQ.3)then
      goto 100
      endif
      
      rmsf=rmsfp(Nvar,1)
      write(Iout,99001)K,rmsf
      call tread(Ifpgen,gen,47,1,47,1,0)
      gen(Irmsf)=rmsf
      call twrite(Ifpgen,gen,47,1,47,1,0)
      if(rmsf.LE.thrsh)then
      write(Iout,99003)
      write(Iout,99002)
      write(Iout,99004)K
      write(Iout,99003)
      call fpexit
      goto 200
      endif
      
100   if(K.GE.Ncycls)then
      write(Iout,99005)
      write(Iout,99002)
      write(Iout,99006)
      write(Iout,99007)
      call ilsw(1,25,0)
      write(Iout,99005)
      call fpexit
      endif
      
200   return
      
      end
C* :1 * 
      
