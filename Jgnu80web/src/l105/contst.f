
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 contst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "contst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "contst.web"
      logical function contst(IOP,ISTEP,NVAR,RMSF,FMAX,RMSD,DMAX,NSTEP,T
     &HRESH,IHDG)
      implicit none
      double precision con,DMAX,FMAX,four,onept5,RMSD,RMSF,THRESH
      integer IHDG,In,IOP,Iout,Ipunch,ireslt,ISTEP,NSTEP,NVAR
      logical test
      dimension IOP(50)
      dimension ireslt(3)
      common/io/In,Iout,Ipunch
      data onept5,four/1.5D0,4.0D0/
      
      
      
      
      
      
      
99001 format(1x,8x,'ITEM',8x,5x,2x,'VALUE',3x,2x,'THRESHOLD',2x,'CONVERG
     &ED'/)
99002 format(1x,'MAXIMUM FORCE       ',5x,f8.6,5x,f8.6,5x,3A1)
99003 format(1x,'RMS     FORCE       ',5x,f8.6,5x,f8.6,5x,3A1)
99004 format(1x,'MAXIMUM DISPLACEMENT',5x,f8.6,5x,f8.6,5x,3A1)
99005 format(1x,'RMS     DISPLACEMEMT',5x,f8.6,5x,f8.6,5x,3A1)
99006 format(1x,'EXIT NOT ALLOWED ON THE FIRST STEP')
99007 format(1H0,'*** OPTIMIZATION ABORTED, TOO MANY STEPS ***')
99008 format(1H0,'*** OPTIMIZATION COMPLETED ***')
      
      
      contst=.TRUE.
      write(Iout,99001)
      
      
      test=RMSF.LT.THRESH
      contst=contst.AND.test
      call convgd(RMSF,THRESH,ireslt)
      write(Iout,99003)RMSF,THRESH,ireslt
      
      
      con=onept5*THRESH
      test=FMAX.LT.con
      contst=contst.AND.test
      call convgd(FMAX,con,ireslt)
      write(Iout,99002)FMAX,con,ireslt
      
      
      con=four*THRESH
      test=RMSD.LT.con
      contst=contst.AND.test
      call convgd(RMSD,con,ireslt)
      write(Iout,99005)RMSD,con,ireslt
      
      
      con=onept5*con
      test=DMAX.LT.con
      contst=contst.AND.test
      call convgd(DMAX,con,ireslt)
      write(Iout,99004)DMAX,con,ireslt
      
      
      test=ISTEP.GT.1
      contst=contst.AND.test
      if(.NOT.test)write(Iout,99006)
      
      
      test=ISTEP.GT.NSTEP
      contst=contst.OR.test
      if(test)write(Iout,99007)
      
      if(.NOT.contst)return
      
      
      if(.NOT.test)write(Iout,99008)
      IHDG=1
      if(test)IHDG=2
      if(test)call ilsw(1,25,1)
      
      return
      
      end
C* :1 * 
      
