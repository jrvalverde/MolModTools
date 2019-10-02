
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zsymb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zsymb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "zsymb.web"
      subroutine zsymb(ISYMB,IOP,ICHARG,MULTIP,SYMBLS,NAMCNT)
      implicit none
      double precision Alpha,Beta,Bl,fp,str,SYMBLS
      integer i,Ianz,ICHARG,ieof,if1,if2,In,IOP,iord,Iout,Ipunch,isubst,
     &ISYMB,Iz,Lalpha,Lbeta,Lbl,len,lenx,MULTIP
      integer NAMCNT,ncur,nsb,Nvar,Nz
      dimension IOP(50),str(10),SYMBLS(100)
      dimension NAMCNT(*)
      common/io/In,Iout,Ipunch
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvar
      
      
      
      
      
99001 format(1H0,' **** END OF FILE')
99002 format(1x,'   CHARGE =',i2,' MULTIPLICITY =',i2)
99003 format(1x,'SYMBOLIC Z-MATRIX')
99004 format(1H0,'  CHARGE, MULTIPLICITY CARD SEEMS DEFECTIVE:')
99005 format(1x,5x,'MULTIPLICITY IS ZERO')
99006 format(1x,5x,'CHARGE IS BOGUS.')
99007 format(1x,5x,'MULTIPLICITY IS BOGUS.')
99008 format(1H0,'THE MAXIMUM OF 50 Z-MATRIX CARDS HAS BEEN EXCEEDED')
99009 format('  THE NAME OF THE CENTER IS TOO LONG.')
      
      
      write(Iout,99003)
      call szprnt(0,0,0,0)
      Nz=0
      ISYMB=0
      nsb=0
      ncur=0
      call ffset(IOP(34))
      call ffread(ieof)
      if(ieof.NE.0)then
      write(Iout,99001)
      call lnk1e
      endif
      
      
      call ffget(str,len,ICHARG,fp,if1)
      call ffget(str,len,MULTIP,fp,if2)
      if(if1.EQ.iord('NUL'))ICHARG=0
      if(if1.NE.iord('NUL'))then
      
      if(if1.NE.iord('INT').OR.if2.NE.iord('INT').OR.MULTIP.LE.0)then
      
      write(Iout,99004)
      if(if1.NE.iord('INT'))write(Iout,99006)
      if(if1.NE.iord('INT'))call fferr(3HINT,if1)
      if(if2.NE.iord('INT'))write(Iout,99007)
      if(if2.NE.iord('INT'))call fferr(3HINT,if2)
      if(MULTIP.LE.0)write(Iout,99005)
      call lnk1e
      endif
      endif
      
      write(Iout,99002)ICHARG,MULTIP
      i=0
      if(MULTIP.GT.1)i=1
      call ilsw(1,1,i)
      
      
100   if(Nz.NE.0)call szprnt(2,0,0,0)
      
      call ffread(ieof)
      if(ieof.NE.0)then
      write(Iout,99001)
      call lnk1e
      endif
      
      call ffget(str,len,i,fp,if1)
      if(if1.NE.iord('END'))then
      if(if1.NE.iord('STR'))call fferr(3HSTR,if1)
      if(len.GT.4)write(Iout,99009)
      if(len.GT.4)call fferr(0,0)
      lenx=len
      call pad(str,lenx,2,1H )
      
      call putb(str,len,NAMCNT,ncur)
      call putdel(2,NAMCNT,ncur)
      
      call szprnt(1,str,len,4)
      Nz=Nz+1
      
      Ianz(Nz)=isubst(str)
      if(Nz.NE.1)then
      
      if(Nz.GT.50)then
      write(Iout,99008)
      call lnk1e
      endif
      
      call zcentr(Iz(Nz,1),NAMCNT,Nz)
      
      call zparm(Bl(Nz),Lbl(Nz),SYMBLS,nsb,ISYMB)
      if(Nz.GE.3)then
      
      call zcentr(Iz(Nz,2),NAMCNT,Nz)
      
      call zparm(Alpha(Nz),Lalpha(Nz),SYMBLS,nsb,ISYMB)
      if(Nz.GE.4)then
      
      call zcentr(Iz(Nz,3),NAMCNT,Nz)
      
      call zparm(Beta(Nz),Lbeta(Nz),SYMBLS,nsb,ISYMB)
      
      call ffget(str,len,i,fp,if1)
      if(if1.NE.iord('END').AND.if1.NE.iord('INT'))call fferr(3HINT,if1)
      if(if1.EQ.iord('END'))Iz(Nz,4)=0
      if(if1.EQ.iord('INT'))Iz(Nz,4)=i
      call szprnt(1,Iz(Nz,4),0,1)
      endif
      endif
      endif
      goto 100
      endif
      
      return
      
      end
C* :1 * 
      
