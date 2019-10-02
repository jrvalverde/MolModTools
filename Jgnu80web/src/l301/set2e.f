
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 set2e"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "set2e.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "set2e.web"
      subroutine set2e(IOP)
      implicit none
      integer i,I2e,Ibasd,Ibase,Ibf,Icon,Icount,Ifil,In,Intcnt,IOP,Iout,
     &Ipunch,Iq,Ireset,irwibf,Ismode,Istat,Isym2e,Itotal
      integer Iudum,Iunit,Iux,Kntt1,Kntt2,Last,lenibf,Limint,Mode,nosym,
     &Nrpext,Ntx,Nwiib,Nwpi
      integer Dbase,Dbasd,Dcount
      integer onoff
      dimension IOP(50)
      dimension Ibf(1)
      common/io/In,Iout,Ipunch
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Isym2e
      common/munit/Iunit(11),I2e,Iudum(8)
      equivalence(Ismode,Ibf(1))
      equivalence(Kntt1,Icount),(Kntt2,Dcount)
      data irwibf/8/,lenibf/15/
      
      
      
      
      
      
      
99001 format(12H RAFFENETTI ,i1,17H INTEGRAL FORMAT.)
99002 format(25H REGULAR INTEGRAL FORMAT.)
99003 format(' DUMP FROM IBF:'/' ISMODE=',i1,'   MODE=',i1,'   ISTAT=',i
     &1,'   LAST=',i2)
99004 format(' NTX=',i1,'   IUX(1-5)=',5I3,'   ICON=',i6,'   NRPEXT=',i4
     &)
99005 format(' KNTT1=',i4,'   KNTT2=',i4/' IBASE=',i4,'   IBASD(1-2)=',2
     &I5,' DBASE=',i4,'   DBASD(1-2)=',2I5/' IRESET(1-2)=',2I5)
99006 format(' IQ=',i1,'   IFIL=',i4,'   INTCNT=',i2,'   ITOTAL=',i3)
99007 format(' LIMINT=',i5,'   NWPI=',i1,'   NWIIB=',i5,'   ISYM2E=',i1)
99008 format(' SET2E ... FATAL ... NWIIB IS ODD:',i9)
99009 format(' TWO-ELECTRON INTEGRAL SYMMETRY IS TURNED ON.')
99010 format(' TWO-ELECTRON INTEGRAL SYMMETRY IS TURNED OFF.')
      
      
      Nwiib=4760
      Iux(1)=19
      Iux(2)=I2e
      Iux(3)=4
      Iux(4)=8
      Iux(5)=9
      Icon=100000
      Nrpext=20
      
      
      
      
      
      
      call ilsw(2,1,i)
      i=mod(i,2)
      if(IOP(11).NE.9)Ismode=IOP(11)
      if(IOP(11).EQ.9.AND.i.EQ.0)Ismode=1
      if(IOP(11).EQ.9.AND.i.EQ.1)Ismode=2
      if(IOP(45).NE.0)Ismode=0
      if(IOP(45).NE.0)IOP(11)=0
      
      Mode=Ismode+1
      Istat=1
      Last=IOP(25)
      
      
      
      
      
      Iq=0
      Ifil=0
      Intcnt=0
      Itotal=0
      
      
      
      
      
      
      if(mod(Nwiib,2).NE.0)then
      write(Iout,99008)Nwiib
      call lnk1e
      endif
      
      if(Ismode.LE.0)then
      
      Nwpi=3
      Limint=(Nwiib-1)/Nwpi
      Kntt1=1
      Kntt2=Nwiib-mod((Nwiib-1),Nwpi)-Nwpi
      Ibasd(1)=1
      Ibasd(2)=Ibasd(1)+Nwiib
      Dbasd(1)=0
      Dbasd(2)=0
      Ibase=Ibasd(1)
      write(Iout,99002)
      else
      
      Nwpi=1+2*Ismode
      Limint=(Nwiib-1)/Nwpi
      Ibasd(1)=1
      Ibasd(2)=Ibasd(1)+Nwiib
      Dbasd(1)=(Limint+2)/2
      Dbasd(2)=Dbasd(1)+(Nwiib/2)
      Ibase=Ibasd(1)
      Dbase=Dbasd(1)
      Dcount=1
      Icount=1
      write(Iout,99001)Ismode
      endif
      Ireset(1)=Kntt1
      Ireset(2)=Kntt2
      
      
      
      
      call ilsw(2,26,nosym)
      Isym2e=0
      if(nosym.EQ.0)then
      if(IOP(30).EQ.1)Isym2e=1
      endif
      if(Isym2e.EQ.0)write(Iout,99010)
      if(Isym2e.EQ.1)write(Iout,99009)
      if(IOP(34).NE.0)then
      write(Iout,99003)Ismode,Mode,Istat,Last
      write(Iout,99004)Ntx,Iux,Icon,Nrpext
      write(Iout,99005)Kntt1,Kntt2,Ibase,Ibasd,Dbase,Dbasd,Ireset
      write(Iout,99006)Iq,Ifil,Intcnt,Itotal
      write(Iout,99007)Limint,Nwpi,Nwiib,Isym2e
      endif
      call twrite(irwibf,Ismode,lenibf,1,lenibf,1,0)
      
      return
      
      end
C* :1 * 
      
