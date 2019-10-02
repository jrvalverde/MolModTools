
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 labscf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "labscf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "labscf.web"
      subroutine labscf(IBUF,IFLST)
      implicit none
      integer Ibasd,Ibase,Ibfpad,IBUF,Icon,Ifil,IFLST,In,Intcnt,Iop,Iout
     &,Ipunch,Iq,Ireset,Ismode,Istat,Itotal,Iux,Kntt1,Kntt2
      integer Last,Limint,Mode,Nrpext,Ntx,Nwiib,Nwpi
      integer Dbase,Dbasd,fld
      dimension IBUF(*)
      common/iop/Iop(50)
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/io/In,Iout,Ipunch
      
99001 format(' BUFFER CONTROL WORD: ',i12,'   IFLST=',i2,'   KNTT1=',i4,
     &'   KNTT2=',i4)
      
      IFLST=fld(0,1,IBUF(1))
      Kntt1=fld(1,12,IBUF(1))
      Kntt2=fld(13,12,IBUF(1))
      
      if(Iop(34).GE.1)write(Iout,99001)IBUF(1),IFLST,Kntt1,Kntt2
      
      
      return
      
      end
C* :1 * 
      
