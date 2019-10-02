
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 labint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "labint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "labint.web"
      subroutine labint(IBUF,IFLAST)
      implicit none
      integer Ibasd,Ibase,Ibfpad,IBUF,Icon,Ifil,IFLAST,In,Intcnt,Iop,Iou
     &t,Ipunch,Iq,Ireset,Ismode,Istat,Itotal,Iux,k1,k2
      integer Kntt1,Kntt2,Last,Limint,Mode,Nrpext,Ntx,Nwiib,Nwpi
      integer Dbase,Dbasd
      dimension IBUF(*)
      common/iop/Iop(50)
      common/io/In,Iout,Ipunch
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      
99001 format(' BUFFER LABEL PACKED: ',i12,'   IFLST=',i2,'   KNTT1=',i4,
     &'   KNTT2=',i4)
      
      if(Ismode.LE.0)then
      k1=(Kntt1-1)/Nwpi
      k2=(Ireset(2)-Kntt2)/Nwpi
      else
      
      k1=Kntt1-1
      k2=0
      endif
      
      IBUF(1)=0
      if(IFLAST.NE.0)call lfld(0,1,IBUF(1),1)
      call lfld(1,12,IBUF(1),k1)
      call lfld(13,12,IBUF(1),k2)
      call lfld(25,4,IBUF(1),Mode)
      
      if(Iop(34).GE.1)write(Iout,99001)IBUF(1),IFLAST,k1,k2
      
      return
      
      end
C* :1 * 
      
