
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dmpint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dmpint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "dmpint.web"
      subroutine dmpint(KOP,IBUF,DBUF)
      implicit none
      double precision DBUF,Valint
      integer Ia,Ib,Ibasd,Ibase,Ibfpad,IBUF,Icon,Ifil,iflst,In,intape,In
     &tcnt,Iout,Ipunch,Iq,iqbufr,iqproc,Ireset,Ismode,Istat
      integer Itotal,itst,Iux,Ja,Jb,Kb,Kntt1,Kntt2,KOP,Last,Lb,Limint,Mi
     &ndx,Mode,next,nfile,Nrpext,Ntx,Nwiib,Nwpi
      integer P,Q,R,Sindx
      integer Dbase,dcount,Dbasd
      dimension Ia(2)
      dimension IBUF(*),DBUF(*)
      common/packed/Ib,Jb,Kb,Lb,Valint,Ja
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/io/In,Iout,Ipunch
      equivalence(Valint,Ia(1)),(P,Ib),(Q,Jb),(R,Jb,Mindx),(Sindx,Lb)
      
      
      
      
      
      
      
      
      
99001 format(/1x,'DMPINT, READING INTEGRALS'/)
99002 format(/1x,'END OF INTEGRAL PRINTING'/)
      
      
      write(Iout,99001)
      iqbufr=1
      iqproc=2
      Intcnt=0
      intape=Iux(2)
      Ntx=1
      call iwind(intape)
      call iread(intape,iqbufr,DBUF)
      Ifil=1
      
100   call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(IBUF(Ibase),iflst)
      write(6,*)iflst
      if(iflst.EQ.0)then
      itst=Ifil-(nfile+Ntx*Icon)
      if(itst.EQ.0)then
      call iwind(intape)
      Ntx=Ntx+1
      next=Iux(Ntx+1)
      call iwind(next)
      intape=next
      endif
      call iread(intape,iqbufr,DBUF)
      Ifil=Ifil+1
      endif
      
      
      call intprt(KOP,IBUF,IBUF)
      
      if(iflst.LE.0)goto 100
      write(Iout,99002)
      call irel(intape)
      call iwind(intape)
      return
      
      end
C* :1 * 
      
