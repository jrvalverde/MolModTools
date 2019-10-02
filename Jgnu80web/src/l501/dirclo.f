
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dirclo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dirclo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "dirclo.web"
      subroutine dirclo(NBASIS,D,F,NSYMOP,NEQBAS,IOP3)
      implicit none
      integer Ibasd,Ibase,Icon,Ifil,Intcnt,Iq,Ireset,Irwc,Irwc1,Irwc2,Ir
     &wc3,Irweig,Irwf,Irwgen,Irwh,Irwibf,Irwlc,Irwle,Irwp,Irwpt
      integer Irws,Irwt,Irwtm,Irww,Ismode,Istat,Itotal,Iux,jdum,Kntt1,Kn
     &tt2,Last,Length,Limint,Mode,Nrpext,Nsq,Ntx,Nwiib,Nwpi
      integer NBASIS,NSYMOP
      integer IOP3(*),NEQBAS(150,*)
      double precision D(*),F(*)
      double precision zero
      integer anyd,Isym2e,Irwlp,Ntt,i,Ii,Iop
      integer Dbase,Dbasd
      common/iop/Iop(50)
      common/ia/Ii(257)
      common/ntt/Ntt,Length,Nsq
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Isym2e
      common/irw501/Irwgen,Irws,Irwh,Irwt,Irweig,Irwc,Irwp,Irwpt,Irwf,Ir
     &wc1,Irwc2,Irwc3,Irwtm,Irwibf(2),Irwle,Irwlc,Irwlp,Irww
      data zero/0.0D00/
      
      do 100 i=1,Ntt
      F(i)=zero
100   continue
      anyd=IOP3(25)
      call shell(D,F,IOP3,jdum)
      if(anyd.GT.11)call phoeni(D,F,IOP3,jdum)
      if(Isym2e.EQ.1)then
      call fsymm(NBASIS,F,NSYMOP,NEQBAS,Ii,D)
      do 150 i=1,Ntt
      F(i)=D(i)
150   continue
      endif
      call tread(Irwh,D,Ntt,1,Ntt,1,0)
      do 200 i=1,Ntt
      F(i)=F(i)+D(i)
200   continue
      call tread(Irwlp,D,Ntt,1,Ntt,1,0)
      return
      end
C* :1 * 
      
