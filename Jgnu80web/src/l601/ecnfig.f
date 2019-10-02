
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ecnfig"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ecnfig.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "ecnfig.web"
      subroutine ecnfig(MULTIP,NAE,NBE,NBASIS,ORBSYM)
      implicit none
      double precision chelt,Chrtbl,one,rcnfig,Symops
      integer i,In,iorb,Iout,Iprmut,Ipunch,irep,irpsav,irwrep,irwsm,iscf
     &,istr,isym,jstr,k,ksp,kspmax,lbl,Lblrep,lc
      integer len,lim,lrwrep,mc,MULTIP,NAE,NBASIS,NBE,nocc,norb,Nreps,Ns
     &ymop
      integer ORBSYM
      dimension ORBSYM(*),irwsm(2),nocc(2),rcnfig(10)
      dimension istr(8),jstr(40)
      common/io/In,Iout,Ipunch
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data irwrep,lrwrep,irwsm(1),irwsm(2)/562,767,563,564/
      data one/1.D0/
      
99001 format(' UNABLE TO DETERMINE ELECTRONIC STATE.')
      
      nocc(1)=NAE
      nocc(2)=NBE
      call ilsw(2,1,iscf)
      if(iscf.LT.2)then
      kspmax=iscf+1
      call tquery(irwrep,len)
      if(len.NE.lrwrep)return
      call tread(irwrep,Nsymop,lrwrep,1,lrwrep,1,0)
      if(Nsymop.EQ.0)return
      if(Nreps.EQ.0)return
      do 50 i=1,Nsymop
      rcnfig(i)=one
50    continue
      call tquery(irwsm(1),len)
      len=min0(len,NBASIS)
      do 100 ksp=1,kspmax
      call tread(irwsm(ksp),ORBSYM,len,1,len,1,0)
      norb=nocc(ksp)
      do 80 iorb=1,norb
      lbl=ORBSYM(iorb)
      do 60 i=1,Nsymop
      chelt=Chrtbl(i,lbl)
      if(iscf.EQ.0)chelt=chelt**2
      rcnfig(i)=rcnfig(i)*chelt
60    continue
80    continue
100   continue
      do 150 irep=1,Nreps
      irpsav=irep
      do 120 isym=1,Nsymop
      if(rcnfig(isym).NE.Chrtbl(isym,irep))goto 150
120   continue
      goto 200
      
150   continue
      endif
      write(Iout,99001)
      goto 300
      
200   lc=0
      call decchr(MULTIP,istr,lc)
      call putchr('-',istr,lc)
      mc=0
      if(irpsav.NE.1)then
      lim=irpsav-1
      do 250 k=1,lim
      call skip(2,Lblrep,mc)
250   continue
      endif
      call getb(2,jstr,len,Lblrep,mc)
      
      call putb(jstr,len,istr,lc)
      call captlz(istr,istr,lc)
      mc=0
      call putbc(' THE ELECTRONIC STATE IS ',25,jstr,mc)
      call putb(istr,lc,jstr,mc)
      call putchr('.',jstr,mc)
      call strout(Iout,jstr,mc,1)
300   return
      
      end
C* :1 * 
      
