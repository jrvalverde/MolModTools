
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getngr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getngr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "getngr.web"
      subroutine getngr(NGR,NUMGR,NROT,MAPPER,MAPROT,JAT,NATOMS,NBASIS,N
     &OSYM)
      implicit none
      integer igr,In,Iout,Ipunch,JAT,MAPPER,MAPROT,NATOMS,NBASIS,nbefor,
     &nextat,nfunc,NGR,NOSYM,NROT,numfun,NUMGR
      dimension NGR(*),MAPROT(*),MAPPER(*)
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
99001 format(1H0,'ALIGNMENT PROBLEM 1 IN GETNGR'/)
99002 format(1H0,'ALIGNMENT PROBLEM 2 IN GETNGR'/)
      
      
      
      nbefor=MAPPER(JAT)-1
      nextat=MAPPER(JAT+1)-1
      if(JAT.EQ.NATOMS)nextat=NBASIS
      numfun=nextat-nbefor
      
      igr=1
      nfunc=0
100   if(nfunc.EQ.nbefor)then
      
      
      nfunc=0
      NUMGR=1
150   NGR(NUMGR)=MAPROT(igr)
      nfunc=nfunc+NGR(NUMGR)
      if(nfunc.EQ.numfun)return
      igr=igr+1
      NUMGR=NUMGR+1
      if(igr.LE.NROT)goto 150
      else
      nfunc=nfunc+MAPROT(igr)
      igr=igr+1
      if(igr.LT.NROT+1)goto 100
      NOSYM=1
      write(Iout,99001)
      return
      endif
      NOSYM=1
      write(Iout,99002)
      return
      
      end
C* :1 * 
      
