
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getrep"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getrep.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "getrep.web"
      subroutine getrep(TABLE,ORBSYM,NB,MODE,NOSYM)
      implicit none
      double precision gabs,one,TABLE,thrsh,tmp
      integer i,iop,irep,isave,k,lblunk,lcur,len,lim,lstunk,maxrep,mcur,
     &MODE,NB,NOSYM,nunk
      double precision Symops,Chrtbl
      integer ocur
      integer Nsymop,Nreps,Lblrep,Iprmut
      dimension TABLE(NB,*)
      integer ORBSYM(NB)
      dimension lblunk(15),tmp(8)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data one/1.0D0/,thrsh/1.0D-4/
      data maxrep/15/
      data lstunk/'??'/
      data lblunk/'?A','?B','?C','?D','?E','?F','?G','?H','?I','?J','?K'
     &,'?L','?M','?N','?O'/
      
      
      
      nunk=0
      ocur=0
      mcur=0
      if(Nreps.GT.0)then
      
      do 50 i=1,Nreps
      call skip(2,Lblrep,mcur)
50    continue
      else
      NOSYM=1
      nunk=1
      Nreps=1
      call putb(lblunk(1),2,Lblrep,mcur)
      call putdel(2,Lblrep,mcur)
      do 100 i=1,Nsymop
      Chrtbl(i,1)=one
100   continue
      endif
      
      do 300 i=1,NB
      do 150 irep=1,Nreps
      
      do 120 iop=1,Nsymop
      if(gabs(TABLE(i,iop)-Chrtbl(iop,irep)).GT.thrsh)goto 150
120   continue
      isave=irep
      goto 200
      
150   continue
      NOSYM=1
      if(Nreps.GE.maxrep)then
      
      call putb(lstunk,2,ORBSYM,ocur)
      call putdel(2,ORBSYM,ocur)
      else
      nunk=nunk+1
      call putb(lblunk(nunk),2,ORBSYM,ocur)
      call putdel(2,ORBSYM,ocur)
      Nreps=Nreps+1
      do 160 iop=1,Nsymop
      Chrtbl(iop,Nreps)=TABLE(i,iop)
160   continue
      call putb(lblunk(nunk),2,Lblrep,mcur)
      call putdel(2,Lblrep,mcur)
      endif
      goto 300
      
200   if(MODE.EQ.1)ORBSYM(i)=isave
      if(MODE.NE.1)then
      
      lcur=0
      if(isave.NE.1)then
      lim=isave-1
      do 210 k=1,lim
      call skip(2,Lblrep,lcur)
210   continue
      endif
      call getb(2,tmp,len,Lblrep,lcur)
      call putb(tmp,len,ORBSYM,ocur)
      call putdel(2,ORBSYM,ocur)
      endif
300   continue
      return
      
      end
C* :1 * 
      
