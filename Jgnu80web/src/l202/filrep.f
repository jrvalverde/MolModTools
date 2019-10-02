
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 filrep"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "filrep.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "filrep.web"
      subroutine filrep(T,LPERM,NATOMS)
      implicit none
      integer i,Isave,LPERM,n,NATOMS,Ncnt,Nsave
      double precision T(9)
      dimension LPERM(NATOMS)
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      common/reploc/Nsave,Ncnt,Isave(15)
      
      if(Nsave.EQ.0)return
      Ncnt=Ncnt+1
      do 100 i=1,Nsave
      n=i
      if(Isave(i).EQ.Ncnt)goto 200
100   continue
      return
      
200   do 300 i=1,9
      Symops(i,n)=T(i)
300   continue
      
      do 400 i=1,NATOMS
      Iprmut(i,n)=LPERM(i)
400   continue
      return
      
      end
C* :1 * 
      
