
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vibsym"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vibsym.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "vibsym.web"
      subroutine vibsym(VIBS,LEN,NCOL,SYMVIB,TABLE,A,AA,AMASS)
      implicit none
      real A,AA,AMASS,SYMVIB,TABLE,VIBS
      integer irwrep,LEN,lenf,lrwrep,NCOL,nosym
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      dimension VIBS(*),A(*),TABLE(*),AA(*),AMASS(*),SYMVIB(*)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data irwrep/562/,lrwrep/767/
      
      
      call tquery(irwrep,lenf)
      if(lenf.LE.0)return
      
      call tread(irwrep,Nsymop,lrwrep,1,lrwrep,1,0)
      
      
      call vibtbl(VIBS,A,TABLE,AA,LEN,NCOL,AMASS)
      
      call getrep(TABLE,SYMVIB,NCOL,0,nosym)
      return
      
      end
C* :1 * 
      
