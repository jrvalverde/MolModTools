
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "getf.web"
      subroutine getf(LAMAX,LBMAX,PAXP,PBXP,FIP)
      implicit none
      double precision f,FIP,PAXP,PBXP
      integer ind,Indf,ip,ipmax,ipp,la,LAMAX,lap,lb,LBMAX,lbp
      dimension PAXP(*),PBXP(*),FIP(*)
      common/indf/Indf(16)
      
      
      
      
      do 100 lap=1,LAMAX
      la=lap-1
      
      do 50 lbp=1,LBMAX
      lb=lbp-1
      
      ind=Indf(4*la+lb+1)
      ipmax=la+lb+1
      
      do 20 ipp=1,ipmax
      ip=ipp-1
      FIP(ind)=f(ip,la,lb,PAXP,PBXP)
      ind=ind+1
20    continue
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
