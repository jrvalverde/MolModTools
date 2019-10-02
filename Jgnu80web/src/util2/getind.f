
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getind"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getind.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "getind.web"
      subroutine getind(LAMAX,LBMAX,LCMAX,LDMAX)
      implicit none
      integer imax,Indc,indx,itotal,la,LAMAX,lap,lb,LBMAX,lbp,lc,LCMAX,l
     &cp,ld,LDMAX,ldp
      common/indc/Indc(256)
      
      
      
      itotal=0
      indx=1
      do 100 lap=1,LAMAX
      la=lap-1
      do 50 lbp=1,LBMAX
      lb=lbp-1
      do 20 lcp=1,LCMAX
      lc=lcp-1
      do 10 ldp=1,LDMAX
      ld=ldp-1
      imax=la+lb+lc+ld+1
      Indc(la*64+lb*16+lc*4+ld+1)=indx
      indx=indx+imax
      itotal=itotal+imax
10    continue
20    continue
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
