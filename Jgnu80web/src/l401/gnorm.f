
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gnorm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gnorm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "gnorm.web"
      subroutine gnorm(AR,AI,ICMP,NBASIS)
      implicit none
      double precision AI,AR,br,gsqrt,one,zero
      integer i,ICMP,NBASIS
      dimension AR(NBASIS),AI(NBASIS)
      data one/1.0D0/,zero/0.0D0/
      
      
      
      
      br=zero
      do 100 i=1,NBASIS
      br=br+AR(i)*AR(i)
      if(ICMP.NE.0)br=br+AI(i)*AI(i)
100   continue
      
      br=one/gsqrt(br)
      
      do 200 i=1,NBASIS
      AR(i)=AR(i)*br
      if(ICMP.NE.0)AI(i)=AI(i)*br
200   continue
      return
      
      end
C* :1 * 
      
