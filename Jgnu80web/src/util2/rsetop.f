
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rsetop"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rsetop.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "rsetop.web"
      subroutine rsetop(NBASIS,PA,PB)
      implicit none
      integer i,j,k,NBASIS
      double precision PA,PB,pspin,ptotl
      dimension PA(*),PB(*)
      
      
      
      
      
      
      k=0
      do 100 i=1,NBASIS
      do 50 j=1,i
      k=k+1
      ptotl=PA(k)+PB(k)
      pspin=PA(k)-PB(k)
      PA(k)=ptotl+ptotl
      PB(k)=pspin+pspin
50    continue
      PA(k)=ptotl
      PB(k)=pspin
100   continue
      
      return
      
      end
C* :1 * 
      
