
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trspn2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trspn2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "trspn2.web"
      subroutine trspn2(N,T)
      implicit none
      integer iloc,ipiv,ipivp,iswap,jloc,loc,N,nm1
      double precision swap,T
      dimension T(*)
      
      
      
      
      
      nm1=N-1
      loc=1
      do 100 ipiv=1,nm1
      iloc=loc
      jloc=loc
      ipivp=ipiv+1
      do 50 iswap=ipivp,N
      iloc=iloc+1
      jloc=jloc+N
      swap=T(iloc)
      T(iloc)=T(jloc)
      T(jloc)=swap
50    continue
      loc=loc+(N+1)
100   continue
      
      return
      
      end
C* :1 * 
      
