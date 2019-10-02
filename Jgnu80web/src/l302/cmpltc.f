
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cmpltc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cmpltc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "cmpltc.web"
      subroutine cmpltc(MAXDIM,NBASIS,NSTART,U,V,S)
      implicit none
      double precision delta,gsqrt,one,S,U,V
      integer i,iend,jadd,jorth,MAXDIM,NBASIS,NSTART
      dimension S(1,1)
      dimension U(MAXDIM,MAXDIM),V(MAXDIM,MAXDIM)
      data one/1.0D0/
      
      
      
      
      
      
      iend=NSTART-2
      
      do 100 jorth=NSTART,NBASIS
      iend=iend+1
      do 50 jadd=1,iend
      call smults(MAXDIM,NBASIS,jadd,jorth,delta,U,S)
      
      do 20 i=1,NBASIS
      V(i,jorth)=U(i,jorth)-delta*V(i,jadd)
20    continue
      
      call smults(MAXDIM,NBASIS,jorth,jorth,delta,U,S)
      
      delta=one/gsqrt(delta)
      
      do 40 i=1,NBASIS
      V(i,jorth)=V(i,jorth)*delta
40    continue
      
50    continue
100   continue
      return
      
      end
C* :1 * 
      
