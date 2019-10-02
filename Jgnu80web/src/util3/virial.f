
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 virial"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "virial.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "virial.web"
      subroutine virial(NBASIS,E,A,B,IRWPT,IRWT,VIR,T)
      implicit none
      double precision A,B,E,scftrc,T,VIR
      integer IRWPT,IRWT,NBASIS,ntt
      dimension A(*),B(*)
      
      
      
      ntt=(NBASIS*(NBASIS+1))/2
      call tread(IRWPT,A,ntt,1,ntt,1,0)
      call tread(IRWT,B,ntt,1,ntt,1,0)
      T=scftrc(A,B,NBASIS,1)
      VIR=-(E-T)/T
      return
      
      end
C* :1 * 
      
