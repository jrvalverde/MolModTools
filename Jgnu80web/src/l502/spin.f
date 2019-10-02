
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 spin"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "spin.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "spin.web"
      subroutine spin(NBASIS,NAE,NBE,MAXNBF,A,U,AA,S2)
      implicit none
      double precision A,AA,four,gfloat,gsqrt,one,pt25,pt5,s,S2,sum,U,ze
     &ro
      integer i,irwca,irwcb,irws,irwur,j,MAXNBF,NAE,NBASIS,NBE
      dimension A(MAXNBF,MAXNBF),U(MAXNBF,MAXNBF),AA(*)
      data pt5/0.5D0/,pt25/0.25D0/,zero/0.0D0/,one/1.0D0/,four/4.0D0/
      data irws/514/,irwca/524/,irwcb/526/,irwur/540/
      
      
      
      
      
      
      call tread(irws,A,MAXNBF,MAXNBF,NBASIS,NBASIS,1)
      call tread(irwca,U,MAXNBF,MAXNBF,NBASIS,NBASIS,0)
      call matrec(U,A,AA,MAXNBF,NBASIS,NBASIS,NBASIS,2)
      call tread(irwcb,A,MAXNBF,MAXNBF,NBASIS,NBASIS,0)
      call matrec(U,A,AA,MAXNBF,NBASIS,NBASIS,NBASIS,1)
      call twrite(irwur,U,MAXNBF,MAXNBF,NBASIS,NBASIS,0)
      
      sum=zero
      if(NAE*NBE.GT.0)then
      do 50 i=1,NAE
      do 20 j=1,NBE
      sum=sum+(U(i,j)*U(i,j))
20    continue
50    continue
      endif
      
      S2=pt5*gfloat(NAE+NBE)+pt25*((gfloat(NAE-NBE))**2)-sum
      s=-pt5+pt5*gsqrt(one+four*S2)
      
      return
      
      end
C* :1 * 
      
