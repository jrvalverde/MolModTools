
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dintrp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dintrp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "dintrp.web"
      double precision function dintrp(TABLE)
      implicit none
      double precision Ap,At,Bp,Bt,C0,C1,C2,C3,Cm1,Cm2,TABLE,x
      integer M
      dimension TABLE(*)
      common/mtpc/At,Bt,Ap,Bp,Cm1,C0,C1,C2,M
      equivalence(Cm2,Bp),(C3,Bt)
      
      
      
      
      x=Cm2*TABLE(M+1)+Cm1*TABLE(M+2)+C0*TABLE(M+3)+C1*TABLE(M+4)+C2*TAB
     &LE(M+5)+C3*TABLE(M+6)
      dintrp=x
      
      return
      
      end
C* :1 * 
      
