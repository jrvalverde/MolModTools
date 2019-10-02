
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 angles"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "angles.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "angles.web"
      subroutine angles(X,Y,Z,THETA,PHI)
      implicit none
      double precision conv,one,PHI,THETA,X,Y,Z,zero
      data zero,one/0.0D0,1.0D0/
      
      conv=180.0/(4.0*datan(one))
      if(X.EQ.zero.AND.Y.EQ.zero)then
      if(Z.GE.zero)then
      THETA=zero
      else
      THETA=180.0
      endif
      PHI=zero
      else
      THETA=dacos(Z)*conv
      if(THETA.GT.180.0)THETA=360.0-THETA
      PHI=datan2(Y,X)*conv
      if(PHI.LT.zero)PHI=PHI+360.0
      if(dabs(PHI-360.0).LT.0.05)PHI=zero
      if(dabs(THETA).LT.0.05)PHI=zero
      if(dabs(THETA-180.0).LT.0.05)PHI=zero
      endif
      return
      end
C* :1 * 
      
