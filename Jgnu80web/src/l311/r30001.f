
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 r30001"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "r30001.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "r30001.web"
      subroutine r30001
      implicit none
      double precision Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,P11,P12,P13,P
     &21,P22,P23,P31,P32
      double precision P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33,Rab,Rabsq
     &,Rcd,Rcdsq,t1,t2,t3,X,X1,X2
      double precision X3,X4
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/gout/X1,X2,X3,X4,X(252)
      
      
      
      
      t1=X2
      t2=X3
      t3=X4
      X2=P11*t1+P21*t2+P31*t3
      X3=P12*t1+P22*t2+P32*t3
      X4=P13*t1+P23*t2+P33*t3
      return
      
      end
C* :1 * 
      
