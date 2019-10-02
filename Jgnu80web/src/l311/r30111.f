
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 r30111"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "r30111.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "r30111.web"
      subroutine r30111
      implicit none
      double precision Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,P11,P12,P13,P
     &21,P22,P23,P31,P32
      double precision P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33,Rab,Rabsq
     &,Rcd,Rcdsq,t1,t2,t3,X
      integer i1,i2,i3,ind,j,k,l
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/gout/X(256)
      
      
      
      
      
      ind=0
      do 100 k=1,4
      do 50 l=1,4
      ind=ind+1
      i1=16+ind
      i2=32+ind
      i3=48+ind
      t1=X(i1)
      t2=X(i2)
      t3=X(i3)
      X(i1)=P11*t1+P21*t2+P31*t3
      X(i2)=P12*t1+P22*t2+P32*t3
      X(i3)=P13*t1+P23*t2+P33*t3
50    continue
100   continue
      ind=-12
      do 200 j=1,4
      ind=ind+12
      do 150 l=1,4
      ind=ind+1
      i1=4+ind
      i2=8+ind
      i3=12+ind
      t1=X(i1)
      t2=X(i2)
      t3=X(i3)
      X(i1)=P11*t1+P21*t2+P31*t3
      X(i2)=P12*t1+P22*t2+P32*t3
      X(i3)=P13*t1+P23*t2+P33*t3
150   continue
200   continue
      ind=-3
      do 300 j=1,4
      do 250 k=1,4
      ind=ind+4
      i1=1+ind
      i2=2+ind
      i3=3+ind
      t1=X(i1)
      t2=X(i2)
      t3=X(i3)
      X(i1)=P11*t1+P21*t2+P31*t3
      X(i2)=P12*t1+P22*t2+P32*t3
      X(i3)=P13*t1+P23*t2+P33*t3
250   continue
300   continue
      return
      
      end
C* :1 * 
      
