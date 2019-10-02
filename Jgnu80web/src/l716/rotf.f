
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rotf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rotf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "rotf.web"
      subroutine rotf(NATOMS,TR,FIN,FOUT)
      implicit none
      double precision FIN,FOUT,TR,tx,ty,tz
      integer i,iat,NATOMS
      dimension TR(3,3),FIN(*),FOUT(*)
      
      
      
      
      
      
      do 100 iat=1,NATOMS
      i=3*(iat-1)
      tx=TR(1,1)*FIN(i+1)+TR(2,1)*FIN(i+2)+TR(3,1)*FIN(i+3)
      ty=TR(1,2)*FIN(i+1)+TR(2,2)*FIN(i+2)+TR(3,2)*FIN(i+3)
      tz=TR(1,3)*FIN(i+1)+TR(2,3)*FIN(i+2)+TR(3,3)*FIN(i+3)
      FOUT(1+i)=tx
      FOUT(2+i)=ty
      FOUT(3+i)=tz
100   continue
      
      return
      
      end
C* :1 * 
      
