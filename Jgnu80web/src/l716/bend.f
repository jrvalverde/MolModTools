
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bend"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bend.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "bend.web"
      subroutine bend(NOINT,I,J,K,B,IB,C,NPARM)
      implicit none
      double precision B,C,dji,djisq,djk,djksq,dotj,eji,ejk,gsqrt,one,rj
     &i,rjk,sinj,zero
      integer I,iaind,IB,J,jaind,K,kaind,m,NOINT,NPARM
      dimension B(3,4,NPARM),IB(4,NPARM),C(*)
      dimension rji(3),rjk(3),eji(3),ejk(3)
      data zero/0.D0/,one/1.D0/
      
      
      
      
      
      
      
      iaind=3*(I-1)
      jaind=3*(J-1)
      kaind=3*(K-1)
      IB(1,NOINT)=I
      IB(2,NOINT)=J
      IB(3,NOINT)=K
      djisq=zero
      djksq=zero
      do 100 m=1,3
      rji(m)=C(m+iaind)-C(m+jaind)
      rjk(m)=C(m+kaind)-C(m+jaind)
      djisq=djisq+rji(m)**2
      djksq=djksq+rjk(m)**2
100   continue
      dji=gsqrt(djisq)
      djk=gsqrt(djksq)
      dotj=zero
      do 200 m=1,3
      eji(m)=rji(m)/dji
      ejk(m)=rjk(m)/djk
      dotj=dotj+eji(m)*ejk(m)
200   continue
      sinj=gsqrt(one-dotj**2)
      do 300 m=1,3
      B(m,3,NOINT)=((dotj*ejk(m)-eji(m)))/(djk*sinj)
      B(m,1,NOINT)=((dotj*eji(m)-ejk(m)))/(dji*sinj)
      B(m,2,NOINT)=-B(m,1,NOINT)-B(m,3,NOINT)
300   continue
      
      return
      
      end
C* :1 * 
      
