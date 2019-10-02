
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 triang"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "triang.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "triang.web"
      subroutine triang(MAXAP3,A,I,J,K,ALP,BET,GAM,DIJ,DIK,DJK)
      implicit none
      double precision A,ALP,BET,DIJ,DIK,DJK,dotij,dotik,dotjk,gacos,GAM
     &,gsqrt,xi,xj,xk,yi,yj,yk,zi,zj
      double precision zk
      integer I,J,K,MAXAP3
      dimension A(MAXAP3,3)
      
      
      
      
      
      xi=A(I,1)
      yi=A(I,2)
      zi=A(I,3)
      xj=A(J,1)
      yj=A(J,2)
      zj=A(J,3)
      xk=A(K,1)
      yk=A(K,2)
      zk=A(K,3)
      DIJ=gsqrt((xi-xj)**2+(yi-yj)**2+(zi-zj)**2)
      DIK=gsqrt((xi-xk)**2+(yi-yk)**2+(zi-zk)**2)
      DJK=gsqrt((xj-xk)**2+(yj-yk)**2+(zj-zk)**2)
      dotjk=(xj-xi)*(xk-xi)+(yj-yi)*(yk-yi)+(zj-zi)*(zk-zi)
      dotik=(xi-xj)*(xk-xj)+(yi-yj)*(yk-yj)+(zi-zj)*(zk-zj)
      dotij=(xi-xk)*(xj-xk)+(yi-yk)*(yj-yk)+(zi-zk)*(zj-zk)
      ALP=gacos(dotjk/(DIJ*DIK))
      BET=gacos(dotik/(DIJ*DJK))
      GAM=gacos(dotij/(DIK*DJK))
      return
      
      end
C* :1 * 
      
