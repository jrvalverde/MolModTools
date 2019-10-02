
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gamgen"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gamgen.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "gamgen.web"
      subroutine gamgen
      implicit none
      double precision C,f,four,pt05,pt15,pt184,six,t,temp1,temp2,y
      integer i,ick,iogam,j,k
      dimension y(410),f(9)
      common/table/C(1200,6)
      data pt15/0.15D0/,pt05/0.05D0/
      data pt184/0.184D0/
      data six/6.0D0/,four/4.0D0/
      data iogam/3/
      
      call fmtset(0,0,0)
      
      t=-pt15
      do 100 i=1,404
      t=t+pt05
      call fmtgen(f,t,6,ick)
      C(i,2)=f(1)
      C(i,3)=f(2)
      C(i,4)=f(3)
      C(i,5)=f(4)
      C(i,6)=f(5)
      y(i)=f(6)
100   continue
      
      do 200 k=1,6
      do 150 i=1,400
      j=i+2
      if(k.NE.6)then
      temp1=C(j+1,k+1)+C(j-1,k+1)-(C(j,k+1)+C(j,k+1))
      temp2=six*C(j,k+1)-four*(C(j+1,k+1)+C(j-1,k+1))+C(j-2,k+1)+C(j+2,k
     &+1)
      C(i,k)=C(j,k+1)
      C(i+400,k)=C(j+1,k+1)-C(j,k+1)
      C(i+800,k)=(temp1-pt184*temp2)/six
      else
      temp1=y(j+1)+y(j-1)-(y(j)+y(j))
      temp2=six*y(j)-four*(y(j+1)+y(j-1))+y(j-2)+y(j+2)
      C(i,k)=y(j)
      C(i+400,k)=y(j+1)-y(j)
      C(i+800,k)=(temp1-pt184*temp2)/six
      endif
150   continue
200   continue
      
      call twrite(iogam,C,7200,1,7200,1,0)
      return
      
      end
C* :1 * 
      
