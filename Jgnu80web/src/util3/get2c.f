
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 get2c"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "get2c.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "get2c.web"
      subroutine get2c(TWOC,X,CONST,A,TWOPT2,INCP)
      implicit none
      double precision A,CONST,g,TWOC,TWOPT2,X,Xint,Zero
      integer INCP,Lamax,Lbmax,Lpmax,lpnew,Maxdum
      dimension TWOC(9),A(45)
      dimension g(9)
      common/int/Zero,Xint(12)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      
      
      
      
      
      
      
      
      
      lpnew=Lpmax+INCP
      g(1)=CONST
      if(lpnew.GT.1)then
      g(2)=TWOPT2*X*g(1)
      if(lpnew.NE.2)then
      g(3)=TWOPT2*(X*g(2)-g(1))
      if(lpnew.NE.3)then
      g(4)=TWOPT2*(X*g(3)-Xint(2)*g(2))
      if(lpnew.NE.4)then
      g(5)=TWOPT2*(X*g(4)-Xint(3)*g(3))
      if(lpnew.NE.5)then
      g(6)=TWOPT2*(X*g(5)-Xint(4)*g(4))
      if(lpnew.NE.6)then
      g(7)=TWOPT2*(X*g(6)-Xint(5)*g(5))
      if(lpnew.NE.7)then
      g(8)=TWOPT2*(X*g(7)-Xint(6)*g(6))
      if(lpnew.NE.8)then
      g(9)=TWOPT2*(X*g(8)-Xint(7)*g(7))
      
      
      TWOC(9)=g(1)*A(37)+g(3)*A(39)+g(5)*A(41)+g(7)*A(43)+g(9)*A(45)
      endif
      TWOC(8)=g(2)*A(30)+g(4)*A(32)+g(6)*A(34)+g(8)*A(36)
      endif
      TWOC(7)=g(1)*A(22)+g(3)*A(24)+g(5)*A(26)+g(7)*A(28)
      endif
      TWOC(6)=g(2)*A(17)+g(4)*A(19)+g(6)*A(21)
      endif
      TWOC(5)=g(1)*A(11)+g(3)*A(13)+g(5)*A(15)
      endif
      TWOC(4)=g(2)*A(8)+g(4)*A(10)
      endif
      TWOC(3)=g(1)*A(4)+g(3)*A(6)
      endif
      TWOC(2)=g(2)*A(3)
      endif
      TWOC(1)=g(1)
      return
      
      end
C* :1 * 
      
