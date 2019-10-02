
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 get1cs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "get1cs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "get1cs.web"
      subroutine get1cs(F,CONST,COEF,INC)
      implicit none
      double precision COEF,CONST,F,F1,F10,F11,F12,F2,F3,F4,F5,F6,F7,F8,
     &F9,Zero
      integer INC,Lamax,Lbmax,Lpmax,lpnew,Maxdum
      dimension F(6)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/int/Zero,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12
      
      
      
      
      
      
      lpnew=(Lpmax+INC+1)/2
      F(1)=CONST
      if(lpnew.EQ.1)return
      F(2)=COEF*F(1)
      if(lpnew.EQ.2)return
      F(3)=COEF*F3*F(2)
      if(lpnew.EQ.3)return
      F(4)=COEF*F5*F(3)
      if(lpnew.EQ.4)return
      F(5)=COEF*F7*F(4)
      if(lpnew.EQ.5)return
      F(6)=COEF*F9*F(5)
      return
      
      end
C* :1 * 
      
