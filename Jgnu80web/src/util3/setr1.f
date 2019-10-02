
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 setr1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "setr1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "setr1.web"
      subroutine setr1
      implicit none
      integer Iadr,Lent,Lind
      double precision S,T2,W2,Ycut
      common/t2w21e/Lent(4),Lind(4),Iadr(4),S(4),Ycut(4),T2(1030),W2(103
     &0)
      
      S(1)=0.25D0
      S(2)=0.367006838D0
      S(3)=0.180984215D0
      S(4)=0.116432928D0
      return
      
      end
C* :1 * 
      
