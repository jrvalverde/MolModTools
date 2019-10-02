
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 setrys"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "setrys.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "setrys.web"
      subroutine setrys
      implicit none
      integer Iadr,Lent,Lind
      double precision S,T2,Ycut
      common/t2/S(7),Ycut(7),T2(2884),Lent(7),Lind(7),Iadr(7)
      
      S(1)=0.25D0
      S(2)=0.367006838D0
      S(3)=0.180984215D0
      S(4)=0.116432928D0
      S(5)=0.084694160D0
      S(6)=0.066094029D0
      S(7)=0.053971558D0
      return
      
      end
C* :1 * 
      
