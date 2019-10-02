
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vprod"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vprod.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "vprod.web"
      subroutine vprod(VP,X,Y)
      implicit none
      double precision VP,X,Y
      dimension VP(3),X(3),Y(3)
      
      
      
      
      
      VP(1)=X(2)*Y(3)-X(3)*Y(2)
      VP(2)=X(3)*Y(1)-X(1)*Y(3)
      VP(3)=X(1)*Y(2)-X(2)*Y(1)
      return
      
      end
C* :1 * 
      
