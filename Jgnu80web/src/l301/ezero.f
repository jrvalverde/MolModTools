
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ezero"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ezero.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "ezero.web"
      subroutine ezero(E1,CS1,NGAUSS,NSPLIT,IA)
      implicit none
      double precision CS1,E1
      integer IA,NGAUSS,NSPLIT
      dimension E1(6),CS1(6)
      
      if(IA.LE.1)then
      if(NSPLIT.NE.1)call berror(2)
      E1(1)=1.300773400D01
      CS1(1)=3.349460434D-02
      E1(2)=1.962079420D00
      CS1(2)=2.347269535D-01
      E1(3)=4.445289530D-01
      CS1(3)=8.137573262D-01
      E1(4)=1.219491560D-01
      CS1(4)=1.000000000D00
      return
      endif
      
      if(NSPLIT.NE.1)call berror(2)
      E1(1)=38.421634D0
      CS1(1)=0.023766D0
      E1(2)=5.77803D0
      CS1(2)=0.154679D0
      E1(3)=1.241774D0
      CS1(3)=0.469630D0
      E1(4)=0.297964D0
      CS1(4)=1.0D0
      return
      
      end
C* :1 * 
      
