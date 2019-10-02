
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pack2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pack2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "pack2.web"
      subroutine pack2
      implicit none
      integer I,J,K,L,Ja
      double precision Valint
      integer shift,ieor
      common/packed/I,J,K,L,Valint,Ja
      Ja=0
      Ja=ieor(Ja,I)
      Ja=ieor(Ja,shift(J,16))
      return
      end
C* :1 * 
      
