
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pack4"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pack4.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "pack4.web"
      subroutine pack4
      implicit none
      integer I,J,K,L,Ja
      double precision Valint
      integer shift,ieor
      common/packed/I,J,K,L,Valint,Ja
      Ja=0
      Ja=ieor(Ja,I)
      Ja=ieor(Ja,shift(J,8))
      Ja=ieor(Ja,shift(K,16))
      Ja=ieor(Ja,shift(L,24))
      return
      end
C* :1 * 
      
