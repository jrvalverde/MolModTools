
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 unpck2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "unpck2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "unpck2.web"
      subroutine unpck2
      implicit none
      integer maski,maskj
      integer I,J,K,L,Ja
      double precision Valint
      integer shift,iand
      common/packed/I,J,K,L,Valint,Ja
      data maski,maskj/65535,-65536/
      I=iand(maski,Ja)
      J=shift(iand(maskj,Ja),-16)
      return
      end
C* :1 * 
      
