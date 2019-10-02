
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 unpck4"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "unpck4.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "unpck4.web"
      subroutine unpck4
      implicit none
      integer maski,maskj,maskk,maskl
      integer I,J,K,L,Ja
      double precision Valint
      integer shift,iand
      common/packed/I,J,K,L,Valint,Ja
      data maski,maskj,maskk,maskl/255,65280,16711680,-16777216/
      I=iand(maski,Ja)
      J=shift(iand(maskj,Ja),-8)
      K=shift(iand(maskk,Ja),-16)
      L=shift(iand(maskl,Ja),-24)
      return
      end
C* :1 * 
      
