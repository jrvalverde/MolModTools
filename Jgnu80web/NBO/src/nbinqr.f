
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbinqr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbinqr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "nbinqr.web"
      subroutine nbinqr(IDAR)
      implicit none
      integer IDAR,Inbo,Ionbo,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfn
     &mo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,
     &Lfnppa
      integer Lfnpr,Nav,NBDAR
      
      parameter(NBDAR=100)
      common/nbodaf/Inbo,Nav,Ionbo(NBDAR)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      if(IDAR.LT.1.OR.IDAR.GT.NBDAR)then
      write(Lfnpr,99001)IDAR,NBDAR
      stop
      endif
      
      if(Ionbo(IDAR).EQ.0)IDAR=0
      return
      
99001 format(/1x,'NBO DAF record out of range: IDAR = ',i4,'  NBDAR = ',
     &i4)
      end
C* :1 * 
      
