
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 halt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "halt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "halt.web"
      subroutine halt(WORD)
      implicit none
      double precision blank,WORD
      integer Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao
     &,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr
      
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      data blank/1H /
      
      if(WORD.EQ.blank)return
      write(Lfnpr,99001)WORD
      stop
      
99001 format(' Non-integer encountered when trying to read variable ','/
     &',a6,'/')
      end
C* :1 * 
      
