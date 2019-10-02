
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 keypar"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "keypar.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "keypar.web"
      subroutine keypar(STRING,LEN,IFLG,LFN,READ,ERROR)
      implicit none
      integer ic,IFLG,il,ip,ir,iv,iw,kfull,klew,kval,LEN,LFN,Lfnao,Lfnar
     &c,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab
      integer Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lf
     &nppa,Lfnpr
      integer STRING(LEN)
      logical READ,ERROR
      
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      data iw,ir,ip,ic,iv,il/1HW,1HR,1HP,1HC,1HV,1HL/
      data kfull,kval,klew/4HFULL,3HVAL,3HLEW/
      
      
      
      
      
      
      
      
      
      
      ERROR=.FALSE.
      
      
      if(STRING(1).EQ.iw)then
      if(LEN.EQ.1)then
      IFLG=-LFN
      return
      endif
      if(LEN.GT.1)then
      call convin(STRING(2),LEN-1,IFLG,ERROR)
      if(ERROR)return
      if(IFLG.GT.1000)then
      write(Lfnpr,99001)
      write(Lfnpr,99002)IFLG
      stop
      endif
      IFLG=-IFLG
      endif
      
      
      elseif(STRING(1).EQ.ir)then
      if(.NOT.READ)then
      ERROR=.TRUE.
      return
      endif
      if(LEN.EQ.1)then
      IFLG=-LFN*1000
      return
      endif
      if(LEN.GT.1)then
      call convin(STRING(2),LEN-1,IFLG,ERROR)
      if(ERROR)return
      if(IFLG.GT.1000)then
      write(Lfnpr,99001)
      write(Lfnpr,99003)IFLG
      stop
      endif
      IFLG=-IFLG*1000
      endif
      
      
      elseif(STRING(1).EQ.ip)then
      if(STRING(2).EQ.iv)then
      IFLG=kval
      return
      endif
      if(STRING(2).EQ.il)then
      IFLG=klew
      return
      endif
      if(LEN.EQ.1)then
      IFLG=kfull
      return
      endif
      if(LEN.GT.1)then
      if(STRING(LEN).NE.ic)then
      call convin(STRING(2),LEN-1,IFLG,ERROR)
      else
      call convin(STRING(2),LEN-2,IFLG,ERROR)
      endif
      endif
      else
      ERROR=.TRUE.
      endif
      return
      
99001 format(/1x,'The NBO program will only communicate with external ',
     &'files 0 thru 999.')
99002 format(1x,'You''re attempting to write to file ',i6,'.')
99003 format(1x,'You''re attempting to read from file ',i6,'.')
      end
C* :1 * 
      
