
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 corinp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "corinp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "corinp.web"
      subroutine corinp(IESS,ICOR)
      implicit none
      integer ICOR,IESS,irep,kchs,kcor,kdel,keywd,knbo,len,Lfnao,Lfnarc,
     &Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho
      integer Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr
      logical end,equal
      dimension keywd(6),kcor(4),kchs(4),kdel(4),knbo(4)
      
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      data kcor/1H$,1HC,1HO,1HR/,kchs/1H$,1HC,1HH,1HO/,kdel/1H$,1HD,1HE,
     &1HL/,knbo/1H$,1HN,1HB,1HO/
      
      
      if(ICOR.EQ.-1)return
      
      
      irep=1
      if(IESS.EQ.0)irep=0
      if(IESS.EQ.6)irep=0
      if(IESS.EQ.7)irep=0
      if(irep.EQ.0)rewind(Lfnin)
      
      
100   call strtin(Lfnin)
      len=6
      call hfld(keywd,len,end)
      if(equal(keywd,kcor,4))then
      
      
      ICOR=1
      return
      else
      if(.NOT.(equal(keywd,knbo,4)))then
      if(.NOT.(equal(keywd,kchs,4)))then
      if(.NOT.(equal(keywd,kdel,4)))then
      if(.NOT.(len.EQ.0.AND.end))goto 100
      goto 200
      endif
      endif
      endif
      
      
      if(irep.EQ.0)goto 100
      backspace(Lfnin)
      ICOR=0
      return
      endif
      
      
200   ICOR=0
      return
      end
C* :1 * 
      
