
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nboinp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nboinp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "nboinp.web"
      subroutine nboinp(NBOOPT,IDONE)
      implicit none
      integer IDONE,irep,keywd,knbo,len,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm
     &,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnp
     &nh
      integer Lfnpnl,Lfnppa,Lfnpr,NBOOPT
      logical end,equal
      dimension NBOOPT(10)
      dimension keywd(6),knbo(4)
      
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      data knbo/1H$,1HN,1HB,1HO/
      
      
      if(NBOOPT(1).EQ.1)then
      IDONE=0
      return
      endif
      
      
      irep=1
      if(NBOOPT(10).EQ.0)irep=0
      if(NBOOPT(10).EQ.6)irep=0
      if(NBOOPT(10).EQ.7)irep=0
      if(irep.EQ.0)rewind(Lfnin)
      
      
100   call strtin(Lfnin)
      len=6
      call hfld(keywd,len,end)
      if(equal(keywd,knbo,4))then
      
      
      IDONE=0
      return
      elseif(.NOT.(len.EQ.0.AND.end))then
      goto 100
      endif
      
      
      if(irep.EQ.1)then
      rewind(Lfnin)
      irep=irep+1
      goto 100
      elseif(NBOOPT(10).EQ.0)then
      NBOOPT(1)=1
      IDONE=0
      else
      IDONE=1
      endif
      return
      end
C* :1 * 
      
