
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 incrd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "incrd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "incrd.web"
      subroutine incrd(IOP6,ACURCY,IOP8,ENGCON,THRESH)
      implicit none
      double precision ACURCY,ecut,five,rhocon,rhocut,str,ten,tenm5,THRE
     &SH,zero
      integer ieof,if1,if2,In,intgr,IOP6,IOP8,iord,Iout,Ipunch,len
      integer ENGCON
      dimension str(10)
      common/io/In,Iout,Ipunch
      data five/5.0D0/,ten/10.0D0/,tenm5/1.0D-5/
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
      
      
99001 format(' UNEXPECTED END-OF-FILE IN INCRD.')
99002 format(' CANNOT INTERPRET FIELD IN INCRD.')
      
      
      
      if((IOP6.EQ.8).OR.(IOP8.EQ.7))then
      
      call ffset(0)
      call ffread(ieof)
      if(ieof.EQ.1)goto 100
      rhocut=zero
      call ffget(str,len,intgr,rhocut,if1)
      if(if1.NE.iord('NUL').AND.if1.NE.iord('FP'))call fferr('FP',if1)
      ecut=zero
      call ffget(str,len,intgr,ecut,if2)
      if(if2.NE.iord('END').AND.if2.NE.iord('NUL').AND.if2.NE.iord('FP')
     &)call fferr('FP',if2)
      endif
      
      
      ACURCY=five*tenm5
      if(IOP6.GT.0)then
      if(IOP6.NE.8)then
      
      ACURCY=ten**(-IOP6)
      else
      ACURCY=rhocon
      endif
      endif
      
      THRESH=tenm5
      ENGCON=IOP8
      if(IOP8.GT.0)then
      if(IOP8.NE.7)then
      
      ecut=ten**(-3-IOP8)
      else
      THRESH=ecut
      endif
      endif
      return
      
100   write(Iout,99001)
      call lnk1e
      write(Iout,99002)
      call lnk1e
      return
      
      end
C* :1 * 
      
