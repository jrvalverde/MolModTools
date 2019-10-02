
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ifld"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ifld.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "ifld.web"
      subroutine ifld(INT,ERROR)
      implicit none
      integer Icd,INT,Ipt,Length,Lfn,Look,Nexp
      double precision one,real,sign,small,zero
      logical ERROR
      
      common/nbcrd1/Icd(80),Look(80),Length,Ipt,Lfn,Nexp
      common/nbcrd2/Point,End,Next,Exp
      logical Point,End,Next,Exp
      
      data zero,one,small/0.0D0,1.0D0,1.0D-3/
      
      
      INT=0
      call rfld(real,ERROR)
      
      
      if(.NOT.(Exp))then
      if(.NOT.(Point))then
      if(Nexp.GE.0)then
      if(Length.NE.0)then
      sign=one
      if(real.LT.zero)sign=-one
      real=real+small*sign
      INT=real
      return
      endif
      endif
      endif
      endif
      
      ERROR=.TRUE.
      Next=.FALSE.
      return
      end
C* :1 * 
      
