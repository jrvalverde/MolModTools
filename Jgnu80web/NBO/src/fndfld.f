
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fndfld"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fndfld.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "fndfld.web"
      subroutine fndfld
      implicit none
      integer icard,Icd,ichar,Ipt,kend,Length,Lfn,Look,m,mcol,nbla,ncol,
     &ncom,neq,nexc,Nexp
      logical equal
      
      common/nbcrd1/Icd(80),Look(80),Length,Ipt,Lfn,Nexp
      common/nbcrd2/Point,End,Next,Exp
      logical Point,End,Next,Exp
      
      dimension kend(3)
      
      data nbla/1H /,ncom/1H,/,nexc/'!'/,neq/1H=/
      data kend/1HE,1HN,1HD/
      
      
      if(End)goto 400
      if(Ipt.GE.80)call rdcard
      if(End)goto 400
      
      
100   do 200 ncol=Ipt,80
      icard=Icd(ncol)
      if(icard.EQ.nexc)goto 300
      if(icard.NE.nbla.AND.icard.NE.ncom.AND.icard.NE.neq)goto 500
200   continue
      
      
300   call rdcard
      if(.NOT.End)goto 100
      
      
400   Length=0
      return
      
      
500   m=0
      do 600 mcol=ncol,80
      ichar=Icd(mcol)
      if(ichar.EQ.nbla.OR.ichar.EQ.ncom.OR.ichar.EQ.neq)goto 700
      m=m+1
      Look(m)=ichar
600   continue
      
      
700   Length=m
      Ipt=mcol
      Next=.FALSE.
      
      
      if(equal(Look,kend,3))End=.TRUE.
      return
      end
C* :1 * 
      
