
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 hfld"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "hfld.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "hfld.web"
      subroutine hfld(KEYWD,LENG,ENDD)
      implicit none
      integer i,Icd,Ipt,kend,KEYWD,LENG,leng1,Length,Lfn,Look,nbla,Nexp
      logical ENDD,equal
      
      common/nbcrd1/Icd(80),Look(80),Length,Ipt,Lfn,Nexp
      common/nbcrd2/Point,End,Next,Exp
      logical Point,End,Next,Exp
      
      dimension KEYWD(LENG),kend(3)
      
      data nbla/1H /
      data kend/1HE,1HN,1HD/
      
      
      End=.FALSE.
      if(Next)call fndfld
      ENDD=End
      leng1=LENG
      LENG=min0(Length,LENG)
      
      
      do 100 i=1,LENG
      KEYWD(i)=Look(i)
100   continue
      
      
      do 200 i=LENG+1,leng1
      KEYWD(i)=nbla
200   continue
      Next=.TRUE.
      
      
      if(equal(KEYWD,kend,3))End=.TRUE.
      return
      end
C* :1 * 
      
