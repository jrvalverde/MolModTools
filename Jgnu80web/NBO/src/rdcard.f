
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdcard"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdcard.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "rdcard.web"
      subroutine rdcard
      implicit none
      integer i,Icd,ichara,icharz,Ipt,Length,Lfn,Look,Nexp
      
      common/nbcrd1/Icd(80),Look(80),Length,Ipt,Lfn,Nexp
      common/nbcrd2/Point,End,Next,Exp
      logical Point,End,Next,Exp
      
      data ichara/'a'/icharz/'z'/
      
      
      read(Lfn,99001,end=200)Icd
      
      
      do 100 i=1,80
      if(Icd(i).GE.ichara.AND.Icd(i).LE.icharz)Icd(i)=Icd(i)-32
100   continue
      
      
      Ipt=1
      return
      
      
200   End=.TRUE.
      return
      
99001 format(80A1)
      end
C* :1 * 
      
