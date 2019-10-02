
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 setbas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "setbas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "setbas.web"
      subroutine setbas(LSTOCC,LSTEMT,NOCC,NEMT,IAT,L,NL,NF,NDIM)
      
      
      implicit none
      integer IAT,icore,iecp,ival,j,L,left,LSTEMT,LSTOCC,NDIM,NEMT,NF,NL
     &,NOCC,nshell
      
      dimension LSTOCC(NDIM),LSTEMT(NDIM)
      dimension icore(4),ival(4)
      
      
      if(L.LT.4)then
      
      
      iecp=0
      call cortbl(IAT,icore,iecp)
      call valtbl(IAT,ival)
      
      
      nshell=max0(icore(L+1),0)+ival(L+1)
      if(nshell.NE.0)then
      
      
      do 20 j=1,nshell
      NOCC=NOCC+1
      LSTOCC(NOCC)=NF+j
20    continue
      left=NL-nshell
      if(left.EQ.0)return
      do 40 j=1,left
      NEMT=NEMT+1
      LSTEMT(NEMT)=NF+nshell+j
40    continue
      return
      endif
      endif
      
      
      do 100 j=1,NL
      NEMT=NEMT+1
      LSTEMT(NEMT)=NF+j
100   continue
      return
      end
C* :1 * 
      
