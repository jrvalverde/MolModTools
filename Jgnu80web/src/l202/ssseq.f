
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ssseq"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ssseq.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "ssseq.web"
      subroutine ssseq(MAXAP3,NATOMS,NOP,MAXOP,IATFLG,NPERM)
      implicit none
      integer iat,IATFLG,iop,jat,MAXAP3,MAXOP,NATOMS,NOP,NPERM
      dimension IATFLG(*),NPERM(MAXAP3,MAXOP)
      
      
      
      
      if(NOP.EQ.1)return
      do 100 iat=1,NATOMS
      if(IATFLG(iat).EQ.2)then
      do 20 iop=2,NOP
      do 10 jat=1,NATOMS
      if(NPERM(jat,iop).EQ.iat)then
      if(IATFLG(jat).EQ.0)IATFLG(jat)=1
      endif
10    continue
20    continue
      endif
100   continue
      return
      
      end
C* :1 * 
      
