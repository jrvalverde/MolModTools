
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 analfc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "analfc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "analfc.web"
      subroutine analfc(NVAR,FRCNST,S)
      implicit none
      double precision d,FRCNST,S
      integer i,iad1,iad2,idx,is,j,NVAR
      dimension FRCNST(*),S(50,50)
      dimension is(100),iad1(50),iad2(50),d(50)
      
      
      
      
      
      idx=0
      do 100 i=1,NVAR
      do 50 j=1,i
      idx=idx+1
      S(i,j)=FRCNST(idx)
      S(j,i)=FRCNST(idx)
50    continue
100   continue
      call inv(S,NVAR,is,iad1,iad2,d,50)
      
      return
      
      end
C* :1 * 
      
