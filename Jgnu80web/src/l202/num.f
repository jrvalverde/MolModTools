
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 num"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "num.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "num.web"
      integer function num(N,IDIGIT)
      implicit none
      integer i,IDIGIT,idx,N
      dimension i(10)
      data i/'1','2','3','4','5','6','7','8','9','0'/
      
      
      
      
      
      if(IDIGIT.NE.2)then
      
      
      idx=N/10
      if(idx.EQ.0)idx=10
      num=i(idx)
      return
      endif
      
      
      idx=mod(N,10)
      if(idx.EQ.0)idx=10
      num=i(idx)
      return
      
      end
C* :1 * 
      
