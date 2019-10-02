
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 convrt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "convrt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "convrt.web"
      subroutine convrt(N,NC1,NC2)
      implicit none
      integer int,isp,N,n1,n2,NC1,NC2
      
      
      dimension int(10)
      data isp,int/' ','1','2','3','4','5','6','7','8','9','0'/
      NC1=isp
      NC2=isp
      if(N.LE.0)return
      if(N.LT.10)then
      NC2=int(N)
      return
      endif
      n1=N/10
      if(n1.GT.9)stop 'ROUTINE CONVRT'
      NC1=int(n1)
      n2=N-n1*10
      if(n2.EQ.0)n2=10
      NC2=int(n2)
      return
      end
C* :1 * 
      
