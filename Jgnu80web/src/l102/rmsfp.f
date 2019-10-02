
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rmsfp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rmsfp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "rmsfp.web"
      double precision function rmsfp(NVAR,MODE)
      implicit none
      double precision D1var,diff,gsqrt,Pool1,sum,Yold,zero
      integer i,Inf,MODE,NVAR
      dimension D1var(1),Yold(1),Pool1(1)
      common/fpinfo/Inf(2394)
      equivalence(Pool1(1),Inf(61))
      equivalence(Yold(1),Inf(181))
      equivalence(D1var(1),Inf(241))
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
      
      
      if(MODE.NE.2)then
      
      
      sum=zero
      do 50 i=1,NVAR
      sum=sum+D1var(i)*D1var(i)
50    continue
      rmsfp=gsqrt(sum)
      return
      endif
      
      
      
      sum=zero
      do 100 i=1,NVAR
      diff=Pool1(NVAR)-Yold(NVAR)
      sum=sum+diff*diff
100   continue
      rmsfp=gsqrt(sum)
      return
      
      end
C* :1 * 
      
