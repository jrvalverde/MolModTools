
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nprio"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nprio.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "nprio.web"
      integer function nprio(I,J,K,L)
      implicit none
      integer I,J,K,L,lend,loop1,loop2,num,num2
      dimension num(4)
      
      
      
      num(1)=I
      num(2)=J
      num(3)=K
      num(4)=L
      lend=4
      do 100 loop1=1,3
      lend=lend-1
      do 50 loop2=1,lend
      if(num(loop2).GT.num(loop2+1))then
      num2=num(loop2)
      num(loop2)=num(loop2+1)
      num(loop2+1)=num2
      endif
50    continue
100   continue
      nprio=num(1)*1728000+num(2)*14400+num(3)*120+num(4)
      
      return
      
      end
C* :1 * 
      
