
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ijkpri"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ijkpri.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "ijkpri.web"
      integer function ijkpri(I,J,K,L)
      implicit none
      integer I,J,K,L,loop,loop1,loop2,num,num2
      dimension num2(4),num(4)
      
      
      
      num2(1)=I
      num2(2)=J
      num2(3)=K
      num2(4)=L
      do 100 loop1=1,4
      num(loop1)=max0(num2(1),num2(2),num2(3),num2(4))
      do 50 loop2=1,4
      if(num(loop1).EQ.num2(loop2))then
      num2(loop2)=-1
      goto 100
      endif
      
50    continue
100   continue
      ijkpri=0
      do 200 loop=1,4
      ijkpri=ijkpri+num(loop)*120**(4-loop)
200   continue
      
      return
      
      end
C* :1 * 
      
