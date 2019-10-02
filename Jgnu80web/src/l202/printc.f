
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 printc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "printc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "printc.web"
      subroutine printc(A)
      implicit none
      integer i,i1,In,Iop,Iout,Ipunch,j,Natoms,num
      double precision A(103,3)
      common Iop(52),Natoms
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
      num=Natoms+3
      do 100 i=1,Natoms
      write(Iout,99001)(A(i,j),j=1,3)
100   continue
      
99001 format(1x,3F20.15)
      
      write(Iout,99001)
      
      
      i1=Natoms+1
      do 200 i=1,3
      write(Iout,99001)(A(j,i),j=i1,num)
200   continue
      write(Iout,99001)
      return
      
      end
C* :1 * 
      
