
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "matout.web"
      subroutine matout(X,M,N,MM,NN)
      implicit none
      integer i,iflg,ilower,In,Iout,Ipunch,iupper,j,M,MM,N,NN
      double precision X
      dimension X(M,N)
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
99001 format(9(11x,i3))
99002 format(i4,9D14.6)
      
      iflg=1
      ilower=1
100   iupper=ilower+5
      if(iupper.GE.NN)then
      iupper=NN
      iflg=0
      endif
      write(Iout,99001)(j,j=ilower,iupper)
      do 200 i=1,MM
      write(Iout,99002)i,(X(i,j),j=ilower,iupper)
200   continue
      if(iflg.NE.0)then
      ilower=iupper+1
      goto 100
      endif
      
      return
      
      end
C* :1 * 
      
