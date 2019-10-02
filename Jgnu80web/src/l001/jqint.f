
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 jqint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "jqint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "jqint.web"
      integer function jqint(J)
      implicit none
      integer i,ibl,ij,In,ints,Iout,Ipunch,is,J
      dimension ints(10)
      common/io/In,Iout,Ipunch
      data ints/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      data ibl/1H /
      
      
      
      
99001 format(37H  STRANGE CHARACTER ON DOLLAR CARD.  ,a1)
      
      if(J.NE.ibl)then
      
      do 50 i=1,10
      is=i
      if(ints(i).EQ.J)goto 100
50    continue
      write(Iout,99001)J
      call lnk1e
100   ij=is-1
      else
      ij=0
      endif
      jqint=ij
      return
      
      end
C* :1 * 
      
