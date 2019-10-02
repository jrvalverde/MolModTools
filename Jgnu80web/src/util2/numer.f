
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 numer"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "numer.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "numer.web"
      integer function numer(NGRP)
      implicit none
      integer i,idigit,jdigit,n,n1,n2,NGRP
      dimension NGRP(3)
      dimension n(10)
      data n/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      
      
      
      
      
      idigit=NGRP(2)
      jdigit=NGRP(3)
      numer=0
      n1=-1
      n2=-1
      do 100 i=1,10
      if(idigit.EQ.n(i))n1=i-1
      if(jdigit.EQ.n(i))n2=i-1
100   continue
      numer=n1
      if(n2.NE.-1)numer=10*n1+n2
      if(numer.LT.0)numer=0
      
      return
      
      end
C* :1 * 
      
