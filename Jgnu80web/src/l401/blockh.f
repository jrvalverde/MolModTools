
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 blockh"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "blockh.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "blockh.web"
      subroutine blockh(X,MAXDIM,IAN,NATOMS,IBASIS,IFDON1,IFDON2,I5OR6D,
     &IFPONH)
      implicit none
      integer i,I5OR6D,ia,IAN,IBASIS,ifd,IFDON1,IFDON2,IFPONH,ilop,In,in
     &dex,Iout,Ipunch,j,jj,MAXDIM,mbasis,NATOMS
      real*8 X,zero
      
      common/io/In,Iout,Ipunch
      dimension X(MAXDIM,MAXDIM),IAN(NATOMS)
      data zero/0.0D0/
      
      if(IBASIS.LE.0)then
      if((IFDON1+IFDON2+IFPONH).EQ.0)return
      endif
      index=0
      
      do 100 i=1,NATOMS
      ia=IAN(i)
      if(ia.LE.2)then
      ilop=index+2
      index=index+1
      if(IBASIS.LE.0)then
      if(IFPONH.LE.0)goto 100
      index=index+3
      else
      index=ilop
      if(IFPONH.GT.0)index=index+3
      endif
      else
      
      if(ia.LE.10)then
      ilop=index+6
      ifd=IFDON1
      
      elseif(ia.LE.18)then
      ilop=index+10
      ifd=IFDON2
      else
      
      write(Iout,99001)
99001 format(//,24H NOT CODED FOR THIRD ROW)
      stop
      endif
      if(IBASIS.LE.0)then
      index=ilop-1
      if(ifd.LE.0)goto 100
      index=index+5+I5OR6D
      else
      index=ilop+3
      if(ifd.GT.0)index=index+5+I5OR6D
      endif
      endif
      
      do 50 j=ilop,index
      do 20 jj=1,mbasis
      if(j.NE.jj)then
      X(j,jj)=zero
      X(jj,j)=zero
      endif
20    continue
50    continue
100   continue
      return
      end
C* :1 * 
      
