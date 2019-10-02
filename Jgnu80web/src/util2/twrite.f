
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 twrite"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "twrite.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 36 "twrite.web"
      subroutine twrite(IARN,X,M,N,MM,NN,K)
      implicit none
      integer i,IARN,ii,inum,j,jj,K,M,MM,N,NN
      double precision X
      dimension X(M,N)
      
      
      
      
      
      
      
      
      
      
      
      inum=IARN
      if(mod(inum,1000).LT.500)inum=inum+500
      
      if(K.NE.0)then
      ii=1
      jj=1
      do 50 j=1,NN
      do 20 i=1,j
      X(ii,jj)=X(i,j)
      ii=ii+1
      if(ii.GT.M)then
      ii=1
      jj=jj+1
      endif
20    continue
50    continue
      ii=(MM*(MM+1))/2
      else
      
      if(NN.NE.1)then
      if(M.LE.MM)then
      
      ii=1
      jj=2
      else
      ii=MM+1
      jj=1
      endif
      do 80 j=2,NN
      do 60 i=1,MM
      X(ii,jj)=X(i,j)
      if(ii.NE.M)then
      
      ii=ii+1
      else
      ii=1
      jj=jj+1
      endif
60    continue
80    continue
      endif
      ii=MM*NN
      endif
      call fileio(1,-inum,ii,X,0)
      if(NN.NE.1)call unpcck(K,ii,X,M,N,MM,NN)
      return
      
      end
C* :1 * 
      
