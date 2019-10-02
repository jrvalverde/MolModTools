
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tread"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tread.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 39 "tread.web"
      subroutine tread(IARN,X,M,N,MM,NN,K)
      implicit none
      integer IARN,ii,inum,K,M,MM,N,NN
      double precision X
      dimension X(M,N)
      
      
      
      
      
      
      
      
      
      
      
      
      
      inum=IARN
      if(mod(inum,1000).LT.500)inum=inum+500
      
      if(K.NE.0)then
      ii=(MM*(MM+1))/2
      else
      
      ii=MM*NN
      endif
      call fileio(2,-inum,ii,X,0)
      if(NN.NE.1)call unpcck(K,ii,X,M,N,MM,NN)
      return
      
      end
C* :1 * 
      
