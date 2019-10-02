
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 unpcck"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "unpcck.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "unpcck.web"
      subroutine unpcck(K,KK,X,M,N,MM,NN)
      implicit none
      integer i,ii,ipfc,ix,j,jj,jx,K,KK,M,MM,N,nfc,NN
      double precision X
      dimension X(M,N)
      
      
      
      
      
      
      
      if(K.NE.0)then
      ii=(MM*(MM+1))/2
      else
      
      ii=MM*NN
      endif
      nfc=ii/M
      ipfc=ii-nfc*M
      if(ipfc.NE.0)then
      
      jj=nfc+1
      ii=ipfc
      else
      jj=nfc
      ii=M
      endif
      if(K.NE.0)then
      ix=NN+1
      do 50 i=1,NN
      ix=ix-1
      do 20 j=1,ix
      jx=MM-i-j+2
      X(jx,ix)=X(ii,jj)
      ii=ii-1
      if(ii.EQ.0)then
      ii=M
      jj=jj-1
      endif
20    continue
50    continue
      jj=NN-1
      do 100 j=1,jj
      ii=j+1
      do 60 i=ii,NN
      X(i,j)=X(j,i)
60    continue
100   continue
      return
      endif
      
      
      do 200 i=1,NN
      ix=NN-i+1
      do 150 j=1,MM
      jx=MM-j+1
      X(jx,ix)=X(ii,jj)
      ii=ii-1
      if(ii.EQ.0)then
      ii=M
      jj=jj-1
      endif
150   continue
200   continue
      return
      
      end
C* :1 * 
      
