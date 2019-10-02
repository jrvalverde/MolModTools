
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gabgst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gabgst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "gabgst.web"
      subroutine gabgst(NBASIS,GA,GB)
      implicit none
      double precision GA,GB,pt25,temp
      integer i,j,k,NBASIS
      dimension GA(*),GB(*)
      data pt25/0.25D0/
      
      
      
      
      
      k=0
      do 100 i=1,NBASIS
      do 50 j=1,i
      k=k+1
      temp=pt25*GB(k)
      GB(k)=GA(k)+temp
      GA(k)=GA(k)-temp
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
