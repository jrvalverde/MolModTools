
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 clean"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "clean.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "clean.web"
      subroutine clean(IBUCK,LENGTH)
      implicit none
      double precision gabs,small,V,zero
      integer i,IBUCK,l,leng,LENGTH,Mdv
      common/v/V(20000),Mdv
      data zero,small/0.D0,1.D-9/
      
      
      
      
      
      
      
      if(LENGTH.GT.0)then
      
      call fileio(2,-IBUCK,0,0,0)
      call fileio(1,-IBUCK,0,0,0)
      
      l=LENGTH
50    leng=min0(l,Mdv)
      l=l-leng
      call fileio(2,IBUCK,leng,V,0)
      
      do 100 i=1,leng
      if(gabs(V(i)).LT.small)V(i)=zero
100   continue
      
      call fileio(1,IBUCK,leng,V,0)
      if(l.GT.0)goto 50
      endif
      
      return
      
      end
C* :1 * 
      
