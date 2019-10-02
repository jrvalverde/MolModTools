
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 inibuc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "inibuc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "inibuc.web"
      subroutine inibuc(IBUCK,LNG,VALUE)
      implicit none
      integer i,IBUCK,left,leng,LNG,m,Mdv,n,nr
      double precision V,VALUE
      common/v/V(20000),Mdv
      
      
      
      
      
      
      call track('INIBUC')
      
      if(LNG.LE.0)return
      
      nr=LNG/Mdv
      left=LNG
      if(mod(LNG,Mdv).NE.0)nr=nr+1
      m=min0(Mdv,LNG)
      do 100 i=1,m
      V(i)=VALUE
100   continue
      
      call fileio(1,-IBUCK,0,0,0)
      do 200 n=1,nr
      leng=min0(left,Mdv)
      left=left-leng
      call fileio(1,IBUCK,leng,V,0)
200   continue
      return
      
      end
C* :1 * 
      
