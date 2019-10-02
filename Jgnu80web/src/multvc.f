
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 multvc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "multvc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "multvc.web"
      subroutine multvc(IBUC,LNG,FACT)
      implicit none
      double precision F42,FACT,Four,Half,One,Onept5,Ten,Three,Two,V,Zer
     &o
      integer i,IBUC,j,left,leng,LNG,Mdv,nruns
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      call track('MULTVC')
      
      if(LNG.LE.0)return
      left=LNG
      nruns=LNG/Mdv
      if(mod(LNG,Mdv).NE.0)nruns=nruns+1
      call fileio(2,-IBUC,0,0,0)
      call fileio(1,-IBUC,0,0,0)
      
      do 100 i=1,nruns
      leng=min0(left,Mdv)
      left=left-leng
      call fileio(2,IBUC,leng,V,0)
      do 50 j=1,leng
      V(j)=V(j)*FACT
50    continue
      call fileio(1,IBUC,leng,V,0)
100   continue
      
      return
      
      end
C* :1 * 
      
