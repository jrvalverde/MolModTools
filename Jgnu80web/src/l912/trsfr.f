
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trsfr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trsfr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "trsfr.web"
      subroutine trsfr(L,IBUC1,IBUC2)
      implicit none
      integer i,index,jndex,leng,m1,Mdv,nruns
      double precision V
      integer L,IBUC1,IBUC2
      common/v/V(20000),Mdv
      
      
      
      
      
      
      call track('TRSFR ')
      
      if(L.LE.0)return
      
      nruns=L/Mdv
      m1=nruns*Mdv
      
      leng=L-m1
      index=m1
      jndex=m1
      call fileio(2,-IBUC1,leng,V,index)
      call fileio(1,-IBUC2,leng,V,jndex)
      if(nruns.NE.0)then
      
      leng=Mdv
      do 50 i=1,nruns
      m1=m1-Mdv
      index=m1
      jndex=m1
      call fileio(2,-IBUC1,leng,V,index)
      call fileio(1,-IBUC2,leng,V,jndex)
50    continue
      endif
      
      return
      
      end
C* :1 * 
      
