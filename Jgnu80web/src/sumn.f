
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sumn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sumn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "sumn.web"
      subroutine sumn(IBUC1,IBUC2,LNG,FACTOR)
      implicit none
      double precision F42,FACTOR,Four,Half,One,Onept5,Ten,Three,Two,V,Z
     &ero
      integer i,IBUC1,IBUC2,ir,leng,length,LNG,Mdv,mdv2,mdv21,nruns,nt,n
     &times
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      call track('SUMN  ')
      
      ntimes=ifix(sngl(FACTOR))
      if(dabs(dfloat(ntimes)-FACTOR).GT.0.)ntimes=3
      if(ntimes.EQ.0.OR.LNG.EQ.0)return
      
      length=LNG
      nt=iabs(ntimes)
      nt=min0(nt,3)
      mdv2=Mdv/2
      mdv21=mdv2+1
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      nruns=length/mdv2
      if((mdv2*nruns).NE.length)nruns=nruns+1
      
      do 300 ir=1,nruns
      leng=min0(length,mdv2)
      length=length-leng
      call fileio(2,IBUC1,leng,V(mdv21),0)
      call fileio(2,IBUC2,leng,V,0)
      
      if(ntimes.GT.0)then
      if(nt.EQ.1)goto 50
      if(nt.EQ.2)then
      
      do 10 i=1,leng
      V(i)=V(i)+V(mdv2+i)+V(mdv2+i)
10    continue
      goto 250
      elseif(nt.EQ.3)then
      goto 150
      endif
      endif
      if(ntimes.LT.0)then
      if(nt.EQ.1)then
      
      do 20 i=1,leng
      V(i)=V(i)-V(mdv2+i)
20    continue
      elseif(nt.EQ.2)then
      
      do 30 i=1,leng
      V(i)=V(i)-(V(mdv2+i)+V(mdv2+i))
30    continue
      elseif(nt.EQ.3)then
      goto 150
      else
      goto 50
      endif
      goto 250
      endif
50    do 100 i=1,leng
      V(i)=V(i)+V(mdv2+i)
100   continue
      goto 250
      
150   do 200 i=1,leng
      V(i)=V(i)+FACTOR*V(mdv2+i)
200   continue
      
250   call fileio(1,IBUC2,leng,V,0)
300   continue
      return
      
      end
C* :1 * 
      
