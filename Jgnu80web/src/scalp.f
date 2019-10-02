
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scalp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scalp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "scalp.web"
      double precision function scalp(IBUC1,IBUC2,N)
      implicit none
      double precision F42,Four,Half,One,Onept5,Ten,Three,Two,V1,V2,Zero
      integer IBUC1,IBUC2,ind,ir,l1,leng,max,Mdv,N,nruns
      common/v/V1(10000),V2(10000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      call track('SCALP ')
      
      scalp=Zero
      if(N.LE.0)return
      
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      
      max=Mdv/2
      if(IBUC1.EQ.IBUC2)max=Mdv
      nruns=N/max
      if(mod(N,max).NE.0)nruns=nruns+1
      l1=N
      do 100 ir=1,nruns
      leng=min0(l1,max)
      l1=l1-leng
      
      call fileio(2,IBUC1,leng,V1,0)
      if(IBUC1.EQ.IBUC2)then
      
      do 20 ind=1,leng
      scalp=scalp+V1(ind)**2
20    continue
      else
      call fileio(2,IBUC2,leng,V2,0)
      do 40 ind=1,leng
      scalp=scalp+V1(ind)*V2(ind)
40    continue
      endif
      
100   continue
      
      return
      
      end
C* :1 * 
      
