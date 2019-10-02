
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 transp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "transp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "transp.web"
      subroutine transp(NI,NJ,IBUC1,IBUC2)
      implicit none
      integer i,IBUC1,IBUC2,ind,ind1,ind2,ind3,j,kount,leng,Mdv,mdv2,mdv
     &21,NI,nic,nicore,nij,nileft,nimax,NJ
      integer njcore,njleft,njmax
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      
      call track('TRANSP')
      
      call fileio(2,-IBUC1,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      mdv2=Mdv/2
      mdv21=mdv2+1
      kount=0
      nimax=mdv2/NJ
      njmax=mdv2/NI
      nij=NI*NJ
      nileft=NI
      
100   nicore=min0(nileft,nimax)
      nileft=nileft-nicore
      leng=nicore*NJ
      call fileio(2,IBUC1,leng,V(mdv21),0)
      
      ind1=mdv2
      do 200 i=1,nicore
      ind2=i
      do 150 j=1,NJ
      ind1=ind1+1
      V(ind2)=V(ind1)
      ind2=ind2+nicore
150   continue
200   continue
      
      njleft=NJ
      ind2=0
      ind3=1
300   njcore=min0(njleft,njmax)
      njleft=njleft-njcore
      leng=njcore*nicore
      ind1=kount*njcore*min0(NI,nimax)
      ind=ind1+ind2
      call fileio(1,-IBUC2,leng,V(ind3),ind)
      ind3=ind3+leng
      ind2=ind2+njcore*NI
      if(njleft.GT.0)goto 300
      
      kount=kount+1
      if(nileft.GT.0)goto 100
      
      if(nij.GT.mdv2)then
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      njleft=NJ
      nicore=min0(NI,nimax)
350   njcore=min0(njleft,njmax)
      njleft=njleft-njcore
      leng=njcore*NI
      call fileio(2,IBUC2,leng,V,0)
      
      ind1=mdv2
      do 400 j=1,njcore
      nileft=NI
      ind=0
360   nic=min0(nileft,nicore)
      nileft=nileft-nic
      ind2=ind+(j-1)*nic
      
      do 380 i=1,nic
      ind1=ind1+1
      ind2=ind2+1
      V(ind1)=V(ind2)
380   continue
      
      ind=ind+nic*njcore
      if(nileft.GT.0)goto 360
      
400   continue
      call fileio(1,IBUC2,leng,V(mdv2+1),0)
      
      if(njleft.GT.0)goto 350
      endif
      
      return
      
      end
C* :1 * 
      
