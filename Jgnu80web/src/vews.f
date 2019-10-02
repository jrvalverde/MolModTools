
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vews"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vews.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "vews.web"
      double precision function vews(IBUCA,IBUCB,DE,EV,NO,NV,IOPT)
      implicit none
      double precision a0,DE,eai,EV,F42,Four,Half,One,Onept5,Ten,Three,T
     &wo,V,Zero
      integer ia,IBUCA,IBUCB,ii,ind,IOPT,leng,Mdv,NO,NV
      dimension EV(*)
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      
      
      
      call track('VEWS  ')
      
      vews=Zero
      if(NO.GT.0.AND.NV.GT.0)then
      
      call fileio(2,-IBUCA,0,0,0)
      if(IBUCB.GT.0)call fileio(1,-IBUCB,0,0,0)
      leng=NO*NV
      call fileio(2,IBUCA,leng,V,0)
      
      ind=0
      if(IOPT.EQ.2)then
      
      do 20 ii=1,NO
      do 10 ia=1,NV
      ind=ind+1
      eai=DE+(EV(ii)-EV(ia+NO))
      a0=V(ind)/eai
      vews=vews+a0*V(ind)
      V(ind)=a0
10    continue
20    continue
      else
      
      do 40 ii=1,NO
      do 30 ia=1,NV
      ind=ind+1
      eai=DE+(EV(ii)-EV(ia+NO))
      a0=V(ind)*eai
      vews=vews+a0*V(ind)
      V(ind)=a0
30    continue
40    continue
      endif
      
      if(IBUCB.GT.0)call fileio(1,IBUCB,leng,V,0)
      endif
      
      return
      
      end
C* :1 * 
      
