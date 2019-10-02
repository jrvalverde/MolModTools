
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tstara"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tstara.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "tstara.web"
      subroutine tstara(IBUC1,IBUC2,C,NO,NV,S,INITW,NBASIS)
      implicit none
      double precision C,S,V
      integer i,IBUC1,IBUC2,lbloc,leftb,leftwi,leftwo,leng,Mdv,mdv11,nb3
     &,NBASIS,nbloc,NO,no3,nr,NV,nv3,nwi,nwo
      logical INITW
      dimension C(*),S(*)
      common/v/V(20000),Mdv
      
      
      
      
      
      
      
      
      call track('TSTARA')
      
      no3=NO*(NO-1)/2
      nv3=NV*(NV-1)/2
      if(no3.LE.0.OR.nv3.LE.0)return
      nb3=NBASIS*(NBASIS-1)/2
      
      leftb=no3
      leftwi=nb3*no3
      leftwo=nv3*no3
      nbloc=Mdv/(nv3+nb3)
      nwi=nbloc*nb3
      nwo=nbloc*nv3
      nr=no3/nbloc
      if(mod(no3,nbloc).NE.0)nr=nr+1
      mdv11=nwi+1
      
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      
      do 100 i=1,nr
      leng=min0(leftwi,nwi)
      leftwi=leftwi-leng
      call fileio(2,IBUC1,leng,V,0)
      leng=min0(leftwo,nwo)
      leftwo=leftwo-leng
      if(INITW)call fileio(2,IBUC2,leng,V(mdv11),0)
      lbloc=min0(leftb,nbloc)
      leftb=leftb-lbloc
      call ctwc1(V(mdv11),C,V,NV,lbloc,S,INITW,NBASIS)
      call fileio(1,IBUC2,leng,V(mdv11),0)
100   continue
      
      return
      
      end
C* :1 * 
      
