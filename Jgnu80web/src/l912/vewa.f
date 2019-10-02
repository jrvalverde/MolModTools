
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vewa"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vewa.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "vewa.web"
      double precision function vewa(IBUCA,IBUCB,DE,EV,NO,NV,IOPT)
      implicit none
      double precision a0,DE,eabij,eaij,eij,EV,F42,Four,Half,One,Onept5,
     &Ten,Three,Two,V,Zero
      integer i,ia,iap,ib,IBUCA,IBUCB,ii,iip,ij,ind,IOPT,kij,left,leng,l
     &ij,Mdv,mij,nij,NO,no1
      integer no3,nrij,nruns,NV,nv1,nv3
      dimension EV(*)
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      
      call track('VEWA  ')
      
      vewa=Zero
      no1=NO-1
      nv1=NV-1
      if(no1.GT.0.AND.nv1.GT.0)then
      
      no3=NO*no1/2
      nv3=NV*nv1/2
      nrij=Mdv/nv3
      nruns=no3/nrij
      if(mod(no3,nrij).NE.0)nruns=nruns+1
      left=no3
      call fileio(2,-IBUCA,0,0,0)
      if(IBUCB.GT.0)call fileio(1,-IBUCB,0,0,0)
      mij=1
      lij=0
      
      do 50 i=1,nruns
      nij=min0(left,nrij)
      left=left-nij
      leng=nij*nv3
      call fileio(2,IBUCA,leng,V,0)
      
      ind=0
      kij=0
      lij=lij+nij
      do 20 ii=1,no1
      iip=ii+1
      do 10 ij=iip,NO
      kij=kij+1
      if(kij.LE.lij)then
      if(kij.GE.mij)then
      eij=(DE)+EV(ii)+EV(ij)
      if(IOPT.EQ.2)then
      
      do 4 ia=1,nv1
      iap=ia+1
      eaij=eij-EV(ia+NO)
      do 2 ib=iap,NV
      ind=ind+1
      eabij=(eaij-EV(ib+NO))
      a0=V(ind)/eabij
      vewa=vewa+a0*V(ind)
      V(ind)=a0
2     continue
4     continue
      else
      
      do 8 ia=1,nv1
      iap=ia+1
      eaij=eij-EV(ia+NO)
      do 6 ib=iap,NV
      ind=ind+1
      eabij=(eaij-EV(ib+NO))
      a0=V(ind)*eabij
      vewa=vewa+a0*V(ind)
      V(ind)=a0
6     continue
8     continue
      endif
      endif
      endif
10    continue
20    continue
      
      if(IBUCB.GT.0)call fileio(1,IBUCB,leng,V,0)
      
      mij=mij+nij
50    continue
      endif
      
      return
      
      end
C* :1 * 
      
