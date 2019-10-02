
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wtoada"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wtoada.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "wtoada.web"
      subroutine wtoada(IBUC1,IBUC2,IBUC3,EVA,NOA,NVA)
      implicit none
      double precision A00,A0s,Anorm,aprev,Cuts,De1,Dehf,Delmax,Den,eabi
     &j,eaij,eij,Energy,EVA,F42,Four,Half,One,Onept5,Q1
      double precision Ten,Three,Two,V,V0,w,W0,Zero
      integer ia,iap,ib,IBUC1,IBUC2,IBUC3,Iflag,ii,iip,ij,ind,Ipcyc,Isd,
     &leng,lij,lmax,ls,m,Maxit,Mdv
      integer mdv1,mdv2,mdv21,mdvl,n1,Niter,no1,NOA,noa3,Norm,nv1,NVA,nv
     &a3
      logical Davail,Savail
      
      
      
      
      
      
      
      dimension EVA(*)
      dimension V0(20000)
      
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/wtoa/De1,Q1,A0s
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      
      equivalence(V0(1),V(1))
      
      
      call track('WTOADA')
      
      no1=NOA-1
      nv1=NVA-1
      if(no1.LE.0.OR.nv1.LE.0)return
      
      nva3=NVA*nv1/2
      noa3=NOA*no1/2
      
      mdvl=Mdv-2*noa3-nva3
      n1=mdvl+1
      
      mdv2=mdvl/2
      mdv21=mdv2+1
      mdv1=(mdv2/nva3)*nva3
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      call fileio(2,-IBUC3,0,0,0)
      lmax=nva3*NOA*no1/2
      leng=min0(lmax,mdv1)
      call fileio(2,IBUC1,leng,V,0)
      call fileio(2,IBUC2,leng,V(mdv21),0)
      lmax=lmax-leng
      ind=0
      lij=mdvl+1
      
      do 100 ii=1,no1
      iip=ii+1
      do 50 ij=iip,NOA
      
      ls=leng
      leng=nva3
      call fileio(2,IBUC3,leng,V0(n1),0)
      leng=ls
      
      if(ind+nva3.GT.leng)then
      call fileio(1,IBUC2,leng,V,0)
      leng=min0(lmax,mdv1)
      call fileio(2,IBUC1,leng,V,0)
      call fileio(2,IBUC2,leng,V(mdv21),0)
      lmax=lmax-leng
      ind=0
      endif
      
      eij=De1+EVA(ii)+EVA(ij)
      
      m=n1
      do 20 ia=1,nv1
      iap=ia+1
      eaij=eij-EVA(ia+NOA)
      do 10 ib=iap,NVA
      ind=ind+1
      w=V(ind)+V0(m)*A0s
      m=m+1
      aprev=V(ind+mdv2)
      eabij=(eaij-EVA(ib+NOA))
      V(ind)=Q1*(w/eabij-aprev)+aprev
10    continue
20    continue
      lij=lij+1
50    continue
100   continue
      
      call fileio(1,IBUC2,leng,V,0)
      
      return
      
      end
C* :1 * 
      
