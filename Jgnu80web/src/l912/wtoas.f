
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wtoas"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wtoas.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "wtoas.web"
      subroutine wtoas(IBUC1,IBUC2,EVA,NOA,NVA)
      implicit none
      double precision A00,A0s,Anorm,aprev,Cuts,De1,Dehf,Delmax,Den,eai,
     &ei,Energy,EVA,F42,Four,Half,One,Onept5,Q1,Ten
      double precision Three,Two,V,w,W0,Zero
      integer ia,IBUC1,IBUC2,Iflag,ii,ind,Ipcyc,Isd,leng,Maxit,Mdv,mdv2,
     &mdv21,Niter,NOA,Norm,NVA
      logical Davail,Savail
      
      
      
      
      
      
      dimension EVA(*)
      
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/wtoa/De1,Q1,A0s
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      
      call track('WTOAS ')
      
      if(NOA.LE.0.OR.NVA.LE.0)return
      
      mdv2=Mdv/2
      mdv21=mdv2+1
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      leng=NOA*NVA
      call fileio(2,IBUC1,leng,V,0)
      call fileio(2,IBUC2,leng,V(mdv21),0)
      ind=0
      
      do 100 ii=1,NOA
      ei=EVA(ii)+(De1)
      do 50 ia=1,NVA
      ind=ind+1
      w=V(ind)
      aprev=V(ind+mdv2)
      eai=(ei-EVA(ia+NOA))
      V(ind)=Q1*(w/eai-aprev)+aprev
50    continue
100   continue
      
      call fileio(1,IBUC2,leng,V,0)
      
      return
      
      end
C* :1 * 
      
