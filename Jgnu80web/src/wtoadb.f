
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wtoadb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wtoadb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "wtoadb.web"
      subroutine wtoadb(IBUC1,IBUC2,IBUC3,EVA,EVB)
      implicit none
      double precision A00,A0s,Anorm,aprev,Cuts,De1,Dehf,Delmax,Den,eabi
     &j,eaij,eij,Energy,EVA,EVB,F42,Four,Half,One,Onept5
      double precision Q1,Ten,Three,Two,V,V0,w,W0,Zero
      integer ia,ib,IBUC1,IBUC2,IBUC3,Ieval,Iflag,ii,ij,ind,Ioab,Ipcyc,I
     &sd,Ispect,leng,lij,lmax,Loab,ls,Lspect
      integer m,Maxbuc,Maxit,Mdv,mdv1,mdv2,mdv21,mdvl,n1,Niter,Noa,Noa2,
     &Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva
      integer Nobvb,Norm,Novaa,Novab,Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb,Nvb
     &,Nvb2,Nvb3
      logical Davail,Savail
      
      
      
      
      
      
      
      dimension EVA(*),EVB(*)
      dimension V0(20000)
      
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/wtoa/De1,Q1,A0s
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      
      equivalence(V0(1),V(1))
      
      call track('WTOADB')
      
      lmax=Novab
      if(lmax.LE.0)return
      
      mdvl=Mdv-2*Noaob-Nvavb
      n1=mdvl+1
      
      mdv2=mdvl/2
      mdv21=mdv2+1
      mdv1=(mdv2/Nvavb)*Nvavb
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,-IBUC2,0,0,0)
      call fileio(1,-IBUC2,0,0,0)
      call fileio(2,-IBUC3,0,0,0)
      leng=min0(lmax,mdv1)
      call fileio(2,IBUC1,leng,V,0)
      call fileio(2,IBUC2,leng,V(mdv21),0)
      lmax=lmax-leng
      ind=0
      lij=mdvl+1
      
      do 100 ii=1,Noa
      do 50 ij=1,Nob
      
      ls=leng
      leng=Nvavb
      call fileio(2,IBUC3,leng,V0(n1),0)
      leng=ls
      
      if(ind+Nvavb.GT.leng)then
      call fileio(1,IBUC2,leng,V,0)
      leng=min0(lmax,mdv1)
      call fileio(2,IBUC1,leng,V,0)
      call fileio(2,IBUC2,leng,V(mdv21),0)
      lmax=lmax-leng
      ind=0
      endif
      
      eij=De1+EVA(ii)+EVB(ij)
      
      m=n1
      do 20 ia=1,Nva
      eaij=eij-EVA(ia+Noa)
      do 10 ib=1,Nvb
      ind=ind+1
      w=V(ind)+V0(m)*A0s
      m=m+1
      aprev=V(ind+mdv2)
      eabij=(eaij-EVB(ib+Nob))
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
      
