
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vewb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vewb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "vewb.web"
      double precision function vewb(IBUCA,IBUCB,DE,EVA,EVB,IOPT)
      implicit none
      double precision a0,DE,eabij,eaij,eij,EVA,EVB,F42,Four,Half,One,On
     &ept5,Ten,Three,Two,V,Zero
      integer i,ia,ib,IBUCA,IBUCB,Ieval,ii,ij,ind,Ioab,IOPT,Ispect,kij,l
     &eft,leng,lij,Loab,Lspect,Maxbuc,Mdv
      integer mij,nij,Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobv
     &a,Nobvb,Novaa,Novab,Novbb,nrij,Nrorb,nruns,Nva
      integer Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      dimension EVA(*),EVB(*)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      
      
      call track('VEWB  ')
      
      vewb=Zero
      if(Novab.GT.0)then
      
      nrij=Mdv/Nvavb
      nruns=Noaob/nrij
      if(mod(Noaob,nrij).NE.0)nruns=nruns+1
      left=Noaob
      call fileio(2,-IBUCA,0,0,0)
      if(IBUCB.GT.0)call fileio(1,-IBUCB,0,0,0)
      mij=1
      lij=0
      
      do 50 i=1,nruns
      nij=min0(left,nrij)
      left=left-nij
      leng=nij*Nvavb
      call fileio(2,IBUCA,leng,V,0)
      
      ind=0
      kij=0
      lij=lij+nij
      do 20 ii=1,Noa
      do 10 ij=1,Nob
      kij=kij+1
      if(kij.LE.lij)then
      if(kij.GE.mij)then
      eij=(DE)+EVA(ii)+EVB(ij)
      if(IOPT.EQ.2)then
      
      do 4 ia=1,Nva
      eaij=eij-EVA(ia+Noa)
      do 2 ib=1,Nvb
      ind=ind+1
      eabij=(eaij-EVB(ib+Nob))
      a0=V(ind)/eabij
      vewb=vewb+a0*V(ind)
      V(ind)=a0
2     continue
4     continue
      else
      
      do 8 ia=1,Nva
      eaij=eij-EVA(ia+Noa)
      do 6 ib=1,Nvb
      ind=ind+1
      eabij=(eaij-EVB(ib+Nob))
      a0=V(ind)*eabij
      vewb=vewb+a0*V(ind)
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
      
