
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dd1sd4"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dd1sd4.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "dd1sd4.web"
      subroutine dd1sd4(SAVAIL,METHOD,NBASIS)
      implicit none
      double precision Cmo,Cut,E,F42,Four,Half,One,Onept5,Ten,Three,Two,
     &V,V1,Zero
      integer i,i1,Iad1,Iad2,Iad3,Ias1,Ias2,Ibout,Ibuc,ibuck,ibucka,icou
     &nt,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7
      integer Idb8,Idb9,Idm,Ieval,ij,In,ind,ind1,index,Intc,Intt,Ioab,Io
     &pcl,Iout,Iprint,Ipunch,Iscr1,Iscr2,Iscr3,Iscr4
      integer Iscr5,Iscr6,Iscrd,ism,Ispect,ispin,Iwd1,Iwd2,Iwd3,Iws1,Iws
     &2,l,la,leng,Loab,Lpair,Ls,Lspect,Maxbuc,mblock
      integer Mdv,mdv1,mdv11,mdv2,mdv21,METHOD,nb21,NBASIS,nbsq,nij,no,N
     &oa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3
      integer Nobva,Nobvb,Novaa,Novab,Novbb,np,npairs,Nr,Nrorb,nruns,nsb
     &eta,nstb,nv,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      integer nvsta,nvstb
      logical iso,abclos,SAVAIL
      dimension Ibuc(3),Ibout(3)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(10000),V1(10000),Mdv
      common/moc/Cmo(12625),E(175)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/constr/Iopcl
      common/io/In,Iout,Ipunch
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Isc
     &r2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      common/wtild/Cut,Ls(81),Lpair,Nr,Intt,Intc
      common/print/Iprint
      equivalence(Ibuc(1),Iad1),(Ibout(1),Iwd1)
      
      
      
      
      
      
      
      
      
99001 format(' ',i5,i9,i13,e15.4)
99002 format(1H )
      
      call track('DD1SD4')
      
      mdv1=Mdv/3
      mdv11=mdv1+1
      mdv2=mdv1*2
      mdv21=mdv2+1
      nbsq=NBASIS**2
      nb21=NBASIS*(NBASIS-1)/2
      nsbeta=Nrorb*NBASIS+1
      
      l=max0(nb21*Noa3,NBASIS**2*Noaob)
      call defbuc(Iscr1,l)
      call defbuc(Iscr2,Noa*NBASIS)
      call defbuc(Iscr3,Nob*NBASIS)
      if(Iopcl.EQ.0)call defbuc(Iscr4,l)
      
      
      ism=Iopcl+2
      
      do 100 ispin=1,ism
      abclos=.FALSE.
      
      if((Iopcl.NE.0).OR.(ispin.NE.1))then
      
      call tread(Ispect,Cmo,Lspect,1,Lspect,1,0)
      
      if(ispin.EQ.2)then
      
      npairs=Noaob
      la=Nvavb
      nvstb=Nob*NBASIS+nsbeta
      nstb=Nrorb*NBASIS
      if(Iopcl.EQ.0)then
      nvsta=Noa*NBASIS+1
      nstb=0
      nvstb=nvsta
      npairs=Noa*(Noa+1)/2
      abclos=.TRUE.
      endif
      iso=.FALSE.
      Lpair=nbsq
      Ls(1)=0
      do 10 i=2,NBASIS
      Ls(i)=Ls(i-1)+NBASIS
10    continue
      elseif(ispin.EQ.3)then
      
      npairs=Nob3
      la=Nvb3
      nvsta=nvstb
      iso=.TRUE.
      no=Nob
      nv=Nvb
      Lpair=nb21
      do 20 i=1,NBASIS
      i1=i-1
      Ls(i)=i1*NBASIS-i-(i*i1)/2
20    continue
      else
      
      npairs=Noa3
      la=Nva3
      nvsta=Noa*NBASIS+1
      iso=.TRUE.
      no=Noa
      nv=Nva
      Lpair=nb21
      do 30 i=1,NBASIS
      i1=i-1
      Ls(i)=i1*NBASIS-i-(i1*i)/2
30    continue
      endif
      
      if(npairs.NE.0.AND.Lpair.NE.0.AND.la.NE.0)then
      ibuck=Ibuc(ispin)
      call fileio(2,-ibuck,0,0,0)
      call fileio(1,-Iscr1,0,0,0)
      
      
      index=0
      if(abclos)then
      icount=Noa
      ind1=Noa
      ind=0
      endif
      do 40 ij=1,npairs
      leng=la
      call fileio(2,-ibuck,leng,V(mdv11),index)
      if(iso)then
      
      call lsexa(V(mdv11),nv)
      call aatild(V,Cmo(nvsta),V(mdv11),E,nv,NBASIS)
      else
      call abtild(V,Cmo(nvsta),V(mdv11),Cmo(nvstb),E,NBASIS)
      endif
      leng=Lpair
      index=index+la
      if(abclos)then
      if(ij.EQ.icount)then
      ind=ind+la
      index=index+ind
      ind1=ind1-1
      icount=icount+ind1
      endif
      endif
      call fileio(1,Iscr1,leng,V,0)
40    continue
      
      
      ibuck=Ibout(ispin)
      ibucka=ibuck
      if(abclos)ibucka=Iscr4
      call fileio(2,-ibuck,0,0,0)
      call fileio(1,-ibuck,0,0,0)
      call fileio(2,-Iscr1,0,0,0)
      call fileio(1,-Iscr1,0,0,0)
      mblock=Mdv/(2*Lpair)
      nruns=npairs/mblock
      if(mod(npairs,mblock).NE.0)nruns=nruns+1
      np=npairs
      
      do 50 nij=1,nruns
      Intc=0
      Intt=0
      Nr=min0(np,mblock)
      np=np-Nr
      leng=Nr*Lpair
      call fileio(2,Iscr1,leng,V,0)
      do 45 i=1,leng
      V1(i)=Zero
45    continue
      if(iso)call wtilda
      if(.NOT.iso)call wtildb
      leng=Nr*Lpair
      call fileio(1,Iscr1,leng,V1,0)
50    continue
      
      call tread(Ispect,Cmo,Lspect,1,Lspect,1,0)
      
      
      if(.NOT.iso)then
      
      if(abclos.AND.SAVAIL)call comijw(ibuck,ibucka,Nva)
      call tstarb(Iscr1,ibucka,Cmo(nvsta),Cmo(nvstb),E,SAVAIL,NBASIS)
      if(abclos)then
      
      call expijw(ibucka,ibuck,Nva)
      
      if(METHOD.EQ.0.OR.METHOD.EQ.4.OR.METHOD.EQ.5)then
      
      call expijw(Iscr1,Iscr4,NBASIS)
      
      call aaclos(Iscr4,Iscr1,Noa,NBASIS)
      
      
      call wia4a(Iscr1,1,Noa,Nva,Noa3,nb21,NBASIS)
      
      call wia4b(Iscr4,Cmo,Cmo(nstb+1),Noaob,nbsq,NBASIS)
      endif
      goto 200
      endif
      else
      call tstara(Iscr1,ibuck,Cmo(nvsta),no,nv,E,SAVAIL,NBASIS)
      endif
      endif
      
      
      if(METHOD.EQ.0.OR.METHOD.EQ.4.OR.METHOD.EQ.5)then
      if(iso)call wia4a(Iscr1,ispin,no,nv,npairs,Lpair,NBASIS)
      if(.NOT.iso)call wia4b(Iscr1,Cmo,Cmo(nstb+1),npairs,Lpair,NBASIS)
      
      call test(ispin,ibuck)
      if(Iprint.NE.0)write(Iout,99002)
      endif
      endif
      
100   continue
      
200   call fileio(6,0,0,0,0)
      
      return
      
      end
C* :1 * 
      
