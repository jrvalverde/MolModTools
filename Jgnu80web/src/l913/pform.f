
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 pform"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "pform.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "pform.web"
      subroutine pform(NBASIS,NO,NV,CMO)
      implicit none
      double precision a0,A00,a1,Anorm,anorm2,anormi,CMO,Cuts,Dehf,Delma
     &x,Den,Dv,Energy,F42,fact,Four,Half,One,Onept5,P
      double precision Ten,Three,Two,V,W0,Zero
      integer i,Iad1,Iad2,Iad3,Iapr,Ias1,Ias2,Idb1,Idb10,Idb2,Idb3,Idb4,
     &Idb5,Idb6,Idb7,Idb8,Idb9,Idmm,Ieval,Iflag
      integer In,ind1,ind2,ind3,ind4,indscr,Ioab,Iopcl,Iout,Ipcyc,Iprint
     &,Ipunch,Irwfd,Iscr1,Iscr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscr7
      integer Iscr8,Iscr9,Isd,Ispect,Iwd1,Iwd2,Iwd3,Iws1,Iws2,j,k,l,l1,l
     &2,l3,l4,leng,Loab,Lspect,m
      integer Maxbuc,Maxit,Mdv,mdv2,mm,n,NBASIS,Niter,NO,no2,Noa,Noa2,No
     &a3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva
      integer Nobvb,nonv,Norm,Novaa,Novab,Novbb,Nrorb,ntt,NV,nv2,Nva,Nva
     &2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      logical Davail,Savail
      dimension Dv(20000)
      dimension P(20000),CMO(*)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscr7,Iscr8,Iscr9,Iapr(10)
      common/io/In,Iout,Ipunch
      common/rwfden/Irwfd
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      common/constr/Iopcl
      common/print/Iprint
      equivalence(V(1),P(1))
      equivalence(Dv(1),V(1))
      
      
      
      
      
      
      
      
      
99001 format(' CORRECTION TO THE MO DENSITY MATRIX'/' ********** ** *** 
     &** ******* ******')
99002 format(' AO DENSITY MATRIX'/' ** ******* ******')
      
      call track(' PFORM')
      
      call fileio(2,-Iscr6,0,0,0)
      call fileio(2,-Iscr7,0,0,0)
      call fileio(2,-Iscr8,0,0,0)
      
      indscr=Mdv-Nrorb
      anorm2=Anorm**2
      anormi=One/anorm2
      ntt=NBASIS*(NBASIS+1)/2
      no2=NO*NO
      nonv=NO*NV
      nv2=NV*NV
      mdv2=Mdv/2
      ind1=0
      ind2=ind1+no2
      ind3=ind2+nonv
      ind4=mdv2
      leng=no2
      call fileio(2,Iscr6,leng,V,0)
      leng=nonv
      call fileio(2,Iscr8,leng,V(ind2+1),0)
      leng=nv2
      call fileio(2,Iscr7,leng,V(ind3+1),0)
      l1=NO
      l2=NV
      l3=NO+1
      l4=NV+1
      
      do 100 i=1,Nrorb
      if(i.LE.NO)then
      do 20 j=1,l1
      V(ind4+j)=V(ind1+j)
20    continue
      ind4=ind4+l1
      ind1=ind1+l3
      l1=l1-1
      do 40 k=1,NV
      V(ind4+k)=V(ind2+k)
40    continue
      ind4=ind4+NV
      ind2=ind2+NV
      endif
      if(i.GT.NO)then
      do 60 l=1,l2
      V(ind4+l)=V(ind3+l)
60    continue
      ind4=ind4+l2
      ind3=ind3+l4
      l2=l2-1
      endif
100   continue
      
      leng=Nrorb*(Nrorb+1)/2
      do 200 i=1,leng
      V(mdv2+i)=V(mdv2+i)*anormi
200   continue
      call fileio(1,-Iscr1,leng,V(mdv2+1),0)
      call expsym(Nrorb,Iscr1,Iscr2)
      leng=Nrorb*Nrorb
      call fileio(2,-Iscr2,leng,V,0)
      if(Iprint.NE.0)then
      write(Iout,99001)
      call ltoutd(Nrorb,V,1)
      endif
      
      mm=0
      do 400 m=1,NBASIS
      ind1=0
      do 250 j=1,Nrorb
      a0=Zero
      ind2=0
      do 220 i=1,Nrorb
      a0=a0+P(ind1+i)*CMO(ind2+m)
      ind2=ind2+NBASIS
220   continue
      V(indscr+j)=a0
      ind1=ind1+Nrorb
250   continue
      do 300 n=1,m
      a0=Zero
      ind3=0
      do 260 j=1,Nrorb
      a0=a0+V(indscr+j)*CMO(ind3+n)
      ind3=ind3+NBASIS
260   continue
      mm=mm+1
      V(mdv2+mm)=a0
300   continue
400   continue
      leng=ntt
      fact=One
      if(Iopcl.EQ.0)fact=Two
      
      call tread(Irwfd,Dv,ntt,1,ntt,1,0)
      do 500 i=1,ntt
      a1=fact*V(mdv2+i)
      Dv(i)=Dv(i)+(a1)
500   continue
      call twrite(Irwfd,Dv,ntt,1,ntt,1,0)
      
      if(Iprint.NE.0)then
      write(Iout,99002)
      call ltoutd(NBASIS,Dv,1)
      endif
      
      return
      
      end
C* :1 * 
      
