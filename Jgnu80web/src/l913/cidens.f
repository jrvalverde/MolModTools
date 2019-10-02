
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cidens"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cidens.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "cidens.web"
      subroutine cidens(NBASIS)
      implicit none
      double precision A00,Anorm,Cmo,Cuts,Dehf,Delmax,Den,Dv,Ee,Energy,F
     &42,Filmoc,Four,Half,One,Onept5,Ten,Three,Two,V
      double precision W0,Zero
      integer i,Iad1,Iad2,Iad3,Iapr,Ias1,Ias2,ibuc1,ibuc2,ibuc3,ibuc4,ib
     &uc5,ibuc6,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5,Idb6
      integer Idb7,Idb8,Idb9,iden,Idmm,idtot,iend,Ieval,In,ind1,Ioab,Iop
     &cl,Iout,Ipcyc,Iprint,Ipunch,Irwfd,Iscr1,Iscr2,Iscr3
      integer Iscr4,Iscr5,Iscr6,Iscr7,Iscr8,Iscr9,Isd,Ispect,Iwd1,Iwd2,I
     &wd3,Iws1,Iws2,l,Loab,Lspect,m,Maxbuc,Maxit,Mdv
      integer mdv2,n1,n2,n3,n4,n5,n6,n7,NBASIS,Niter,no,no2,no3,Noa,Noa2
     &,Noa3,Noaob,Noava,Noavb,Nob
      integer Nob2,Nob3,Nobva,Nobvb,nonv,Norm,Novaa,Novab,Novbb,Nrorb,ns
     &t,ntt,nv,nv2,nv3,Nva,Nva2,Nva3,Nvavb,Nvb
      integer Nvb2,Nvb3
      logical Davail,Savail
      dimension Dv(20000)
      dimension iden(2)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      common/moc/Cmo(6225),Ee(175),Filmoc(6400)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/constr/Iopcl
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscr7,Iscr8,Iscr9,Iapr(10)
      common/io/In,Iout,Ipunch
      common/rwfden/Irwfd
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iprint,Davail,Savail,Niter
      equivalence(Dv(1),V(1))
      data iden/528,530/
      data idtot/532/
      
      
      
      
      
      
      
      
99001 format(' THE TOTAL DENSITY MATRIX'/' *** ***** ******* ******')
      
      
      call track('CIDENS')
      
      l=Noa**2*Nvb**2
      call defbuc(Iscr1,l)
      call defbuc(Iscr2,l)
      call defbuc(Iscr3,l)
      call defbuc(Iscr4,l)
      m=max0(Noa,Nvb)
      l=Noavb*m
      call defbuc(Iscr5,l)
      call defbuc(Iscr6,l)
      l=m**2
      call defbuc(Iscr7,l)
      call defbuc(Iscr8,l)
      
      mdv2=Mdv/2
      iend=Iopcl+1
      ntt=NBASIS*(NBASIS+1)/2
      
      call tread(Ispect,Cmo,Lspect,1,Lspect,1,0)
      
      do 100 i=1,iend
      if(i.EQ.2)then
      
      no=Nob
      nv=Nvb
      nonv=Nobvb
      n1=Noaob*Nva
      n2=Noava
      n3=Nvavb*Noa
      no3=Nob3
      nv3=Nvb3
      ibuc1=Ias2
      ibuc2=Iad3
      ibuc3=Iad2
      ibuc4=Iscr2
      ibuc5=Ias1
      ibuc6=Iad2
      nst=Nrorb*NBASIS+1
      else
      
      no=Noa
      nv=Nva
      nonv=Noava
      n1=Noaob*Nvb
      n2=Nobvb
      n3=Nvavb*Nob
      no3=Noa3
      nv3=Nva3
      ibuc1=Ias1
      ibuc2=Iad1
      ibuc3=Iscr2
      ibuc4=Iscr3
      ibuc5=Ias2
      ibuc6=Iscr3
      nst=1
      endif
      
      no2=no*no
      nv2=nv*nv
      n4=no3*nv2
      n5=no3*nv
      n6=nv3*no2
      n7=nv3*no
      Irwfd=iden(i)
      
      call trsfr(nonv,ibuc1,Iscr8)
      call matcab(no,nv,nv,ibuc1,Iscr8,Iscr7,1,0)
      
      call expaba(ibuc2,Iscr1,no3,nv)
      call trsfr(n4,Iscr1,Iscr4)
      call matcab(n5,nv,nv,Iscr1,Iscr4,Iscr7,1,1)
      
      
      if(i.NE.2)call mattrn(Noa,Nob,Nva,Nvb,3,Iad2,Iscr2,mdv2)
      call trsfr(Novab,ibuc3,Iscr3)
      call matcab(n1,nv,nv,ibuc3,Iscr3,Iscr7,1,1)
      
      call multvc(Iscr8,nonv,A00)
      
      call expand(ibuc2,Iscr1,no,nv)
      call mattrn(no,no,nv,nv,2,Iscr1,Iscr4,mdv2)
      call matcab(nonv,nonv,1,Iscr4,ibuc1,Iscr8,1,1)
      
      call mattrn(Noa,Nob,Nva,Nvb,2,Iad2,Iscr2,mdv2)
      
      if(i.NE.2)call transp(Noava,Nobvb,Iscr2,Iscr3)
      call matcab(n2,nonv,1,ibuc4,ibuc5,Iscr8,1,1)
      
      call mattrn(1,n2,no,nv,3,ibuc4,Iscr1,mdv2)
      call trsfr(Novab,Iscr1,Iscr3)
      call matcab(n3,no,no,Iscr1,Iscr3,Iscr6,-1,0)
      
      call mattrn(1,1,no,nv,3,ibuc1,Iscr1,mdv2)
      call trsfr(nonv,Iscr1,Iscr2)
      call matcab(nv,no,no,Iscr1,Iscr2,Iscr6,-1,1)
      
      call mattrn(1,no3,nv3,1,2,ibuc2,Iscr4,mdv2)
      call expaba(Iscr4,Iscr2,nv3,no)
      call trsfr(n6,Iscr2,Iscr4)
      call matcab(n7,no,no,Iscr2,Iscr4,Iscr6,-1,1)
      
      call pform(NBASIS,no,nv,Cmo(nst))
100   continue
      
      call tread(iden(1),Dv,ntt,1,ntt,1,0)
      if(Iopcl.NE.0)then
      ind1=ntt
      call tread(iden(2),Dv(ntt+1),ntt,1,ntt,1,0)
      do 150 i=1,ntt
      Dv(i)=Dv(i)+Dv(ind1+i)
150   continue
      endif
      call twrite(idtot,Dv,ntt,1,ntt,1,0)
      write(Iout,99001)
      call ltoutd(NBASIS,Dv,1)
      
      call fileio(6,0,0,0,0)
      
      return
      
      end
C* :1 * 
      
