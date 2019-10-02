
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 normds"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "normds.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "normds.web"
      subroutine normds
      implicit none
      double precision a0,A00,Anorm,aswitc,Cuts,Dehf,Delmax,Den,Energy,F
     &42,Four,Half,One,Onept5,scalp,Ten,Three,Two,V,W0
      double precision Zero
      integer Iad1,Iad2,Iad3,Ias1,Ias2,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5,Id
     &b6,Idb7,Idb8,Idb9,Idmm,Ieval,Iflag,ilocal,In
      integer Ioab,Iopcl,Iout,Ipcyc,Iprint,Ipunch,Iscr1,Iscr2,Iscr3,Iscr
     &4,Iscr5,Iscr6,Iscrd,Isd,Ispect,Iwd1,Iwd2,Iwd3,Iws1,Iws2
      integer Loab,Lspect,Maxbuc,Maxit,Mdv,mdv2,Niter,Noa,Noa2,Noa3,Noao
     &b,Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Norm,Novaa
      integer Novab,Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      logical Davail,Savail
      common/v/V(20000),Mdv
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/constr/Iopcl
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      common/io/In,Iout,Ipunch
      common/print/Iprint
      data aswitc/0.1D0/
      data ilocal/3333/
      
      
      
      
      
      
      
      
99001 format(' A(0)=',d12.5,6x,'ANORM=',d12.5,6x,'W(0)=',d15.8)
99002 format(' NORMALIZATION OF A-VECTOR SWITCHED TO: SUM(S) A(S)**2 = 1
     &')
      
      call track('NORMDS')
      
      mdv2=Mdv/2
      
      Anorm=scalp(Iad1,Iad1,Novaa)+scalp(Ias1,Ias1,Noava)
      if(Iopcl.NE.0)then
      
      Anorm=Anorm+scalp(Iad3,Iad3,Novbb)+scalp(Ias2,Ias2,Nobvb)
      else
      Anorm=Anorm+Anorm
      endif
      Anorm=Anorm+scalp(Iad2,Iad2,Novab)+A00**2
      Anorm=dsqrt(Anorm)
      
      if(dabs(A00).GE.aswitc)then
      
      a0=One/A00
      if(Norm.NE.0)a0=One/Anorm
      else
      if(Norm.EQ.0)write(Iout,99002)
      Norm=1
      a0=One/Anorm
      endif
      if(a0.NE.One)then
      A00=A00*a0
      Anorm=Anorm*a0
      
      call multvc(Iad1,Novaa,a0)
      call multvc(Iad2,Novab,a0)
      call multvc(Ias1,Noava,a0)
      if(Iopcl.NE.0)then
      call multvc(Iad3,Novbb,a0)
      call multvc(Ias2,Nobvb,a0)
      endif
      endif
      
      call defbuc(ilocal,Novab)
      call mattrn(Noa,Nva,Nob,Nvb,2,Idb2,ilocal,mdv2)
      
      W0=scalp(Idb1,Iad1,Novaa)
      if(Iopcl.NE.0)then
      
      W0=W0+scalp(Idb3,Iad3,Novbb)
      else
      W0=W0+W0
      endif
      W0=W0+scalp(ilocal,Iad2,Novab)
      if(Iprint.NE.0)write(Iout,99001)A00,Anorm,W0
      
      call fileio(5,ilocal,0,0,0)
      
      return
      
      end
C* :1 * 
      
