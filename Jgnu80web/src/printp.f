
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 printp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "printp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "printp.web"
      subroutine printp(NAE)
      implicit none
      double precision a0,A00,a1,aa,aat,ab,abt,ane,Anorm,anorm2,bb,bbt,C
     &mo,Cuts,Dehf,Delmax,Den,Energy,F42,Four
      double precision Half,One,Onept5,Ten,Three,Two,V,W0,Zero
      integer i,i1,Iad1,Iad2,Iad3,Ias1,Ias2,Idb1,Idb10,Idb2,Idb3,Idb4,Id
     &b5,Idb6,Idb7,Idb8,Idb9,Idmm,Ieval,Ifill
      integer Iflag,ij,ilocal,In,Ioab,Iopcl,Iout,Ipcyc,Ipunch,is,Isd,Isp
     &ect,Iwd1,Iwd2,Iwd3,Iws1,Iws2,j,js,Loab
      integer Lspect,m,max,Maxbuc,Maxit,Mdv,mdv2,n0,n1,n2,n3,n4,n5,NAE,n
     &d,Niter,no1,Noa,Noa2,Noa3
      integer Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Norm,Novaa,Nov
     &ab,Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      logical Davail,Savail
      dimension Cmo(20000)
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/constr/Iopcl
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Ifill(19
     &)
      common/io/In,Iout,Ipunch
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      equivalence(Cmo(1),V(1))
      data ilocal/3333/
      
      
      
      
      
      
      
      
      
99001 format(' ENERGIES ARE NORMALIZED TO SUM(S) A(S)**2 = 1')
99002 format(' PAIR ENERGIES AND WEIGHTS'/16x,'ALPHA-ALPHA',33x,'ALPHA-B
     &ETA'/16x,11(1H*),33x,10(1H*))
99003 format(' PAIR ENERGIES AND WEIGHTS'/16x,'ALPHA-ALPHA',33x,'ALPHA-B
     &ETA',34x,'BETA-BETA'/16x,11(1H*),33x,10(1H*),34x,9(1H*))
99004 format(1H )
99005 format(' ',i7,i3,1x,e15.8,e17.8)
99006 format(' ',43x,i8,i3,1x,e15.8,e17.8)
99007 format(' ',87x,i8,i3,1x,e15.8,e17.8)
99008 format(' SUM:',7x,e15.8,e17.8,12x,e15.8,e17.8)
99009 format(' SUM:',7x,e15.8,e17.8,12x,e15.8,e17.8,12x,e15.8,e17.8)
      
      call track('PRINTP')
      
      call defbuc(ilocal,Novab)
      mdv2=Mdv/2
      call mattrn(Noa,Nva,Nob,Nvb,2,Idb2,ilocal,mdv2)
      
      nd=NAE-Noa
      n0=2*max0(Nvb3,Nvavb)+1
      n1=n0+Noa3
      n2=n1+Noa3
      n3=n2+Noaob
      n4=n3+Noaob
      n5=n4+Nob3
      if(n5+Nob3.LE.Mdv)then
      call scalp1(Iad1,Idb1,Cmo(n0),Nva3,Noa3)
      call scalp1(Iad1,Iad1,Cmo(n1),Nva3,Noa3)
      call scalp1(Iad2,ilocal,Cmo(n2),Nvavb,Noaob)
      call scalp1(Iad2,Iad2,Cmo(n3),Nvavb,Noaob)
      if(Iopcl.NE.0)then
      call scalp1(Iad3,Idb3,Cmo(n4),Nvb3,Nob3)
      call scalp1(Iad3,Iad3,Cmo(n5),Nvb3,Nob3)
      endif
      n0=n0-1
      n1=n1-1
      n2=n2-1
      n3=n3-1
      n4=n4-1
      n5=n5-1
      if(Iopcl.EQ.0)write(Iout,99002)
      if(Iopcl.NE.0)write(Iout,99003)
      if(dabs(A00).GT.1.E-4)then
      
      ane=One/A00
      else
      ane=One/Anorm
      write(Iout,99001)
      endif
      anorm2=One/Anorm**2
      max=max0(Noa3,Noaob)
      aa=Zero
      ab=Zero
      bb=Zero
      aat=Zero
      abt=Zero
      bbt=Zero
      do 50 ij=1,max
      write(Iout,99004)
      no1=Noa-1
      if(no1.GT.0)then
      m=0
      do 10 i=1,no1
      i1=i+1
      do 5 j=i1,Noa
      m=m+1
      if(m.EQ.ij)then
      a0=Cmo(n0+ij)*ane
      aa=aa+a0
      a1=Cmo(n1+ij)*anorm2
      aat=aat+a1
      is=i+nd
      js=j+nd
      write(Iout,99005)is,js,a0,a1
      endif
5     continue
10    continue
      endif
      if(Noa.NE.0.AND.Nob.NE.0)then
      m=0
      do 20 i=1,Noa
      do 15 j=1,Nob
      m=m+1
      if(m.EQ.ij)then
      a0=Cmo(n2+ij)*ane
      ab=ab+a0
      a1=Cmo(n3+ij)*anorm2
      abt=abt+a1
      is=i+nd
      js=j+nd
      write(Iout,99006)is,js,a0,a1
      endif
15    continue
20    continue
      endif
      if(Iopcl.NE.0)then
      no1=Nob-1
      if(no1.GT.0)then
      m=0
      do 25 i=1,no1
      i1=i+1
      do 22 j=i1,Nob
      m=m+1
      if(m.EQ.ij)then
      a0=Cmo(n4+ij)*ane
      bb=bb+a0
      a1=Cmo(n5+ij)*anorm2
      bbt=bbt+a1
      is=i+nd
      js=j+nd
      write(Iout,99007)is,js,a0,a1
      endif
22    continue
25    continue
      endif
      endif
50    continue
      if(Iopcl.EQ.0)write(Iout,99008)aa,aat,ab,abt
      if(Iopcl.NE.0)write(Iout,99009)aa,aat,ab,abt,bb,bbt
      endif
      
      return
      
      end
C* :1 * 
      
