
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cids1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cids1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 150 "cids1.web"
      subroutine cids1(JUMP)
      implicit none
      double precision A00,A0old,Anorm,Atmchg,C,Cmo,cut1,Cuts,Dehf,del,D
     &elmax,Den,Dep,Dv,E,Ee,Energy,F42,Filmoc,Four
      double precision Half,One,Onept5,Qep,Ten,tenp7,Three,Two,V,W0,Zero
      integer i,Iad1,Iad2,Iad3,Ian,Iapr,Ias1,Ias2,Ibckt,Icharg,Icivar,ic
     &ut,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7
      integer Idb8,Idb9,ideriv,Idimmy,Idmm,Idummy,Idump,Ieval,Iextrp,Ifl
     &ag,Igen,Igeno,In,Inforb,inhexp,Inr,Ioab,Iop,Iopcl,Iout
      integer ipairp,Ipcyc,Iprint,Ipunch,ischem,Iscr1,Iscr2,Iscr3,Iscr4,
     &Iscr5,Iscr6,Iscr7,Iscr8,Iscr9,Isd,Ispect,iupdat,Iw1sav,Iw2sav,Iwd1
      integer Iwd2,Iwd3,Iws1,Iws2,JUMP,La0,Lanorm,Lccd,Lcisd4,Lcivar,Ldq
     &4,Lecid,Lecids,Lehf,Lenrgy,Lextrp,Ligen,Lisd,Llccd,Llccd4
      integer Lmp2,Lmp3,Lnforb,Loab,Lscc1,Lsdq4,Lsdtq4,Lspect,Lvar1,Maxb
     &uc,Maxit,Mdim,Mdv,mdv2,method,mit,Multip,Nae,Natoms,Nbasis
      integer Nbe,Ne,Niter,Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3
     &,Nobuc,nobuc1,nobuc9,Nobva,Nobvb,Norm,Novaa,Novab
      integer Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      logical Davail,Savail
      dimension Ibckt(50),Dv(6225),E(175),Igen(500)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/v/V(20000),Mdv
      common/rwfp/Igeno,Ligen,Inforb,Lnforb,Icivar,Lcivar,Iextrp,Lextrp
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      common/extrap/Dep(2),Qep,A0old,Inr,Idummy
      common/nobuc/Nobuc
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscr7,Iscr8,Iscr9,Iapr(10)
      common/locgen/Lehf,Lmp2,Lmp3,Ldq4,Lsdq4,Lsdtq4,Lecid,Lecids,Lscc1,
     &Lccd,Llccd,Lvar1,Lcisd4,Llccd4,Lenrgy,Lanorm,La0,Lisd
      common/moc/Cmo(6225),Ee(175),Filmoc(6400)
      common/mdmax/Mdim
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/wsav/Iw1sav,Iw2sav
      common/onstr/Iopcl
      common/io/In,Iout,Ipunch
      common/print/Iprint
      common/dump/Idump,Idimmy
      equivalence(Ibckt(1),Idb1)
      equivalence(Dv(1),Igen(1),Cmo(1)),(E(1),Ee(1))
      data tenp7/1.D-7/
      
      
      
99001 format(i2,d18.13)
99002 format(g20.14)
99003 format(i3)
99004 format(' CONFIGURATION INTERACTION WITH SINGLE- AND DOUBLE SUBSTIT
     &UTIONS',/1x,63(1H*))
99005 format(' CONFIGURATION INTERACTION WITH DOUBLE SUBSTITUTIONS',/1x,
     &51(1H*))
99006 format(' MOLLER-PLESSET THIRD ORDER PERTURBATION THEORY',/1x,46(1H
     &*))
99007 format(' UMP4 WITH DOUBLES AND QUADRUPLES',/1x,32(1H*))
99008 format(' UMP4 WITH SINGLES,DOUBLES AND QUADRUPLES',/1x,40(1H*))
99009 format(' MP4(SDTQ)',/1x,9(1H*))
99010 format(' COUPLED CLUSTER THEORY WITH DOUBLE SUBSTITUTIONS',/1x,48(
     &1H*))
99011 format(' ILLEGAL METHOD',/1x,14(1H*))
99012 format(' ITERATION METHOD: DE=')
99013 format(' ',22x,'W(0)/A0')
99014 format(' A.O.-INTEGRAL CUTOFF',22x,d16.8)
99015 format(' ITERATIONS=',i3,3x,'CONVERGENCE=',d10.3)
99016 format(' ',22x,'0')
99017 format(' ',22x,'ILLEGAL')
99018 format(' ',42x,'NORMALIZATION: A(0)=1')
99019 format(' ',42x,'NORMALIZATION: SUM(S) A(S)**2 = 1')
      
      
      method=Iop(5)
      mit=Iop(6)
      iupdat=Iop(7)
      ideriv=Iop(15)
      ischem=Iop(18)+1
      inhexp=Iop(19)
      icut=Iop(20)
      ipairp=Iop(25)
      Norm=Iop(26)
      Iprint=Iop(33)
      Idump=Iop(34)
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      mdv2=Mdv/2
      
      if(method.EQ.0)write(Iout,99004)
      if(method.EQ.1)write(Iout,99005)
      if(method.EQ.2)write(Iout,99006)
      if(method.EQ.3)write(Iout,99007)
      if(method.EQ.4)write(Iout,99008)
      if(method.EQ.5)write(Iout,99009)
      if(method.EQ.6)write(Iout,99010)
      if(method.GE.7)write(Iout,99011)
      if(method.LT.7)then
      
      if(method.LT.2.OR.method.GT.5)then
      if(ischem.GT.1)write(Iout,99012)
      if(ischem.EQ.2)write(Iout,99013)
      if(ischem.EQ.3)write(Iout,99016)
      if(ischem.GT.3)write(Iout,99017)
      if(ischem.GT.3)goto 100
      endif
      
      if(Nbasis.LE.Mdim)then
      
      call ilsw(2,1,Iopcl)
      if(Iopcl.LT.2)then
      
      del=Zero
      if(mit.EQ.7)read(In,99001)mit,del
      if(mit.GT.0)Maxit=mit
      if(method.GE.2.AND.method.LE.5)Maxit=1
      if(ideriv.GT.0)Delmax=tenp7
      if(del.GT.Zero)Delmax=del
      if(method.LT.2.OR.method.GT.5)write(Iout,99015)Maxit,Delmax
      
      cut1=Zero
      if(icut.EQ.1)read(In,99002)cut1
      if(cut1.GT.Zero)Cuts=cut1
      if(cut1.GT.Zero)write(Iout,99014)Cuts
      
      Ipcyc=0
      if(ipairp.EQ.3)read(In,99003)Ipcyc
      
      call tread(Igeno,Dv,Ligen,1,Ligen,1,0)
      Dehf=Dv(Lehf)
      
      Davail=.TRUE.
      Savail=.FALSE.
      A00=One
      
      Energy=Zero
      Inr=0
      Qep=Zero
      
      Niter=0
      if(Norm.EQ.0.AND.method.LE.1)write(Iout,99018)
      if(Norm.EQ.1.AND.method.LE.1)write(Iout,99019)
      
      
      do 10 i=1,Nobuc
      Ibckt(i)=i
10    continue
      nobuc1=Nobuc+1
      nobuc9=Nobuc+9
      do 20 i=nobuc1,nobuc9
      Ibckt(i)=3000+i
20    continue
      
      if(Iopcl.EQ.0)then
      Ias2=Ias1
      Iws2=Iws1
      Iad3=Iad1
      Iwd3=Iwd1
      Idb3=Idb1
      endif
      
      if(Noava.GT.0)call conddf(Ias1,Noava)
      if(Novaa.GT.0)call conddf(Iad1,Novaa)
      if(Novab.GT.0)call conddf(Iad2,Novab)
      if(.NOT.Savail)call inibuc(Ias1,Noava,Zero)
      if(.NOT.(Davail))then
      call inibuc(Iad1,Novaa,Zero)
      call inibuc(Iad2,Novab,Zero)
      endif
      if(Iopcl.NE.0)then
      if(Nobvb.GT.0)call conddf(Ias2,Nobvb)
      if(Novbb.GT.0)call conddf(Iad3,Novbb)
      if(.NOT.Savail)call inibuc(Ias2,Nobvb,Zero)
      if(.NOT.Davail)call inibuc(Iad3,Novbb,Zero)
      endif
      
      call normds
      
      if(ipairp.EQ.2.OR.ipairp.EQ.3.AND.Ipcyc.EQ.0)call printp(Nae)
      
      if(Novaa.GT.0)call conddf(Iwd1,Novaa)
      if(Novab.GT.0)call conddf(Iwd2,Novab)
      if(Iopcl.NE.0.AND.Novbb.GT.0)call conddf(Iwd3,Novbb)
      if(Noava.GT.0)call conddf(Iws1,Noava)
      if(Iopcl.NE.0.AND.Nobvb.GT.0)call conddf(Iws2,Nobvb)
      if(.NOT.Savail)call inibuc(Iws1,Noava,Zero)
      if(.NOT.(Davail))then
      call inibuc(Iwd1,Novaa,Zero)
      call inibuc(Iwd2,Novab,Zero)
      endif
      if(Iopcl.NE.0)then
      if(.NOT.Savail)call inibuc(Iws2,Nobvb,Zero)
      if(.NOT.Davail)call inibuc(Iwd3,Novbb,Zero)
      endif
      
      if(ideriv.GT.0)call conddf(Iw1sav,Novab)
      if(ideriv.GT.0)call conddf(Iw2sav,Novab)
      
      call twrite(Icivar,A00,Lcivar,1,Lcivar,1,0)
      call twrite(Iextrp,Dep,Lextrp,1,Lextrp,1,0)
      
      
      if(Idump.GT.1)call fdump
      JUMP=101
      return
      endif
      endif
      endif
      
100   call lnk1e
      
      stop 909
      
      end
C* :1 * 
      
