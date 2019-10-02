
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cids3"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cids3.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "cids3.web"
      subroutine cids3(JUMP)
      implicit none
      double precision A00,Anorm,Atmchg,C,Cut,Cuts,Dehf,Delmax,Den,Den1,
     &Energy,F42,Four,Half,One,Onept5,scalp,Ten,Three,Two
      double precision V,W0,Zero
      integer i,Iad1,Iad2,Iad3,Ian,Ias1,Ias2,Ibasd,Ibase,Ibckt,Ibfpad,Ic
     &harg,Icivar,Icon,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5
      integer Idb6,Idb7,Idb8,Idb9,ideriv,Idmm,Idummy,Idump,Ieval,Iextrp,
     &Ifil,Iflag,Igeno,In,Inforb,Intc,Intcnt,Intt,Ioab,Iop
      integer Iopcl,Iout,Ipcyc,Iprint,Ipunch,Iq,Ireset,Irwibf,Iscr1,Iscr
     &2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd,Isd,Ismode,Ispect,Istat,Itotal
      integer Iux,Iw1sav,Iw2sav,Iwd1,Iwd2,Iwd3,Iws1,Iws2,JUMP,Kntt1,Kntt
     &2,Last,Lcivar,Lenibf,Lextrp,Ligen,Limint,Lnforb,Loab,Lpair
      integer Ls,Lspect,Maxbuc,Maxit,Mdv,method,Mode,Multip,Nae,Natoms,N
     &basis,Nbe,Ne,Niter,Noa,Noa2,Noa3,Noaob,Noava,Noavb
      integer Nob,Nob2,Nob3,Nobuc,nobuc1,nobuc9,Nobva,Nobvb,Norm,Novaa,N
     &ovab,Novbb,Nr,Nrorb,Nrpext,Ntx,Nva,Nva2,Nva3,Nvavb
      integer Nvb,Nvb2,Nvb3,Nwiib,Nwpi
      integer Dbase,Dbasd,dcount
      logical Davail,Savail
      dimension Ibckt(50)
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
      common/nobuc/Nobuc
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      common/wtild/Cut,Ls(81),Lpair,Nr,Intt,Intc
      common/locibf/Irwibf,Lenibf
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/wsav/Iw1sav,Iw2sav
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/constr/Iopcl
      common/io/In,Iout,Ipunch
      common/print/Iprint
      common/dump/Idump,Idummy
      equivalence(Den,Den1)
      equivalence(Ibckt(1),Idb1)
      
      
      
      
      
      
      
      
      
      
99001 format(34x,'CONTRIBUTION FROM DD1=',d18.8)
      
      
      method=Iop(5)
      ideriv=Iop(15)
      Iprint=Iop(33)
      Idump=Iop(34)
      
      call ilsw(2,1,Iopcl)
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      call tread(Icivar,A00,Lcivar,1,Lcivar,1,0)
      
      Den=Zero
      if(Davail)then
      
      do 50 i=1,Nobuc
      Ibckt(i)=i
50    continue
      
      nobuc1=Nobuc+1
      nobuc9=Nobuc+9
      do 100 i=nobuc1,nobuc9
      Ibckt(i)=3000+i
100   continue
      
      if(Iopcl.EQ.0)then
      Ias2=Ias1
      Iws2=Iws1
      Iad3=Iad1
      Iwd3=Iwd1
      Idb3=Idb1
      endif
      
      call tread(Irwibf,Ismode,Lenibf,1,Lenibf,1,0)
      Cut=Cuts
      
      if(method.EQ.0.OR.method.EQ.4.OR.method.EQ.5)call sd5ds5(Davail,Sa
     &vail)
      
      call dd1sd4(Savail,method,Nbasis)
      
      if(Iopcl.EQ.0)call aaclos(Iwd2,Iwd1,Noa,Nva)
      
      if(ideriv.GT.0)call trsfr(Novab,Iwd2,Iw1sav)
      
      if(Iopcl.EQ.0)Den1=Two*scalp(Iad1,Iwd1,Novaa)+scalp(Iad2,Iwd2,Nova
     &b)
      if(Iopcl.NE.0)Den1=scalp(Iad1,Iwd1,Novaa)+scalp(Iad2,Iwd2,Novab)+s
     &calp(Iad3,Iwd3,Novbb)
      if(Iprint.GT.0)write(Iout,99001)Den1
      
      
      call twrite(Icivar,A00,Lcivar,1,Lcivar,1,0)
      endif
      
      
      if(Idump.GT.1)call fdump
      JUMP=101
      
      return
      
      end
C* :1 * 
      
