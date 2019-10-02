
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 window"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "window.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 169 "window.web"
      subroutine window(JUMP)
      implicit none
      double precision Ad,As,Atmchg,Bd,C,F42,Filler,Four,Half,One,Onept5
     &,Ten,Three,Two,Zero
      integer i,i1,i2,Ian,iani,Icharg,icip,iend,iendp,Ieval,Ievalf,In,in
     &d1,Indcon,Inforb,Ioab,Ioc,Iop,iopcl,Iouab
      integer Iout,iprint,Ipunch,Ispect,ist,istm,itest,iwdo,j,JUMP,kl,ks
     &m,kspin,LENFIL,Lnforb,Loab,Lspect,m,MAXBAS,Maxbuc
      integer MEMLEN,Multip,Nae,Natoms,nb2,Nbasis,Nbe,nbs2,nbsq,Ne,Noa,N
     &oa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva
      integer Nobvb,Novaa,Novab,Novbb,Nrorb,nrorb2,nsbeta,NSQMAX,nst,Nva
     &,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      parameter(MAXBAS=150,MEMLEN=50000,NSQMAX=(MAXBAS*MAXBAS),LENFIL=(M
     &EMLEN-2*NSQMAX-MAXBAS))
      
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/memry/As(NSQMAX),Ad(NSQMAX),Bd(MAXBAS),Filler(LENFIL)
      common/scfrwf/Ievalf,Iouab,Ioc(2)
      common/comorb/Inforb,Lnforb
      common/io/In,Iout,Ipunch
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/conv/Indcon
      
      
99001 format(2I3)
99002 format(' RANGE OF M.O.''S USED FOR CORRELATION:',2I4)
99003 format(' NOA=',i3,5x,'NVA=',i3,10x,'NOB=',i3,5x,'NVB=',i3)
99004 format(/' CAUTION: TEST WINDOW IS USED'/)
99005 format(' *** THERE IS NO CORRELATION ENERGY FOR THIS SYSTEM ***')
99006 format(4I3)
      
      
      call drum
      
      iwdo=Iop(10)
      itest=Iop(30)
      icip=Iop(31)
      iprint=Iop(33)
      
      call ilsw(2,1,iopcl)
      ksm=iopcl+1
      nb2=ksm*Nbasis
      nbsq=Nbasis**2
      
      call tread(Ievalf,Ad,nb2,1,nb2,1,0)
      
      ist=1
      iend=Nbasis
      if(iwdo.EQ.2)read(In,*)ist,iend
      if(ist.EQ.0)ist=1
      if(iend.EQ.0)iend=Nbasis
      if(iwdo.EQ.1)then
      ist=1
      iend=Nbasis
      do 50 i=1,Natoms
      iani=Ian(i)
      if(iani.GT.10)then
      ist=ist+1
      
      elseif(iani.GT.2)then
      ist=ist+1
      endif
50    continue
      endif
      write(Iout,99002)ist,iend
      if(itest.NE.0)then
      
      
      write(Iout,99004)
      if(iopcl.EQ.0)then
      do 60 i=1,Nbasis
      Ad(i+Nbasis)=Ad(i)
60    continue
      endif
      nb2=2*Nbasis
      call twrite(Ieval,Ad,nb2,1,nb2,1,0)
      
      nbsq=Nbasis**2
      nbs2=nbsq*ksm
      istm=ist-1
      iendp=iend+1
      do 150 kspin=1,ksm
      call tread(Ioc(kspin),Ad,Nbasis,Nbasis,Nbasis,Nbasis,0)
      nsbeta=(kspin-1)*nbsq
      if(istm.GT.0)then
      kl=istm*Nbasis
      do 70 i=1,kl
      As(i+nsbeta)=Zero
70    continue
      endif
      do 100 i=ist,iend
      i1=(i-1)*Nbasis
      i2=i1+nsbeta
      do 80 j=1,Nbasis
      As(i2+j)=Ad(i1+j)
80    continue
100   continue
      if(iendp.LT.Nbasis)then
      kl=(Nbasis-iend)*Nbasis
      ind1=iend*Nbasis+nsbeta
      do 110 i=1,kl
      As(i+ind1)=Zero
110   continue
      endif
150   continue
      Lspect=nbs2
      call twrite(Ispect,As,Lspect,1,Lspect,1,0)
      
      Nrorb=Nbasis
      ist=1
      iend=Nbasis
      else
      
      
      Nrorb=iend-ist+1
      nsbeta=Nrorb*Nbasis
      
      m=0
      do 200 i=ist,iend
      m=m+1
      Bd(m)=Ad(i)
200   continue
      nst=iopcl*Nbasis
      do 250 i=ist,iend
      m=m+1
      Bd(m)=Ad(i+nst)
250   continue
      nrorb2=2*Nrorb
      call twrite(Ieval,Bd,nrorb2,1,nrorb2,1,0)
      
      m=0
      do 300 kspin=1,ksm
      call tread(Ioc(kspin),Ad,Nbasis,Nbasis,Nbasis,Nbasis,0)
      do 280 i=ist,iend
      i1=(i-1)*Nbasis
      do 260 j=1,Nbasis
      m=m+1
      As(m)=Ad(i1+j)
260   continue
280   continue
300   continue
      
      Lspect=m
      call twrite(Ispect,As,Lspect,1,Lspect,1,0)
      endif
      
      
      istm=ist-1
      Noa=Nae-istm
      Nob=Nbe-istm
      Nva=Nrorb-Noa
      Nvb=Nrorb-Nob
      if((Noa+Nob).LE.1.OR.(Nva+Nvb).LE.1)write(Iout,99005)
      if((Noa+Nob).LE.1.OR.(Nva+Nvb).LE.1)call lnk1e
      if(iprint.GT.0)write(Iout,99003)Noa,Nva,Nob,Nvb
      Noaob=Noa*Nob
      Noava=Noa*Nva
      Noavb=Noa*Nvb
      Nobva=Nob*Nva
      Nobvb=Nob*Nvb
      Noavb=Noa*Nvb
      Nvavb=Nva*Nvb
      Noa2=Noa*(Noa+1)/2
      Noa3=Noa*(Noa-1)/2
      Nob2=Nob*(Nob+1)/2
      Nob3=Nob*(Nob-1)/2
      Nva2=Nva*(Nva+1)/2
      Nva3=Nva*(Nva-1)/2
      Nvb2=Nvb*(Nvb+1)/2
      Nvb3=Nvb*(Nvb-1)/2
      Novaa=Noa3*Nva3
      Novab=Noava*Nobvb
      Novbb=Nob3*Nvb3
      if(iprint.GT.2)then
      call matout(As,Nbasis,Nrorb,Nbasis,Nrorb)
      if(iopcl.GT.0)call matout(As(nsbeta+1),Nbasis,Nrorb,Nbasis,Nrorb)
      endif
      
      
      if(iopcl.NE.0)then
      call tread(Iouab,Ad,Nbasis,Nbasis,Nbasis,Nbasis,0)
      m=0
      do 350 i=ist,iend
      i1=(i-1)*Nbasis
      do 320 j=ist,iend
      m=m+1
      As(m)=Ad(i1+j)
320   continue
350   continue
      Loab=m
      call twrite(Ioab,As,Loab,1,Loab,1,0)
      endif
      call twrite(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      
      if(icip.EQ.1)call ciprm
      
      JUMP=0
      
      return
      
      end
C* :1 * 
      
