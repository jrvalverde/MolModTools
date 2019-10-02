
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 geom"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "geom.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 87 "geom.web"
      subroutine geom(JUMP)
      implicit none
      double precision Alpha,Atmchg,Beta,Bl,C,Phycon,Toang,work
      integer i,i1,i2,i3,i4,i5,i6,i7,Ian,Ianz,Icharg,idout,idump,iend,In
     &,Iop,Iout,iozmat,iprint,Ipunch
      integer irwchg,irwzc,Iz,JUMP,Lalpha,Lbeta,Lbl,maxnz,Multip,mxwork,
     &Nae,Natoms,Nbasis,Nbe,ndchg,Ne,Nvar,Nz,nz3
      logical ttest,error
      dimension iozmat(2)
      dimension work(1000)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/phycon/Toang,Phycon(29)
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alpha(50),Beta(50),Lbl(50),La
     &lpha(50),Lbeta(50),Nz,Nvar
      common/io/In,Iout,Ipunch
      data iozmat/507,351/,maxnz/50/,irwzc/572/,irwchg/577/
      data mxwork/1000/,ndchg/0/
      
      
99001 format(' ALLOCATION FAILURE IN GEOM.'/'  NEEDS ',i6,' BUT HAS ONLY
     &',i6,' WORDS.')
99002 format(18x,'Z-MATRIX ORIENTATION:')
99003 format(20x,'DISTANCE MATRIX (ANGSTROMS)')
99004 format(1x,18x,'INPUT ORIENTATION:')
99005 format(1x,'PROBLEM WITH THE Z-MATRIX IN LINK 202')
99006 format(1x,'PROBLEM WITH THE DISTANCE MATRIX IN LINK 202')
99007 format(1x,'NON-DEFAULT ATOMIC CHARGES: (BEFORE COMPRESS)')
99008 format(1x,i10,f10.5)
      
      
      iprint=Iop(33)+Iop(34)
      idump=Iop(34)
      
      if(Iop(15).NE.0)call ilsw(1,26,1)
      
      
      if(Iop(29).NE.1)then
      
      call tread(iozmat(1),Ianz,iozmat(2),1,iozmat(2),1,0)
      
      nz3=3*Nz
      i1=1
      i2=i1+Nz
      i3=i2+Nz
      i4=i3+Nz
      i5=i4+Nz
      i6=i5+Nz
      i7=i6+nz3
      iend=i7+Nz-1
      if(iend.GT.mxwork)then
      write(Iout,99001)iend,mxwork
      call lnk1e
      endif
      
      call subvar(Bl,Alpha,Beta,Lbl,Lalpha,Lbeta,Nz,Nvar)
      
      call twrite(iozmat(1),Ianz,iozmat(2),1,iozmat(2),1,0)
      
      ttest=Iop(10).EQ.0
      if(Iop(11).EQ.0)call zprint(Nz,Ianz,Iz,Bl,Alpha,Beta,Toang)
      call tquery(irwchg,ndchg)
      if(ndchg.NE.0)then
      call tread(irwchg,work(i7),Nz,1,Nz,1,0)
      if(idump.NE.0)then
      write(Iout,99007)
      do 10 i=1,Nz
      In=i7+i-1
      write(Iout,99008)i,work(In)
10    continue
      endif
      endif
      call ztoc(maxnz,Nz,Ianz,Iz,Bl,Alpha,Beta,ttest,Natoms,Ian,C,work(i
     &6),work(i1),work(i2),work(i3),work(i4),work(i5),Iout,error,work(i7
     &))
      if(error)then
      write(Iout,99005)
      call lnk1e
      endif
      
      call twrite(irwzc,work(i6),nz3,1,nz3,1,0)
      
      if(ttest)call twrite(iozmat(1),Ianz,iozmat(2),1,iozmat(2),1,0)
      endif
      if(Iop(11).EQ.0)then
      if(Iop(29).EQ.0)write(Iout,99002)
      if(Iop(29).EQ.1)write(Iout,99004)
      if(Iop(29).EQ.0)call corpr1(Nz,Ianz,work(i6),Toang)
      if(Iop(29).EQ.1)call corpr1(Natoms,Ian,C,Toang)
      endif
      
      idout=Iout
      if((Natoms.LE.2).OR.(Iop(9).NE.0))idout=0
      if(idout.NE.0)write(idout,99003)
      call dismat(Natoms,Ian,C,2,5,idout,error,Iop(12),Toang)
      if(error)then
      write(Iout,99006)
      call lnk1e
      endif
      
      call filchg(Natoms,Ian,Atmchg,work(i7),ndchg,iprint)
      
      call symm(Iop,Natoms,Multip,Icharg,Ian,C,Atmchg,Toang)
      
      JUMP=0
      return
      
      end
C* :1 * 
      
