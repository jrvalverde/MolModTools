
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cycclo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cycclo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 34 "cycclo.web"
      subroutine cycclo(A,ETOTAL,EONE,FINAC,ACURCY,NBASIS,NE,CYCLE,NSYMO
     &P,NEQBAS)
      implicit none
      double precision A,ACURCY,EONE,eprev,ETOTAL,etwo,FINAC,gabs,preduc
     &,pt5,scftrc,Thresh,two,vshift,zero
      integer i,Icount,ieflag,iend,Iflag,Ilshft,In,inda,inda1,inda2,inda
     &a,indb,indb1,indb2,indbb,Iop3,Ioscl,Iout,Ipunch,Irwc
      integer Irwc1,Irwc2,Irwc3,Irweig,Irwf,Irwgen,Irwh,Irwibf,Irwlc,Irw
     &le,Irwlp,Irwp,Irwpt,Irws,Irwt,Irwtm,Irww,istart,Key,Labdck
      integer Length,Lenibf,Locrho,MAXBAS,Maxcyc,NBASIS,Ncyset,NE,NEQBAS
     &,Nlshft,nocc,Nsq,NSYMOP,Ntt
      parameter(MAXBAS=150)
      integer CYCLE,Print,Punden,Inhibe,Engcon,Psave
      dimension A(*),NEQBAS(MAXBAS,8)
      
      integer direct
      common/iop3/Iop3(50)
      
      common/ntt/Ntt,Length,Nsq
      common/irw501/Irwgen,Irws,Irwh,Irwt,Irweig,Irwc,Irwp,Irwpt,Irwf,Ir
     &wc1,Irwc2,Irwc3,Irwtm,Irwibf,Lenibf,Irwle,Irwlc,Irwlp,Irww
      common/psave/Psave
      common/convg/Icount,Iflag,Key
      common/io/In,Iout,Ipunch
      common/scfops/Print,Maxcyc,Punden,Locrho,Engcon,Inhibe,Ncyset,Iosc
     &l,Ilshft,Nlshft
      common/thresh/Thresh
      common/punlab/Labdck(4,3)
      data zero/0.0D0/,pt5/0.5D0/,two/2.0D0/
      
      
      
99001 format(6x,d22.15)
99002 format(' M. O. COEFFICIENTS AT CYCLE ',i3,'.')
99003 format(25H DENSITY MATRIX AT CYCLE ,i3,'.')
99004 format(10('>'),' CONVERGENCE CRITERION NOT MET.')
99005 format(1x,i3)
99006 format(7x,' (NON-VARIATIONAL)')
99007 format(' FOCK-MATRIX IN ORTHOGONAL BASIS:')
99008 format(' EIGENVALUES OF THE FOCK MATRIX AT CYCLE ',i3/(1x,i3,1x,d2
     &0.13))
99009 format(' CORE-HAMILTONIAN')
99010 format(' EIGENVECTORS FROM DIAGONALIZATION:')
99011 format(' TRANSFORMATION MATRIX:')
99012 format(' ',70('*')/' BEGIN CYCLE ',i3/' ',70(1H*))
99013 format(' ',20('>'),'TERMINATED DUE TO OSCILLATING ENERGY.')
      vshift=float(Ilshft)/1000
      direct=Iop3(45)
      if(direct.NE.0)direct=1
      
      
      inda=1
      inda1=1
      inda2=Ntt+1
      indaa=Nsq+1
      
      indb=Ntt+Ntt+1
      indb1=indb
      indb2=indb+Ntt
      indbb=1+NBASIS+2*Nsq
      eprev=zero
      CYCLE=0
      nocc=NE/2
      
      do 100 i=1,Length
      A(i)=zero
100   continue
      
      Icount=0
      Iflag=0
      Key=1
      
      call tread(Irwlp,A(inda1),Ntt,1,Ntt,1,0)
      
      
      
      
      
      
      
      
200   CYCLE=CYCLE+1
      if(Print.GT.2)write(Iout,99012)CYCLE
      
      ieflag=Iflag
      
      call tread(Irwh,A(inda2),Ntt,1,Ntt,1,0)
      
      
      if(CYCLE.EQ.1)then
      if(Print.GT.2)then
      write(Iout,99009)
      call ltoutd(NBASIS,A(inda2),1)
      endif
      endif
      
      
      
      EONE=scftrc(A(inda1),A(inda2),NBASIS,1)
      if(Psave.EQ.0)write(Iout,99005)CYCLE
      
      
      
      if(direct.NE.0)then
      call dirclo(NBASIS,A(inda1),A(inda2),NSYMOP,NEQBAS,Iop3)
      else
      call fofclo(NBASIS,CYCLE,A(inda1),A(inda2),A(indb),A(indb),Print,N
     &SYMOP,NEQBAS)
      endif
      
      
      
      call twrite(Irwf,A(inda2),Ntt,1,Ntt,1,0)
      
      etwo=scftrc(A(inda1),A(inda2),NBASIS,1)
      ETOTAL=pt5*(EONE+etwo)
      if(ieflag.LE.0)then
      if(Psave.EQ.0)write(Iout,99001)ETOTAL
      else
      
      if(Psave.EQ.0)write(Iout,99006)
      endif
      
      if(CYCLE.GT.Nlshft)then
      Ilshft=0
      vshift=0.0D00
      endif
      
      if((CYCLE.GT.1).AND.Ilshft.NE.0)call levshf(A(inda2),A(inda1),A(in
     &db),A(indbb),NBASIS,NBASIS,nocc,vshift,Irwlc,Irws,Irwlp)
      
      call tread(Irwtm,A(indb),NBASIS,NBASIS,NBASIS,NBASIS,0)
      
      
      
      
      call fvmul(NBASIS,A(inda2),A(indb),A(inda),A(indbb))
      
      call vdagt(NBASIS,A(indb),A(inda),A(inda1),A(indbb))
      
      
      if(Print.GE.3)then
      write(Iout,99007)
      call ltoutd(NBASIS,A(inda1),1)
      endif
      
      call diagd(A(inda1),A(indb),A(indbb),NBASIS,A(indaa),A(inda2),NBAS
     &IS,.FALSE.)
      
      
      call tread(Irwtm,A(inda),NBASIS,NBASIS,NBASIS,NBASIS,0)
      
      if(Print.GE.3)then
      write(Iout,99010)
      call linout(A(indb),NBASIS,1)
      write(Iout,99011)
      call linout(A(inda),NBASIS,1)
      endif
      
      
      
      call cvu(NBASIS,A(inda),A(indb),A(indaa))
      
      
      
      call twrite(Irwle,A(indbb),NBASIS,1,NBASIS,1,0)
      call twrite(Irwlc,A(inda),NBASIS,NBASIS,NBASIS,NBASIS,0)
      
      
      if(Print.GT.2)then
      istart=indbb
      iend=istart+NBASIS-1
      write(Iout,99008)CYCLE,(i,A(i),i=istart,iend)
      write(Iout,99002)CYCLE
      call linout(A(inda),NBASIS,1)
      endif
      
      call frmp(NBASIS,nocc,two,A(inda),A(indb),Print)
      
      do 300 i=1,Ntt
      A(i+inda1-1)=A(i+indb-1)
300   continue
      
      
      call twrite(Irwlp,A(inda1),Ntt,1,Ntt,1,0)
      
      
      if(Punden.GT.3)then
      call binwt(A(inda1),2*Ntt,Labdck(1,2))
      call tread(Irwlc,A(indb),Nsq,1,Nsq,1,0)
      call binwt(A(indb),2*Nsq,Labdck(1,1))
      endif
      
      call conclo(NBASIS,ACURCY,FINAC,A(inda1),A(inda2),Inhibe,preduc)
      
      
      
      if(Iflag+ieflag.EQ.0)then
      
      if(Engcon.LE.0)then
      
      if(Key.EQ.0)then
      
      call ilsw(1,5,0)
      goto 400
      endif
      
      elseif(gabs(ETOTAL-eprev).LE.Thresh)then
      call ilsw(1,5,0)
      goto 400
      endif
      
      if(Ioscl.GT.0)then
      eprev=ETOTAL
      elseif(ETOTAL.LT.eprev)then
      
      eprev=ETOTAL
      else
      
      write(Iout,99013)
      call ilsw(1,5,1)
      goto 400
      endif
      
      elseif(Ncyset.LE.0)then
      goto 200
      endif
      if(CYCLE.LT.Maxcyc)goto 200
      
      write(Iout,99004)
      call ilsw(1,5,1)
      
400   ACURCY=FINAC
      return
      
      end
C* :1 * 
      
