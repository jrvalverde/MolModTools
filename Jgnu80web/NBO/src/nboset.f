
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nboset"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nboset.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "nboset.web"
      
      
      
      subroutine nboset(NBOOPT)
      implicit none
      double precision Accthr,Athr,Crtset,Dthr,E2thr,Ethr,half,Prjset,Pt
     &hr,tenth,Thrset
      integer i,Ichoos,Iprint,Ipseud,Ispin,Iw3c,Iwapol,Iwcubf,Iwdetl,Iwd
     &m,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtnao,Iwtnbo,Jcore,Jprint,Ko
     &pt
      integer Lbl,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lf
     &nnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr
     &,MAXATM
      integer MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,NBOOPT,Ndim,Nlew
     &,Nval
      dimension NBOOPT(10)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nblbl/Nlew,Nval,Lbl(10,MAXBAS,4)
      
      data tenth,half/0.1D0,0.5D0/
      
      
      Iwdm=1
      Iw3c=0
      Iwhybs=0
      Iwpnao=0
      Iwtnao=0
      Iwtnab=0
      Iwtnbo=0
      Iwfock=1
      Iprint=12
      Ipseud=0
      Iwdetl=0
      Iwmulp=0
      Ichoos=0
      Kopt=0
      Jcore=0
      Iwcubf=0
      Open=.FALSE.
      Ortho=.FALSE.
      Uhf=.FALSE.
      Auhf=.FALSE.
      Rohf=.FALSE.
      Ci=.FALSE.
      Mcscf=.FALSE.
      Complx=.FALSE.
      do 100 i=1,60
      Jprint(i)=0
100   continue
      
      Lfnao=31
      Lfnpna=32
      Lfnnao=33
      Lfnpnh=34
      Lfnnho=35
      Lfnpnb=36
      Lfnnbo=37
      Lfnpnl=38
      Lfnnlm=39
      Lfnmo=40
      Lfndm=41
      Lfnnab=42
      Lfnppa=43
      Lfnarc=47
      Lfndaf=-48
      Lfndef=49
      
      
      Nval=-1
      
      
      Thrset=-1.9D0
      Prjset=-0.2D0
      Accthr=-tenth
      Crtset=1.999
      E2thr=-half
      Athr=-1.000
      Pthr=-25.000
      Ethr=-0.100
      Dthr=-0.020
      
      
      
      if(NBOOPT(1).EQ.-1)Jprint(1)=1
      
      
      if(NBOOPT(1).EQ.1)Ichoos=-1
      if(NBOOPT(1).EQ.1)Jcore=-1
      
      
      if(NBOOPT(3).NE.0)then
      Jprint(8)=1
      Jprint(46)=1
      endif
      
      
      if(NBOOPT(4).NE.0)Jprint(14)=1
      
      
      Jprint(2)=NBOOPT(10)
      
      return
      end
C* :1 * 
      
