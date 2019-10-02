
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 glpint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "glpint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 34 "glpint.web"
      subroutine glpint(JUMP)
      implicit none
      double precision A,Atmchg,C,C1,C2,C3,C4,Ca,Cb,Cc,Cd,Clp,Core,Eek,E
     &ep,Exx,F100,f20,F20i,F42
      double precision F6i,Filla,Fnumb1,Fnumb2,Fnumb3,Fnumb4,Fnumb5,Fnum
     &b6,Fnumb7,Four,gatan,gfloat,gsqrt,Half,One,Onept5,pi,pi3haf,pt5,ro
     &otpi
      double precision Ss,Ten,Three,Two,twopi,X,Xa,Xb,Xc,Xint,Y,Ya,Yb,Yc
     &,Z,Za,Zb,Zc,Zero,Zero1
      double precision Zlp
      integer i,iaind,Ian,Iatom,Icharg,icntr,Idmp,Idump,Iend,Ifilla,ifpr
     &t,Igbegn,Igdf,Igend,Imj,In,Indix,Indiy,Indiz,Indjx
      integer Indjy,Indjz,Inew,Iop,Iout,iprint,Ipunch,Ipurd,Ipurf,irad1,
     &irad2,irad3,Irange,irwb,irwh,irwlp,ishell,Istart,itemp,Itype
      integer iv,ivloc,j,Jan,jcntr,Jend,Jgbegn,Jgdf,Jgend,Jnew,Jnktyp,Jr
     &ange,jshell,Jstart,Jtype,JUMP,Kfirst,Klast,Lamax,Lbmax
      integer Lbound,LENB,leniwl,lenrwb,lenrwl,Lentq,lim1,Limitd,Lind,ll
     &im,Lmax,lmaxps,Lpmax,Lpskip,lrad,MAXATM,Maxdum,maxi2,maxint,maxp2
      integer maxpri,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Multip,mxcore,N1
     &0ord,N5ord,N6ord,N7ord,Nae,Natoms,Nbasis,Nbe,Ne,Nfroz,nlim,Nlp
      integer Nordr,Nshell,Ntpse,ntt
      parameter(MAXATM=100)
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer scona,sconb
      integer Ubound,Ulpure
      dimension ifprt(8)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/dump/Idmp,Idump
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limit
     &d(11)
      common/type/Itype,Jtype,Jnktyp(10)
      common/memry/Core(50000)
      common/lp2/Nlp(400),Clp(400),Zlp(400),Kfirst(35,5),Klast(35,5),Lma
     &x(35),Lpskip(35),Nfroz(35)
      common/fnumbs/Fnumb1,Fnumb2,Fnumb3,Fnumb4,Fnumb5,Fnumb6,Fnumb7
      common/ia/Lind(164),Ifilla(92)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/io/In,Iout,Ipunch
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/int/Zero1,Xint(12)
      common/new/Inew,Jnew
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/block/Ss(100),Eek(100),Eep(200)
      common/ipure/Ipurd,Ipurf
      common/intcon/F6i,F20i,F100
      common/a/A(45),Filla(129)
      common/inds/Indjx(20),Indjy(20),Indjz(20),Indix(20),Indiy(20),Indi
     &z(20)
      
      common/pseud/Ntpse(7,MAXATM)
      common/centre/Xa,Ya,Za,Xb,Yb,Zb,Xc,Yc,Zc,Iatom
      common/prims/Igbegn,Igend,Jgbegn,Jgend,Igdf,Jgdf
      
      data f20/20.0D00/
      data ifprt/0,1,2,0,0,1,1,2/
      data irwb/6/,irwh/15/,irwlp/12/,lenrwb/1481/,lenrwl/1210/
      
      
      
      
99001 format(39H ********** CORE HAMILTONIAN **********)
99002 format(38H ********** LOCAL POTENTIAL **********)
      
      mxcore=50000
      pi=Four*gatan(One)
      twopi=pi+pi
      rootpi=gsqrt(pi)
      pi3haf=pi*rootpi
      F6i=One/Xint(6)
      F20i=One/f20
      Fnumb1=Zero
      Fnumb2=One/Ten
      Fnumb3=pi
      Fnumb4=twopi+twopi
      Fnumb5=gsqrt(Fnumb4)
      Fnumb6=Fnumb4*Fnumb4
      Fnumb7=rootpi
      
      
      iprint=Iop(33)+1
      iprint=ifprt(iprint)
      call ilsw(2,2,Ipurd)
      call ilsw(2,16,Ipurf)
      Idump=Iop(34)
      if(Idump.GE.2)iprint=2
      
      
      call tread(irwb,Exx,LENB,1,LENB,1,0)
      if(Idump.GE.2)call bdump(2)
      
      ntt=Nbasis*(Nbasis+1)/2
      
      call tread(irwlp,Nlp,leniwl,1,lenrwl,1,0)
      
      
      pt5=gfloat(1)/gfloat(2)
      F100=gfloat(100)
      
      call setord
      call ldata
      call ztab
      
      maxpri=0
      maxint=0
      do 100 ishell=1,Nshell
      maxpri=max0(maxpri,Shelln(ishell))
      scona=Shellc(ishell)
      Itype=Shellt(ishell)
      Lamax=Itype+1
      Irange=Ubound(Lamax)-Lbound(Lamax,scona+1)+1
      maxint=max0(maxint,Irange)
100   continue
      maxp2=maxpri*maxpri
      maxi2=maxint*maxint
      ntt=(Nbasis*(Nbasis+1))/2
      lrad=maxp2*maxi2
      iv=1
      ivloc=iv+ntt
      itemp=ivloc+ntt-1
      irad1=ivloc+lrad
      irad2=irad1+lrad
      irad3=irad2+343*maxp2-1
      if(irad3.GT.mxcore)write(6,*)' Not Enough Scratch Space      in LI
     &NK 306  ',irad3,mxcore
      if(irad3.GT.mxcore)call lnk1e
      itemp=max0(itemp,irad3)
      
      nlim=4
      llim=4
      call iclear(7*MAXATM,Ntpse)
      do 200 i=1,Natoms
      lmaxps=Lmax(i)
      do 150 j=1,lmaxps
      Ntpse(j,i)=Kfirst(i,j+1)
150   continue
      Ntpse(lmaxps+1,i)=Klast(i,lmaxps+1)+1
      Ntpse(lmaxps+2,i)=Kfirst(i,1)
      Ntpse(lmaxps+3,i)=Klast(i,1)+1
200   continue
      call aclear(ntt,Core(iv))
      
      
      
      do 300 ishell=1,Nshell
      Inew=ishell
      Xa=X(ishell)
      Ya=Y(ishell)
      Za=Z(ishell)
      Igbegn=Shella(ishell)
      Igend=Igbegn+Shelln(ishell)-1
      icntr=Jan(ishell)
      Itype=Shellt(ishell)
      Lamax=Itype+1
      scona=Shellc(ishell)
      Iend=Ubound(Lamax)
      Istart=Lbound(Lamax,scona+1)
      Irange=Iend-Istart+1
      Igdf=Shladf(Inew)
      
      
      do 250 jshell=1,ishell
      Jnew=jshell
      Xb=X(jshell)
      Yb=Y(jshell)
      Zb=Z(jshell)
      Jgbegn=Shella(jshell)
      Jgend=Jgbegn+Shelln(jshell)-1
      jcntr=Jan(jshell)
      Jtype=Shellt(jshell)
      Lbmax=Jtype+1
      sconb=Shellc(jshell)
      Jstart=Lbound(Lbmax,sconb+1)
      Jend=Ubound(Lbmax)
      Jrange=Jend-Jstart+1
      Jgdf=Shladf(Jnew)
      
      Lpmax=Lamax+Lbmax-1
      Lentq=Irange*Jrange
      lim1=Lentq+100
      Imj=iabs(ishell-jshell)
      
      do 220 Iatom=1,Natoms
      if(Lpskip(Iatom).NE.1)then
      
      do 205 i=1,lim1
      Eep(i)=Zero
205   continue
      iaind=3*(Iatom-1)
      Xc=C(1+iaind)
      Yc=C(2+iaind)
      Zc=C(3+iaind)
      call cntrlp(Eep,Core(ivloc),Core(irad1),Core(irad2),maxi2,maxp2)
      
      call fillp(Eep,Core(iv))
      endif
220   continue
250   continue
300   continue
      
      if(iprint.GT.0)then
      write(Iout,99002)
      call linout(Core(iv),Nbasis,0)
      endif
      call tread(irwh,Core(ivloc),ntt,1,ntt,1,0)
      do 400 i=1,ntt+1
      Core(iv+i-1)=Core(iv+i-1)+Core(ivloc+i-1)
400   continue
      call twrite(irwh,Core(iv),ntt,1,ntt,1,0)
      if(iprint.GT.0)then
      write(Iout,99001)
      call linout(Core(iv),Nbasis,0)
      endif
      JUMP=0
      return
      
      
      end
C* :1 * 
      
