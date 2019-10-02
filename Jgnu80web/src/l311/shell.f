
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 shell"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "shell.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 119 "shell.web"
      subroutine shell(D,F,IOP,JUMP)
      implicit none
      double precision Acx,Acy,Acy2,Acz,Ag,Ap,App,Aqx,Aqz,Auxvar,Ax,Ay,A
     &z,Bg,Bp,Bpp,Bx,By,Bz,C
      double precision C1,C11,C12,C13,C2,C21,C22,C23,C3,C31,C32,C33,Cg,C
     &max,Cmaxa,Cmaxb,Cmaxc,Cmaxd,Conp,Const
      double precision Cosg,Cosp,Cpa,Cpb,Cpc,Cpd,Cq,Csa,Csb,Csc,Csd,Cx,C
     &y,Cz,Dbuf2e,Dg,Dp00,Dp00p,Dp01,Dp01p
      double precision Dp10,Dp10p,Dp11,Dp11p,Dq,Dq00,Dq01,Dq10,Dq11,Dx,D
     &y,Dz,Eab,Ecd,Ep,Error1,Error2,Exx,four,g1
      double precision g11,g12,g13,g2,g21,g22,g23,g3,g31,g32,g33,Ga,Gab,
     &Gb,Gc,Gcd,Gd,gint,Gout,Gp
      double precision Gstore,one,P11,P12,P13,P21,P22,P23,P31,P32,P33,Pa
     &,Pb,Pc,Pd,pi,Pidiv4,Pito52,Pq1,Pq2
      double precision Pq3,pt25,Px,Py,Pz,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32
     &,Q33,Qperp,Qperp2,Qx,Qy,Qz,Rab
      double precision Rabsq,Rcd,Rcdsq,Rpq,Rpqsq,Sa,Sb,Sc,Sd,Sing,Sinp,s
     &ixty,symfac,tenm12,Ttt,two,twopt5,v,Var1,Var2
      double precision X,xqq,xxtest,Y,Z,zero
      integer i,iacc,Ibf,Ibuf2e,Ic,idcout,iitype,ijkl,ijkld,ikjld,iljkd,
     &In,indv,Inew,Iout,Ipunch,iret,Irwb,Irwfm,Irwibf
      integer Irwicb,isdupe,iset,ish,ishdup,Ishell,isite1,isite2,isite3,
     &Isml,Ismlp,Ismlq,Ismode,istart,Isym2e,isymm,isytmp,Itype,iv11,iv12
      integer iv13,iv14,iv21,iv22,iv23,iv24,iv31,iv32,iv33,iv34,Ivec,ive
     &ctr,j,Jan,jndv,Jnew,Jnktyp,jsh,Jshell,jstart
      integer jtdat,Jtype,JUMP,Jvec,k,kndv,Knew,ksh,Kshell,kstart,Kvec,l
     &,La,lambda,Lat,Lb,Lbt,Lc,Lct,Ld
      integer Ldt,LENB,Lenibf,lenneq,lndv,Lnew,loi,lsh,Lshell,lstart,Lve
     &c,Mab,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Mcd,Mode,mu
      integer neq,neqshl,nf,nfa,nfb,nfc,nfd,Nga,Ngangb,Ngb,Ngc,Ngd,ngdat
     &,ngout,ngsave,nset,Nshell,nsymop,nu,numd
      integer IOP(*)
      double precision D(*),F(*)
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      integer sigma
      logical dbuf,reject
      dimension ivectr(4),ngsave(3),loi(4),nf(2),isytmp(2)
      dimension ngdat(16),jtdat(16)
      dimension Ibuf2e(1)
      dimension gint(3,3)
      dimension isdupe(3)
      common/cconst/Const,Conp(100)
      common/shlnos/Ishell,Jshell,Kshell,Lshell,Inew,Jnew,Knew,Lnew
      common/shlinf/Nga,La,Ag(10),Csa(10),Cpa(10),Ngb,Lb,Bg(10),Csb(10),
     &Cpb(10),Ngc,Lc,Cg(10),Csc(10),Cpc(10),Ngd,Ld,Dg(10),Csd(10),Cpd(10
     &)
      common/lt/Lat,Lbt,Lct,Ldt
      common/misc/Mab,Mcd,Ngangb
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/pqgeom/Ap,Bp,Cq,Dq,Px,Py,Pz,Qx,Qy,Qz,Rpq,Rpqsq,Pq1,Pq2,Pq3,
     &C11,C12,C13,C21,C22,C23,C31,C32,C33
      common/ginf/Ga,Gb,Gc,Gd,Sa,Sb,Sc,Sd,Pa,Pb,Pc,Pd,Gab,Gcd
      common/type/Itype,Jtype,Jnktyp(10)
      common/gout/Gout(256)
      common/pgeom/Gp(100),Ep(100),Dp00p(100),Dp01p(100),Dp10p(100),Dp11
     &p(100),App(100),Bpp(100)
      common/eabecd/Eab,Ecd
      common/dpq/Dp00,Dp01,Dp10,Dp11,Dq00,Dq01,Dq10,Dq11
      common/cos/C
      common/phi/Cosp,Sinp
      common/qgeom/Acx,Acy,Acz,Acy2,Cosg,Sing,Aqx,Aqz,Qperp,Qperp2
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension neqshl(MAXSHL,8)
      common/auxvar/Auxvar,Var1,Var2
      common/ctable/Ttt(6000)
      common/io/In,Iout,Ipunch
      common/picon/Pito52,Pidiv4
      common/irw311/Irwb,Irwfm,Irwicb,Irwibf,Lenibf
      common/gstore/Gstore(256,3)
      common/idat/Ivec(16,3),Jvec(16,3),Kvec(16,3),Lvec(16,3)
      common/shdups/Ic(256,5)
      common/ibf/Ibf(30)
      common/buf2e/Dbuf2e(4760)
      equivalence(g11,gint(1,1)),(g12,gint(1,2)),(g13,gint(1,3))
      equivalence(g21,gint(2,1)),(g22,gint(2,2)),(g23,gint(2,3))
      equivalence(g31,gint(3,1)),(g32,gint(3,2)),(g33,gint(3,3))
      equivalence(Ismode,Ibf(1)),(Mode,Ibf(2))
      equivalence(ijkld,isdupe(1)),(iljkd,isdupe(2)),(ikjld,isdupe(3))
      equivalence(Ibuf2e(1),Dbuf2e(1))
      equivalence(Isym2e,Ibf(30))
      data zero/0.0D0/,one/1.0D0/
      data sixty/60.0D0/
      data four/4.0D0/,two/2.0D0/,twopt5/2.5D0/
      data tenm12/1.0D-12/
      data pt25/0.25D0/,nf/1,4/
      data jtdat/1,2,2,3,2,4,4,5,2,4,4,5,3,5,5,6/
      data ngdat/1,4,4,16,4,64,64,64,4,64,64,64,16,64,64,256/
      data dbuf/.TRUE./,idcout/11/
      data isymm/551/,neq/565/
      iacc=IOP(28)
      if(iacc.EQ.0)then
      Pidiv4=datan(one)
      pi=four*Pidiv4
      Pito52=two*pi**twopt5
      call tread(Irwb,Exx(1),LENB,1,LENB,1,0)
      if(IOP(34).GE.2)call bdump(2)
      call tread(Irwfm,Ttt(1),6000,1,6000,1,0)
      call out2e(-1,mu,nu,lambda,sigma,gint,dbuf,Ibuf2e,Dbuf2e,iret,idco
     &ut,IOP,D,F)
      if(iret.EQ.0)then
      call filmax
      call elim
      
      if(Isym2e.EQ.1)then
      call tread(isymm,isytmp,1,1,1,1,0)
      nsymop=isytmp(1)
      lenneq=4*MAXSHL
      call tread(neq,neqshl(1,1),lenneq,1,lenneq,1,0)
      endif
      
      
      
      do 60 ish=1,Nshell
      do 50 jsh=1,ish
      do 45 ksh=1,jsh
      do 40 lsh=1,ksh
      Ishell=ish
      Jshell=jsh
      Kshell=ksh
      Lshell=lsh
      
      
      if(ish.EQ.jsh.AND.ksh.NE.lsh)then
      
      Ishell=lsh
      Jshell=ksh
      Kshell=jsh
      Lshell=ish
      endif
      ivectr(1)=Ishell
      ivectr(2)=Jshell
      ivectr(3)=Kshell
      ivectr(4)=Lshell
      
      call isymgo(Ishell,Jshell,Kshell,Lshell,nsymop,neqshl,Isym2e,rejec
     &t,symfac)
      if(.NOT.(reject))then
      
      Lat=Shellt(Ishell)
      Lbt=Shellt(Jshell)
      Lct=Shellt(Kshell)
      Ldt=Shellt(Lshell)
      numd=Lat/2+Lbt/2+Lct/2+Ldt/2
      if(numd.EQ.0)then
      Itype=8*Lat+4*Lbt+2*Lct+Ldt+1
      
      nset=3
      do 2 iset=1,nset
      isdupe(iset)=0
2     continue
      
      if(Ishell.EQ.Jshell.OR.Jshell.EQ.Kshell.OR.Kshell.EQ.Lshell)nset=2
      if(Ishell.EQ.Kshell.OR.Jshell.EQ.Lshell)nset=1
      
      
      do 12 iset=1,nset
      
      
      indv=Ivec(Itype,iset)
      jndv=Jvec(Itype,iset)
      kndv=Kvec(Itype,iset)
      lndv=Lvec(Itype,iset)
      
      Inew=ivectr(indv)
      Jnew=ivectr(jndv)
      Knew=ivectr(kndv)
      Lnew=ivectr(lndv)
      
      isdupe(iset)=ishdup(Inew,Jnew,Knew,Lnew)
      
      La=Shellt(Inew)
      Lb=Shellt(Jnew)
      Lc=Shellt(Knew)
      Ld=Shellt(Lnew)
      iitype=8*La+4*Lb+2*Lc+Ld+1
      Jtype=jtdat(iitype)
      ngout=ngdat(iitype)
      
      do 4 i=1,ngout
      Gout(i)=zero
4     continue
      call sinfo
      call sgeom
      call pinf(symfac)
      if(Jtype.LE.1)then
      call sp0000
      else
      do 8 k=1,Ngc
      Gc=Cg(k)
      do 6 l=1,Ngd
      Gd=Dg(l)
      Gcd=Gc+Gd
      Ecd=one/Gcd
      Cq=Gd*Ecd*Rcd
      Dq=Cq-Rcd
      xqq=Cq*Dq*Gcd
      if(xqq+sixty.LT.0)then
      v=zero
      else
      
      v=dexp(xqq)*Ecd
      endif
      xxtest=Cmaxc(k)*Cmaxd(l)*v
      if(xxtest.LE.Error1)then
      
      if(xxtest.LE.Error2)goto 6
      Ismlq=1
      else
      Ismlq=0
      endif
      Sc=Csc(k)
      Sd=Csd(l)
      Pc=Cpc(k)
      Pd=Cpd(l)
      Dq00=Sc*Sd*v
      Dq01=Sc*Pd*v
      Dq10=Pc*Sd*v
      Dq11=Pc*Pd*v
      Aqx=Acx+Sing*Cq
      Aqz=Acz+Cosg*Cq
      Qperp2=Aqx*Aqx+Acy2
      Qperp=dsqrt(Qperp2)
      if(Qperp.LE.tenm12)then
      
      Cosp=one
      Sinp=zero
      else
      Cosp=-Aqx/Qperp
      Sinp=-Acy/Qperp
      endif
      if(Jtype.EQ.4)then
      call sp0101
      
      elseif(Jtype.EQ.5)then
      call sp0111
      
      elseif(Jtype.LT.3)then
      call sp0001
      goto 6
      elseif(Jtype.EQ.3)then
      
      call sp0011
      else
      
      call sp1111
      endif
      call rot2
      if(Jtype.EQ.1.OR.Jtype.EQ.2)then
      elseif(Jtype.EQ.4)then
      
      call tq0101
      elseif(Jtype.EQ.5)then
      
      call tq0111
      elseif(Jtype.EQ.6)then
      
      call tq1111
      else
      
      call tq0011
      endif
6     continue
8     continue
      if(Jtype.EQ.1)then
      elseif(Jtype.EQ.3)then
      
      call r30011
      elseif(Jtype.EQ.4)then
      
      call r30101
      elseif(Jtype.EQ.5)then
      
      call r30111
      elseif(Jtype.EQ.6)then
      
      call r31111
      else
      
      call r30001
      endif
      endif
      
      do 10 i=1,ngout
      Gstore(i,iset)=Gout(i)
10    continue
      ngsave(iset)=ngout
12    continue
      
      
      
      
      
      ijkl=ijkld+iljkd+ikjld
      
      nfa=nf(Lat+1)
      nfb=nf(Lbt+1)
      nfc=nf(Lct+1)
      nfd=nf(Ldt+1)
      
      iv11=Ivec(Itype,1)
      iv12=Jvec(Itype,1)
      iv13=Kvec(Itype,1)
      iv14=Lvec(Itype,1)
      iv21=Ivec(Itype,2)
      iv22=Jvec(Itype,2)
      iv23=Kvec(Itype,2)
      iv24=Lvec(Itype,2)
      iv31=Ivec(Itype,3)
      iv32=Jvec(Itype,3)
      iv33=Kvec(Itype,3)
      iv34=Lvec(Itype,3)
      
      istart=Aos(Ishell)-1
      jstart=Aos(Jshell)-1
      kstart=Aos(Kshell)-1
      lstart=Aos(Lshell)-1
      
      if(Ismode.LE.0)then
      
      do 22 i=1,nfa
      loi(1)=i-1
      mu=i+istart
      do 20 j=1,nfb
      loi(2)=j-1
      nu=j+jstart
      do 18 k=1,nfc
      loi(3)=k-1
      lambda=k+kstart
      do 16 l=1,nfd
      loi(4)=l-1
      sigma=l+lstart
      
      isite1=64*loi(iv11)+16*loi(iv12)+4*loi(iv13)+loi(iv14)+1
      isite2=64*loi(iv21)+16*loi(iv22)+4*loi(iv23)+loi(iv24)+1
      isite3=64*loi(iv31)+16*loi(iv32)+4*loi(iv33)+loi(iv34)+1
      
      g11=Gstore(isite1,1)
      g12=Gstore(isite2,2)
      g13=Gstore(isite3,3)
      
      if(ijkl.GT.0)then
      if(nset.EQ.1)goto 14
      if(nset.NE.2)then
      
      if(ikjld.GT.0)then
      if(Ic(isite3,ikjld).EQ.0)g13=zero
      endif
      endif
      if(iljkd.GT.0)then
      if(Ic(isite2,iljkd).EQ.0)g12=zero
      endif
14    if(ijkld.GT.0)then
      if(Ic(isite1,ijkld).EQ.0)g11=zero
      endif
      endif
      
      call out2e(nset,mu,nu,lambda,sigma,gint,dbuf,Ibuf2e,Dbuf2e,iret,id
     &cout,IOP,D,F)
16    continue
18    continue
20    continue
22    continue
      else
      
      
      if(nset.EQ.2)then
      
      if(Jshell.NE.Kshell)then
      
      ngout=ngsave(2)
      do 24 i=1,ngout
      Gstore(i,3)=Gstore(i,2)
24    continue
      else
      
      ngout=ngsave(1)
      do 26 i=1,ngout
      Gstore(i,3)=Gstore(i,1)
26    continue
      endif
      elseif(nset.NE.3)then
      
      ngout=ngsave(1)
      do 28 i=1,ngout
      Gstore(i,2)=Gstore(i,1)
      Gstore(i,3)=Gstore(i,1)
28    continue
      endif
      
      do 38 i=1,nfa
      loi(1)=i-1
      mu=i+istart
      do 36 j=1,nfb
      loi(2)=j-1
      nu=j+jstart
      do 34 k=1,nfc
      loi(3)=k-1
      lambda=k+kstart
      do 32 l=1,nfd
      loi(4)=l-1
      sigma=l+lstart
      
      isite1=64*loi(iv11)+16*loi(iv12)+4*loi(iv13)+loi(iv14)+1
      isite2=64*loi(iv21)+16*loi(iv22)+4*loi(iv23)+loi(iv24)+1
      isite3=64*loi(iv31)+16*loi(iv32)+4*loi(iv33)+loi(iv34)+1
      
      g1=Gstore(isite1,1)
      g2=Gstore(isite2,2)
      g3=Gstore(isite3,3)
      
      g31=g3-g2
      g21=g2+g3
      g11=g1-pt25*g21
      g32=g1-g3
      g22=g1+g3
      g12=g2-pt25*g22
      g33=g1-g2
      g23=g1+g2
      g13=g3-pt25*g23
      
      if(ijkl.NE.0)then
      if(nset.EQ.1)goto 30
      if(nset.NE.2)then
      
      if(ikjld.NE.0)then
      if(Ic(isite3,ikjld).EQ.0)then
      g13=zero
      g23=zero
      g33=zero
      endif
      endif
      endif
      if(iljkd.NE.0)then
      if(Ic(isite2,iljkd).EQ.0)then
      g12=zero
      g22=zero
      g32=zero
      endif
      endif
30    if(ijkld.NE.0)then
      if(Ic(isite1,ijkld).EQ.0)then
      g11=zero
      g21=zero
      g31=zero
      endif
      endif
      endif
      
      call out2e(nset,mu,nu,lambda,sigma,gint,dbuf,Ibuf2e,Dbuf2e,iret,id
     &cout,IOP,D,F)
32    continue
34    continue
36    continue
38    continue
      endif
      endif
      endif
40    continue
45    continue
50    continue
60    continue
      call out2e(0,mu,nu,lambda,sigma,gint,dbuf,Ibuf2e,Dbuf2e,iret,idcou
     &t,IOP,D,F)
      endif
      endif
      
      JUMP=0
      
99001 format(3(10x,4I3,d20.13))
99002 format(3(10x,12x,d20.13))
      
      return
      
      end
C* :1 * 
      
