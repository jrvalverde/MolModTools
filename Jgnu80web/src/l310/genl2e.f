
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 genl2e"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "genl2e.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 65 "genl2e.web"
      subroutine genl2e(D,F,IOP,JUMP)
      implicit none
      double precision as,Atmchg,Binom,bs,C,C1,C2,C3,C4,Ca,Cb,Cc,Cd,cs,C
     &sum,ctemp,Cx,Cy,Cz,Dbuf2e
      double precision ds,Dxyz,ep,epeq,epi,eppeqi,eq,eqi,exparg,Exx,F15,
     &Fact,Fipx,Fipy,Fipz,Fiqx,Fiqy,Fiqz,Five,Fm
      double precision Four,fourp,fourq,gatan,gexp,gfloat,gsqrt,H4p,H4q,
     &Hepsi,Hpax,Hpay,Hpaz,Hpbx,Hpby,Hpbz,Hpqx,Hpqy,Hpqz,Hqcx
      double precision Hqcy,Hqcz,Hqdx,Hqdy,Hqdz,One,pax,pay,paz,pbx,pby,
     &pbz,pconst,pi,pqexp,pqx,pqy,pqz,premul,Pt5
      double precision px,py,pz,qcx,qcy,qcz,qdx,qdy,qdz,qx,qy,qz,R1,R2,R
     &3,R3ov2,R4,ra,Rabsq,rb
      double precision rc,Rcdsq,rd,rho,rho4i,Root15,Root3,Root5,rpqsq,Th
     &ree,Tq,Two,twoint,X,Xa,Xb,Xc,Xd,Y,Ya
      double precision Yb,Yc,Yd,Z,Z1,Z2,Z3,Za,Zb,Zc,Zd,Zero
      integer i,Ian,Ibuf2e,Icharg,ick,idcout,Iend,Ifpure,Igauss,Igbeg,Ig
     &df,Igend,Imj,Imk,Imkjml,In,Indc,Indf,indlpq,indx
      integer indy,indz,Inew,intc,iop20p,Iout,Ipunch,Ipurd,Ipure,Ipurf,I
     &range,iret,irwb,Ishell,ist,Istart,Itype,ix,iy,iz
      integer j,Jan,Jend,Jgauss,Jgbeg,Jgdf,Jgend,Jml,Jnew,Jpure,Jrange,J
     &shell,jst,Jstart,Jtype,JUMP,k,Kend,Kgauss,Kgbeg
      integer Kgdf,Kgend,Kml,Knew,Kpure,Krange,Kshell,kst,Kstart,Ktype,l
     &,la,Lamax,lambda,lb,Lbmax,Lbound,lc,Lcmax,ld
      integer Ldmax,LENB,Lend,Lentq,Lgauss,Lgbeg,Lgdf,Lgend,Lhold,limx,l
     &imxyz,limy,limz,Lind,Lnew,Lpmax,lpq,Lpqmax,Lpure,Lqmax
      integer Lrange,lsave,Lshell,lst,Lstart,Ltype,ma,maxl,MAXPRM,MAXS21
     &,MAXSH1,MAXSHL,Maxtyp,mb,mc,md,Mhold,mu,Multip,N10ord
      integer N5ord,N6ord,N7ord,na,Nae,Natoms,nb,Nbasis,Nbe,nc,nd,ndc,Ne
     &,nga,ngb,ngc,ngd,Nhold,Nordr,Nshell
      integer nu,Numdf
      integer IOP(*)
      double precision D(*),F(*)
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer Sconap,Sconbp,Sconcp,Scondp
      integer Ubound,Ulpure
      integer sigma
      logical dbuf
      dimension lsave(4)
      dimension Ibuf2e(1)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/io/In,Iout,Ipunch
      common/twbuf2/Tq(10000),Dbuf2e(4760)
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      common/stypes/Itype,Jtype,Ktype,Ltype
      common/scons/Sconap,Sconbp,Sconcp,Scondp
      common/newshl/Inew,Jnew,Knew,Lnew
      common/shells/Ishell,Jshell,Kshell,Lshell
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/coord/Xa,Ya,Za,Xb,Yb,Zb,Rabsq,Xc,Yc,Zc,Xd,Yd,Zd,Rcdsq
      common/gcloop/Igauss,Igbeg,Igend,Igdf,Jgauss,Jgbeg,Jgend,Jgdf,Kgau
     &ss,Kgbeg,Kgend,Kgdf,Lgauss,Lgbeg,Lgend,Lgdf
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/contr/Ca(20),Cb(20),Cc(20),Cd(20)
      common/fminf/Dxyz,Fm(15)
      common/hold/Lhold(20),Mhold(20),Nhold(20)
      common/fipq/Fipx(64),Fipy(64),Fipz(64),Fiqx(64),Fiqy(64),Fiqz(64)
      common/h310/Hpax(4),Hpay(4),Hpaz(4),Hpbx(4),Hpby(4),Hpbz(4),Hqcx(4
     &),Hqcy(4),Hqcz(4),Hqdx(4),Hqdy(4),Hqdz(4),Hpqx(13),Hpqy(13),Hpqz(1
     &3),H4p(7),H4q(7),Hepsi(13)
      common/indc/Indc(256)
      common/c310/Csum(15),Cx(1792),Cy(1792),Cz(1792)
      common/indf/Indf(16)
      common/binom/Binom(28)
      common/factor/Fact(15)
      common/lind/Lind(20)
      common/ipure/Ipurd,Ipurf
      common/jpure/Ifpure(4,4),Ipure,Jpure,Kpure,Lpure
      common/con310/Zero,One,Two,Three,Four,Five,F15
      equivalence(Dbuf2e(1),Ibuf2e(1))
      data irwb/506/
      data dbuf/.TRUE./,idcout/10/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      call drum
      pi=Four*gatan(One)
      Root3=gsqrt(Three)
      Root5=gsqrt(Five)
      Root15=gsqrt(F15)
      R3ov2=Pt5*Root3
      R3=Pt5*Root3
      pconst=Two*pi*pi*gsqrt(pi)
      call ilsw(2,16,Ipurf)
      call ilsw(2,2,Ipurd)
      iop20p=2*Ipurf+Ipurd+1
      lsave(1)=Ubound(1)
      lsave(2)=Ubound(2)
      lsave(3)=Ubound(3)
      lsave(4)=Ubound(4)
      if(Ipurd.EQ.0)lsave(3)=Ulpure(3)
      if(Ipurf.EQ.0)lsave(4)=Ulpure(4)
      R1=Pt5*gsqrt(Five/Two)
      R2=Three/(Root5+Root5)
      R4=Pt5*gsqrt(Three/Two)
      Z1=Four/Root5
      Z2=One/Root5
      Z3=Three/Root5
      call setord
      call tread(irwb,Exx(1),LENB,1,LENB,1,0)
      call out2e(-1,mu,nu,lambda,sigma,Tq,dbuf,Ibuf2e,Dbuf2e,iret,idcout
     &,IOP,D,F)
      call fmtset(0,0,0)
      call getind(4,4,4,4)
      Hpax(1)=One
      Hpay(1)=One
      Hpaz(1)=One
      Hpbx(1)=One
      Hpby(1)=One
      Hpbz(1)=One
      Hqcx(1)=One
      Hqcy(1)=One
      Hqcz(1)=One
      Hqdx(1)=One
      Hqdy(1)=One
      Hqdz(1)=One
      Hpqx(1)=One
      Hpqy(1)=One
      Hpqz(1)=One
      H4p(1)=One
      H4q(1)=One
      Hepsi(1)=One
      
      Fact(1)=One
      Fact(2)=One
      do 100 i=3,15
      Fact(i)=gfloat(i-1)*Fact(i-1)
100   continue
      
      do 200 i=1,20
      Lind(i)=(i*(i-1))/2
200   continue
      
      
      
      do 400 Ishell=1,Nshell
      Inew=Ishell
      Xa=X(Ishell)
      Ya=Y(Ishell)
      Za=Z(Ishell)
      Igbeg=Shella(Ishell)
      nga=Shelln(Ishell)
      Itype=Shellt(Ishell)
      Igdf=Shladf(Ishell)
      Sconap=Shellc(Ishell)+1
      Igend=Igbeg+nga-1
      Lamax=Itype+1
      Istart=Lbound(Lamax,Sconap)
      Iend=Ubound(Lamax)
      Irange=Iend-Istart+1
      Ipure=Ifpure(Lamax,iop20p)
      
      do 300 Jshell=1,Ishell
      Jnew=Jshell
      Xb=X(Jshell)
      Yb=Y(Jshell)
      Zb=Z(Jshell)
      Jgbeg=Shella(Jshell)
      ngb=Shelln(Jshell)
      Jtype=Shellt(Jshell)
      Jgdf=Shladf(Jshell)
      Sconbp=Shellc(Jshell)+1
      Jgend=Jgbeg+ngb-1
      Lbmax=Jtype+1
      Jstart=Lbound(Lbmax,Sconbp)
      Jend=Ubound(Lbmax)
      Jrange=Jend-Jstart+1
      Jpure=Ifpure(Lbmax,iop20p)
      Lpmax=Lamax+Lbmax-1
      Imj=iabs(Ishell-Jshell)
      Rabsq=(Xb-Xa)*(Xb-Xa)+(Yb-Ya)*(Yb-Ya)+(Zb-Za)*(Zb-Za)
      
      do 280 Kshell=1,Ishell
      Knew=Kshell
      Xc=X(Kshell)
      Yc=Y(Kshell)
      Zc=Z(Kshell)
      Kgbeg=Shella(Kshell)
      ngc=Shelln(Kshell)
      Ktype=Shellt(Kshell)
      Kgdf=Shladf(Kshell)
      Sconcp=Shellc(Kshell)+1
      Kgend=Kgbeg+ngc-1
      Lcmax=Ktype+1
      Kstart=Lbound(Lcmax,Sconcp)
      Kend=Ubound(Lcmax)
      Krange=Kend-Kstart+1
      Kpure=Ifpure(Lcmax,iop20p)
      Imk=iabs(Ishell-Kshell)
      maxl=Kshell
      if(Imk.EQ.0)maxl=Jshell
      
      do 260 Lshell=1,maxl
      Lnew=Lshell
      Xd=X(Lshell)
      Yd=Y(Lshell)
      Zd=Z(Lshell)
      Lgbeg=Shella(Lshell)
      ngd=Shelln(Lshell)
      Ltype=Shellt(Lshell)
      Lgdf=Shladf(Lshell)
      Scondp=Shellc(Lshell)+1
      Lgend=Lgbeg+ngd-1
      Ldmax=Ltype+1
      Lstart=Lbound(Ldmax,Scondp)
      Lend=Ubound(Ldmax)
      Lrange=Lend-Lstart+1
      Lpure=Ifpure(Ldmax,iop20p)
      Numdf=Itype/2+Jtype/2+Ktype/2+Ltype/2
      Lqmax=Lcmax+Ldmax-1
      Lpqmax=Lpmax+Lqmax-1
      Jml=iabs(Jshell-Lshell)
      Kml=iabs(Kshell-Lshell)
      Imkjml=Imk+Jml
      Lentq=Irange*Jrange*Krange*Lrange
      
      Rcdsq=(Xd-Xc)*(Xd-Xc)+(Yd-Yc)*(Yd-Yc)+(Zd-Zc)*(Zd-Zc)
      Iend=Ubound(Lamax)
      Jend=Ubound(Lbmax)
      Kend=Ubound(Lcmax)
      Lend=Ubound(Ldmax)
      ndc=nga*ngb*ngc*ngd
      do 205 i=1,Lentq
      Tq(i)=Zero
205   continue
      
      intc=0
      do 245 Igauss=Igbeg,Igend
      as=Exx(Igauss)
      call fillc(Itype,Igbeg,Igauss,Igdf,Ca)
      
      do 242 Jgauss=Jgbeg,Jgend
      bs=Exx(Jgauss)
      call fillc(Jtype,Jgbeg,Jgauss,Jgdf,Cb)
      
      ep=as+bs
      fourp=Four*ep
      epi=One/ep
      px=(as*Xa+bs*Xb)*epi
      py=(as*Ya+bs*Yb)*epi
      pz=(as*Za+bs*Zb)*epi
      exparg=as*bs*Rabsq*epi
      pax=px-Xa
      pbx=px-Xb
      pay=py-Ya
      pby=py-Yb
      paz=pz-Za
      pbz=pz-Zb
      
      if(Lamax.GE.2)then
      do 206 i=2,Lamax
      Hpax(i)=Hpax(i-1)*pax
      Hpay(i)=Hpay(i-1)*pay
      Hpaz(i)=Hpaz(i-1)*paz
206   continue
      endif
      if(Lbmax.GE.2)then
      do 208 i=2,Lbmax
      Hpbx(i)=Hpbx(i-1)*pbx
      Hpby(i)=Hpby(i-1)*pby
      Hpbz(i)=Hpbz(i-1)*pbz
208   continue
      endif
      
      if(Lpmax.GE.2)then
      do 210 i=2,Lpmax
      H4p(i)=H4p(i-1)*fourp
210   continue
      endif
      
      call getf(Lamax,Lbmax,Hpax,Hpbx,Fipx)
      call getf(Lamax,Lbmax,Hpay,Hpby,Fipy)
      call getf(Lamax,Lbmax,Hpaz,Hpbz,Fipz)
      
      do 240 Kgauss=Kgbeg,Kgend
      cs=Exx(Kgauss)
      call fillc(Ktype,Kgbeg,Kgauss,Kgdf,Cc)
      
      do 238 Lgauss=Lgbeg,Lgend
      ds=Exx(Lgauss)
      call fillc(Ltype,Lgbeg,Lgauss,Lgdf,Cd)
      
      
      eq=cs+ds
      fourq=Four*eq
      eqi=One/eq
      qx=(cs*Xc+ds*Xd)*eqi
      qy=(cs*Yc+ds*Yd)*eqi
      qz=(cs*Zc+ds*Zd)*eqi
      pqexp=gexp(-exparg-cs*ds*Rcdsq*eqi)
      epeq=ep*eq
      eppeqi=One/(ep+eq)
      rho=epeq*eppeqi
      rho4i=One/(Four*rho)
      premul=pconst*gsqrt(eppeqi)*pqexp/epeq
      qcx=qx-Xc
      qdx=qx-Xd
      qcy=qy-Yc
      qdy=qy-Yd
      qcz=qz-Zc
      qdz=qz-Zd
      pqx=qx-px
      pqy=qy-py
      pqz=qz-pz
      
      if(Lcmax.GE.2)then
      do 212 i=2,Lcmax
      Hqcx(i)=Hqcx(i-1)*qcx
      Hqcy(i)=Hqcy(i-1)*qcy
      Hqcz(i)=Hqcz(i-1)*qcz
212   continue
      endif
      if(Ldmax.GE.2)then
      do 214 i=2,Ldmax
      Hqdx(i)=Hqdx(i-1)*qdx
      Hqdy(i)=Hqdy(i-1)*qdy
      Hqdz(i)=Hqdz(i-1)*qdz
214   continue
      endif
      
      if(Lqmax.GE.2)then
      do 216 i=2,Lqmax
      H4q(i)=H4q(i-1)*fourq
216   continue
      endif
      
      if(Lpqmax.GE.2)then
      do 218 i=2,Lpqmax
      Hpqx(i)=Hpqx(i-1)*pqx
      Hpqy(i)=Hpqy(i-1)*pqy
      Hpqz(i)=Hpqz(i-1)*pqz
      Hepsi(i)=Hepsi(i-1)*rho4i
218   continue
      endif
      
      call getf(Lcmax,Ldmax,Hqcx,Hqdx,Fiqx)
      call getf(Lcmax,Ldmax,Hqcy,Hqdy,Fiqy)
      call getf(Lcmax,Ldmax,Hqcz,Hqdz,Fiqz)
      
      call getc(Fipx,Fiqx,Hpqx,Cx)
      call getc(Fipy,Fiqy,Hpqy,Cy)
      call getc(Fipz,Fiqz,Hpqz,Cz)
      
      rpqsq=pqx*pqx+pqy*pqy+pqz*pqz
      Dxyz=rho*rpqsq
      call fmtgen(Fm,Dxyz,Lpqmax,ick)
      
      
      
      intc=0
      do 236 i=Istart,Iend
      la=Lhold(i)
      ma=Mhold(i)
      na=Nhold(i)
      ra=Ca(i)
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      
      do 234 j=Jstart,Jend
      lb=Lhold(j)
      mb=Mhold(j)
      nb=Nhold(j)
      rb=Cb(j)*ra
      
      do 232 k=Kstart,Kend
      lc=Lhold(k)
      mc=Mhold(k)
      nc=Nhold(k)
      rc=Cc(k)*rb
      Lend=Ubound(Ldmax)
      if(Kml.EQ.0)Lend=k
      if(Imkjml.EQ.0.AND.iabs(i-k).EQ.0)Lend=j
      
      do 230 l=Lstart,Lend
      ld=Lhold(l)
      md=Mhold(l)
      nd=Nhold(l)
      rd=Cd(l)*rc
      
      limx=la+lb+lc+ld+1
      limy=ma+mb+mc+md+1
      limz=na+nb+nc+nd+1
      limxyz=limx+limy+limz-2
      indx=Indc(64*la+16*lb+4*lc+ld+1)-1
      indy=Indc(64*ma+16*mb+4*mc+md+1)-1
      indz=Indc(64*na+16*nb+4*nc+nd+1)-1
      
      do 220 lpq=1,limxyz
      Csum(lpq)=Zero
220   continue
      
      do 226 ix=1,limx
      do 224 iy=1,limy
      ctemp=Cx(indx+ix)*Cy(indy+iy)
      do 222 iz=1,limz
      indlpq=ix+iy+iz-2
      Csum(indlpq)=Csum(indlpq)+ctemp*Cz(indz+iz)
222   continue
224   continue
226   continue
      
      twoint=Zero
      do 228 lpq=1,limxyz
      twoint=twoint+Csum(lpq)*Fm(lpq)
228   continue
      
      intc=intc+1
      Tq(intc)=Tq(intc)+rd*premul*twoint
230   continue
232   continue
234   continue
236   continue
      
238   continue
240   continue
242   continue
245   continue
      call gpurdf(intc)
      
      ist=Aos(Ishell)-1
      jst=Aos(Jshell)-1
      kst=Aos(Kshell)-1
      lst=Aos(Lshell)-1
      intc=0
      do 255 i=Istart,Iend
      mu=ist+Nordr(i)
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      do 250 j=Jstart,Jend
      nu=jst+Nordr(j)
      do 248 k=Kstart,Kend
      lambda=kst+Nordr(k)
      Lend=lsave(Ldmax)
      if(Kml.EQ.0)Lend=k
      if(Imkjml.EQ.0.AND.i.EQ.k)Lend=j
      do 246 l=Lstart,Lend
      sigma=lst+Nordr(l)
      intc=intc+1
      
      call out2e(1,mu,nu,lambda,sigma,Tq(intc),dbuf,Ibuf2e,Dbuf2e,iret,i
     &dcout,IOP,D,F)
246   continue
248   continue
250   continue
255   continue
      
260   continue
280   continue
300   continue
400   continue
      call out2e(0,mu,nu,lambda,sigma,Tq(1),dbuf,Ibuf2e,Dbuf2e,iret,idco
     &ut,IOP,D,F)
      
      JUMP=0
      return
      
      end
C* :1 * 
      
