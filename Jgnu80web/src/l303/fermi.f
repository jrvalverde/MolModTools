
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fermi"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fermi.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "fermi.web"
      subroutine fermi(NBASIS,NATOMS,C,FRMI,IPRINT)
      implicit none
      double precision acx,acy,acz,arg,ax,ay,az,C,C1,C2,C3,C4,coef,cx,cy
     &,cz,Dummy,dx2y2,dz2,exparg
      double precision Exx,f0,f1m,f1p,f2m,f2p,f3m,f3p,FRMI,gexp,gexp1,ha
     &cx,hacy,hacz,one,Pt5,R1,R2,R3,R3ov2
      double precision R4,racsq,Root15,Root3,Root5,v,X,Y,Z,Z1,Z2,Z3,Zero
      integer i,iaind,iaos,iatom,icol,Idum,iend,igauss,igbegn,igdf,igend
     &,In,Indjx,Indjy,Indjz,Iout,ipowx,ipowy,ipowz,IPRINT
      integer Ipunch,Ipurd,Ipurf,irwfrm,ishell,istart,itype,ix,iy,iz,Jan
     &,Lbound,len,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,mu,N10ord
      integer N5ord,N6ord,N7ord,NATOMS,NBASIS,Nordr,Nshell
      integer Shella,Shellc,Shelln,Shellt,Shladf,Aos,Aon
      integer sconst
      integer Ubound,Ulpure
      dimension C(*),FRMI(*),v(20)
      dimension coef(20)
      dimension ix(20),iy(20),iz(20)
      dimension hacx(4),hacy(4),hacz(4)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/io/In,Iout,Ipunch
      common/ipure/Ipurd,Ipurf
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/inds/Indjx(20),Indjy(20),Indjz(20),Idum(60)
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/const/Zero,Dummy(8)
      data one/1.0D0/
      data irwfrm/17/
      
      
      
      
      
      
      
99001 format(1H0////5x,'FERMI CONTACT INTEGRALS (NBASIS X NATOMS)')
      
      hacx(1)=one
      hacy(1)=one
      hacz(1)=one
      do 100 i=1,20
      ix(i)=Indjx(i)-1
      iy(i)=Indjy(i)-1
      iz(i)=Indjz(i)-1
100   continue
      
      
      do 300 iatom=1,NATOMS
      iaind=3*(iatom-1)
      cx=C(1+iaind)
      cy=C(2+iaind)
      cz=C(3+iaind)
      
      
      do 200 ishell=1,Nshell
      ax=X(ishell)
      ay=Y(ishell)
      az=Z(ishell)
      igbegn=Shella(ishell)
      igend=igbegn+Shelln(ishell)-1
      itype=Shellt(ishell)+1
      sconst=Shellc(ishell)+1
      iend=Ubound(itype)
      istart=Lbound(itype,sconst)
      igdf=Shladf(ishell)
      iaos=Aos(ishell)-1
      acx=cx-ax
      acy=cy-ay
      acz=cz-az
      racsq=acx*acx+acy*acy+acz*acz
      do 120 i=2,4
      hacx(i)=hacx(i-1)*acx
      hacy(i)=hacy(i-1)*acy
      hacz(i)=hacz(i-1)*acz
120   continue
      
      
      do 140 i=1,20
      v(i)=Zero
140   continue
      
      
      do 160 igauss=igbegn,igend
      gexp1=Exx(igauss)
      call fillc(itype-1,igbegn,igauss,igdf,coef)
      arg=-gexp1*racsq
      exparg=gexp(arg)
      
      
      do 150 i=istart,iend
      ipowx=ix(i)
      ipowy=iy(i)
      ipowz=iz(i)
      v(i)=v(i)+coef(i)*hacx(ipowx+1)*hacy(ipowy+1)*hacz(ipowz+1)*exparg
150   continue
      
160   continue
      
      
      if(Ipurd*Ipurf.NE.1)then
      if(itype.GE.3)then
      if(itype.EQ.4)then
      
      if(Ipurf.NE.1)then
      
      
      f0=v(13)-R2*(v(16)+v(19))
      f1p=R4*(Z1*v(17)-v(11)-Z2*v(14))
      f1m=R4*(Z1*v(18)-v(12)-Z2*v(15))
      f2p=R3*(v(16)-v(19))
      f2m=v(20)
      f3p=R1*(v(11)-Z3*v(14))
      f3m=R1*(Z3*v(15)-v(12))
      
      v(11)=f0
      v(12)=f1p
      v(13)=f1m
      v(14)=f2p
      v(15)=f2m
      v(16)=f3p
      v(17)=f3m
      iend=17
      endif
      elseif(Ipurd.NE.1)then
      
      
      dz2=v(7)-Pt5*(v(5)+v(6))
      dx2y2=R3ov2*(v(5)-v(6))
      
      v(5)=dz2
      v(6)=v(9)
      v(7)=v(10)
      v(9)=v(8)
      v(8)=dx2y2
      iend=9
      endif
      endif
      endif
      
      
      icol=NBASIS*(iatom-1)
      do 180 i=istart,iend
      mu=iaos+Nordr(i)
      FRMI(mu+icol)=v(i)
180   continue
      
200   continue
      
300   continue
      
      
      len=NBASIS*NATOMS
      call twrite(irwfrm,FRMI,len,1,len,1,0)
      if(IPRINT.NE.0)then
      write(Iout,99001)
      call matout(FRMI,NBASIS,NATOMS,NBASIS,NATOMS)
      endif
      return
      
      end
C* :1 * 
      
