
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sinfo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sinfo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "sinfo.web"
      subroutine sinfo
      implicit none
      double precision Ag,Ax,Ay,Az,Bg,Bx,By,Bz,C1,C2,C3,Cg,Cmax,Cmaxa,Cm
     &axb,Cmaxc,Cmaxd,Cpa,Cpb,Cpc
      double precision Cpd,Csa,Csb,Csc,Csd,Cx,Cy,Cz,Dg,Dx,Dy,Dz,Error1,E
     &rror2,Exx,P11,P12,P13,P21,P22
      double precision P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q
     &33,Rab,Rabsq,Rcd,Rcdsq,X,Y,Z
      integer i,Inew,Ishell,Isml,Ismlp,Ismlq,j,Jan,Jnew,Jshell,k,Knew,Ks
     &hell,l,La,Lb,Lc,Ld,Lnew,Lshell
      integer Mab,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Mcd,n,Nga,Ngangb,Ng
     &b,Ngc,Ngd,ni,nj,nk,nl,Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      common/shlnos/Ishell,Jshell,Kshell,Lshell,Inew,Jnew,Knew,Lnew
      common/shlinf/Nga,La,Ag(10),Csa(10),Cpa(10),Ngb,Lb,Bg(10),Csb(10),
     &Cpb(10),Ngc,Lc,Cg(10),Csc(10),Cpc(10),Ngd,Ld,Dg(10),Csd(10),Cpd(10
     &)
      common/misc/Mab,Mcd,Ngangb
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/maxc/Cmax(240),Cmaxa(10),Cmaxb(10),Cmaxc(10),Cmaxd(10),Isml
     &p(100),Ismlq,Isml,Error1,Error2
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      
      
      
      
      
      
      i=Shella(Inew)
      j=Shella(Jnew)
      k=Shella(Knew)
      l=Shella(Lnew)
      Nga=Shelln(Inew)
      Ngb=Shelln(Jnew)
      Ngc=Shelln(Knew)
      Ngd=Shelln(Lnew)
      Ax=X(Inew)
      Ay=Y(Inew)
      Az=Z(Inew)
      Bx=X(Jnew)
      By=Y(Jnew)
      Bz=Z(Jnew)
      Cx=X(Knew)
      Cy=Y(Knew)
      Cz=Z(Knew)
      Dx=X(Lnew)
      Dy=Y(Lnew)
      Dz=Z(Lnew)
      do 100 ni=1,Nga
      n=i-1+ni
      Cmaxa(ni)=Cmax(n)
      Ag(ni)=Exx(n)
      Csa(ni)=C1(n)
      Cpa(ni)=C2(n)
100   continue
      do 200 nj=1,Ngb
      n=j-1+nj
      Cmaxb(nj)=Cmax(n)
      Bg(nj)=Exx(n)
      Csb(nj)=C1(n)
      Cpb(nj)=C2(n)
200   continue
      do 300 nk=1,Ngc
      n=k-1+nk
      Cmaxc(nk)=Cmax(n)
      Cg(nk)=Exx(n)
      Csc(nk)=C1(n)
      Cpc(nk)=C2(n)
300   continue
      do 400 nl=1,Ngd
      n=l-1+nl
      Cmaxd(nl)=Cmax(n)
      Dg(nl)=Exx(n)
      Csd(nl)=C1(n)
      Cpd(nl)=C2(n)
400   continue
      Mab=La+Lb-1
      Mcd=Lc+Ld-1
      Ngangb=Nga*Ngb
      return
      
      end
C* :1 * 
      
