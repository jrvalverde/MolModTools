
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qinf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qinf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "qinf.web"
      subroutine qinf
      implicit none
      double precision absc,C1,C2,C3,C4,cmaxk,cmaxl,cs,Cutint,ds,Ep,Ep2i
     &,Eq,eqi,Eqsav,Exparg,Exx,gabs,gexp,gmax1
      integer Igauss,Igbeg,Igdf,Igend,Itype,Jan,Jgauss,Jgbeg,Jgdf,Jgend,
     &Jtype,Kgauss,Kgbeg,Kgdf,Kgend,Klcutq,Klind,knddf,Ktype,Lgauss
      integer Lgbeg,Lgdf,Lgend,lnddf,Ltype,MAXPRM,MAXS21,MAXSH1,MAXSHL,M
     &axtyp,Nshell
      double precision One,Pexp,Pqcut1,Pqcut2,Pqcut3,Ptest,Qexp,Qtest,Qx
     &psav,Qxsav,Qysav,Qzsav,Rabsq,Rcdsq,X,Xa,Xb,Xc,Xd,Xint
      double precision Y,Ya,Yb,Yc,Yd,Z,Za,Zb,Zc,Zd,Zero
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      common/coord/Xa,Ya,Za,Xb,Yb,Zb,Rabsq,Xc,Yc,Zc,Xd,Yd,Zd,Rcdsq
      common/gcloop/Igauss,Igbeg,Igend,Igdf,Jgauss,Jgbeg,Jgend,Jgdf,Kgau
     &ss,Kgbeg,Kgend,Kgdf,Lgauss,Lgbeg,Lgend,Lgdf
      common/qinfo/Eqsav(100),Qxsav(100),Qysav(100),Qzsav(100),Qxpsav(10
     &0),Exparg,Ptest,Pexp,Ep,Eq,Ep2i,Klind,Klcutq(100)
      common/dfcuts/Pqcut1,Pqcut2,Pqcut3,Cutint
      common/stypes/Itype,Jtype,Ktype,Ltype
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/int/Zero,Xint(12)
      equivalence(Ptest,Qtest),(Pexp,Qexp)
      equivalence(One,Xint(1))
      
      
      
      
      
      Klind=0
      do 100 Kgauss=Kgbeg,Kgend
      cs=Exx(Kgauss)
      cmaxk=gabs(C1(Kgauss))
      if(Ktype.GT.0)then
      absc=gabs(C2(Kgauss))
      cmaxk=gmax1(cmaxk,absc)
      if(Ktype.GT.1)then
      knddf=Kgdf+(Kgauss-Kgbeg)
      absc=gabs(C3(knddf))
      cmaxk=gmax1(cmaxk,absc)
      if(Ktype.GT.2)then
      absc=gabs(C4(knddf))
      cmaxk=gmax1(cmaxk,absc)
      endif
      endif
      endif
      
      do 50 Lgauss=Lgbeg,Lgend
      ds=Exx(Lgauss)
      cmaxl=gabs(C1(Lgauss))
      if(Ltype.GT.0)then
      absc=gabs(C2(Lgauss))
      cmaxl=gmax1(cmaxl,absc)
      if(Ltype.GT.1)then
      lnddf=Lgdf+(Lgauss-Lgbeg)
      absc=gabs(C3(lnddf))
      cmaxl=gmax1(cmaxl,absc)
      if(Ltype.GT.2)then
      absc=gabs(C4(lnddf))
      cmaxl=gmax1(cmaxl,absc)
      endif
      endif
      endif
      
      Klind=Klind+1
      Eq=cs+ds
      eqi=One/Eq
      Eqsav(Klind)=Eq
      Qxsav(Klind)=(cs*Xc+ds*Xd)*eqi
      Qysav(Klind)=(cs*Yc+ds*Yd)*eqi
      Qzsav(Klind)=(cs*Zc+ds*Zd)*eqi
      Exparg=cs*ds*Rcdsq*eqi
      if(Exparg.LE.Pqcut3)then
      
      Qexp=gexp(-Exparg)
      Qxpsav(Klind)=Qexp
      Qtest=cmaxk*cmaxl*Qexp
      if(Qtest.GE.Pqcut1)then
      Klcutq(Klind)=0
      
      elseif(Qtest.LT.Pqcut2)then
      Klcutq(Klind)=2
      else
      
      Klcutq(Klind)=1
      endif
      else
      Qxpsav(Klind)=Zero
      Klcutq(Klind)=2
      endif
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
