
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lpsctr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lpsctr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "lpsctr.web"
      subroutine lpsctr(NSCATR,ISCATR,JSCATR,ISYMOP,TQIN,TQOUT,TQEQ,ITRA
     &NS)
      implicit none
      integer i,Icntfl,idx,iebeg,Iend,Iitypp,Ijcnfl,Ijtyp2,Ijtype,Imj,in
     &beg,Inew,Irange,irbeg,isbeg,ISCATR,ishell,Istart,ISYMOP,itbeg
      integer itr,ITRANS,Itype,Itypmx,j,Jcntfl,jebeg,Jend,Jjtypp,jnbeg,J
     &new,jop,Jrange,jrbeg,jsbeg,JSCATR,jshell,Jstart,jtbeg,jtr
      integer Jtype,Jtypmx,k,labeg,Lamax,lbbeg,Lbmax,Lbound,Lentq,Limitd
     &,Lpmax,Maxdum,Mxcinl,N10ord,N5ord,N6ord,N7ord,Nordr,NSCATR
      real*8 TQEQ,TQIN,TQOUT
      integer sconap,sconbp,Ubound,Ulpure
      integer MAXSHL,MAXPRM,MAXSH1,MAXS21,Jan,Shella,Shelln,Shellt,Shell
     &c,Shladf,Aos,Aon,Nshell,Maxtyp
      real*8 Exx,C1,C2,C3,C4,X,Y,Z
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/new/Inew,Jnew
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limit
     &d(11)
      common/type/Itype,Jtype,Iitypp,Jjtypp,Ijtype,Ijtyp2,Mxcinl,Itypmx,
     &Jtypmx,Icntfl,Jcntfl,Ijcnfl
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      dimension ISCATR(*),JSCATR(*),ISYMOP(*),TQIN(*),TQOUT(*),TQEQ(*),I
     &TRANS(20,8)
      
      inbeg=Inew
      isbeg=Istart
      iebeg=Iend
      irbeg=Irange
      itbeg=Itype
      labeg=Lamax
      jnbeg=Jnew
      jsbeg=Jstart
      jebeg=Jend
      jrbeg=Jrange
      jtbeg=Jtype
      lbbeg=Lbmax
      
      do 100 k=1,NSCATR
      ishell=ISCATR(k)
      jshell=JSCATR(k)
      jop=ISYMOP(k)
      Inew=ishell
      Jnew=jshell
      Imj=iabs(ishell-jshell)
      Itype=Shellt(ishell)
      sconap=Shellc(ishell)+1
      Lamax=Itype+1
      Istart=Lbound(Lamax,sconap)
      Iend=Ubound(Lamax)
      Irange=Iend-Istart+1
      
      Jtype=Shellt(jshell)
      sconbp=Shellc(jshell)+1
      Lbmax=Jtype+1
      Jstart=Lbound(Lbmax,sconbp)
      Jend=Ubound(Lbmax)
      Jrange=Jend-Jstart+1
      
      idx=0
      do 50 i=Istart,Iend
      itr=ITRANS(i,jop)
      do 20 j=Jstart,Jend
      jtr=itr*ITRANS(j,jop)
      idx=idx+1
      TQEQ(idx)=TQIN(idx)*jtr
20    continue
50    continue
      call fillp(TQEQ,TQOUT)
100   continue
      
      Inew=inbeg
      Istart=isbeg
      Iend=iebeg
      Irange=irbeg
      Itype=itbeg
      Lamax=labeg
      Jnew=jnbeg
      Jstart=jsbeg
      Jend=jebeg
      Jrange=jrbeg
      Jtype=jtbeg
      Lbmax=lbbeg
      
      return
      end
C* :1 * 
      
