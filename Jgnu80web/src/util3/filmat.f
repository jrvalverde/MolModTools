
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 filmat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "filmat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "filmat.web"
      subroutine filmat(F,A)
      implicit none
      double precision A,C1,C2,C3,Exx,F,X,Y,Z
      integer i,Iend,Ifilla,ii,Imj,indfm,indx1,Inew,intc,Ipurd,Ipurf,Ira
     &nge,ist,Istart,ix,j,Jan,Jend,jj,Jnew
      integer Jrange,jst,Jstart,jx,Lamax,Lbmax,Lbound,Lentq,Limdum,Lind,
     &Lpmax,Maxdum,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,N10ord,N5ord,N6ord
      integer N7ord,Nordr,Nshell
      integer Aos,Aon,Shella,Shelln,Shellt,Shellc
      integer Ubound,Ulpure
      dimension F(*),A(*)
      common/ipure/Ipurd,Ipurf
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limdu
     &m(11)
      common/new/Inew,Jnew
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/ia/Lind(164),Ifilla(92)
      
      
      
      
      call purdf1(F)
      
      
      
      indx1=0
      ist=Aos(Inew)-1
      jst=Aos(Jnew)-1
      do 100 i=Istart,Iend
      ix=Nordr(i)
      if(Imj.EQ.0)Jend=i
      intc=indx1
      do 50 j=Jstart,Jend
      intc=intc+1
      jx=Nordr(j)
      ii=ix+ist
      jj=jx+jst
      if(ii.LT.jj)then
      indfm=Lind(jj)+ii
      elseif(ii.EQ.jj)then
      indfm=Lind(ii+1)
      else
      indfm=Lind(ii)+jj
      endif
      
      A(indfm)=F(intc)
50    continue
      indx1=indx1+Jrange
100   continue
      
      
      Iend=Ubound(Lamax)
      Jend=Ubound(Lbmax)
      Irange=Iend-Istart+1
      Jrange=Jend-Jstart+1
      return
      
      end
C* :1 * 
      
