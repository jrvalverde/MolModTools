
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fillp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fillp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "fillp.web"
      subroutine fillp(F,A)
      implicit none
      real*8 A,F
      integer i,Iend,ii,Imj,indfm,indx1,Inew,intc,Ipurd,Ipurf,Irange,ist
     &,Istart,ix,j,Jend,jj,Jnew,Jrange,jst
      integer Jstart,jx,Lamax,Lbmax,Lbound,Lentq,Limitd,lind,Lpmax,Maxdu
     &m,N10ord,N5ord,N6ord,N7ord,Nordr
      integer Ubound,Ulpure
      dimension F(*),A(*)
      common/ipure/Ipurd,Ipurf
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Limit
     &d(11)
      common/new/Inew,Jnew
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
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      
      
      
      
      lind(i,j)=(i*(i-1))/2+j
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
      indfm=lind(jj,ii)
      elseif(ii.EQ.jj)then
      indfm=lind(ii,ii)
      else
      indfm=lind(ii,jj)
      endif
      
      A(indfm)=A(indfm)+F(intc)
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
      
