
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aosumf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aosumf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "aosumf.web"
      subroutine aosumf(INTC,TQ,XIP,YIP,ZIP)
      implicit none
      integer i,Idmp,Idump,Iend,Imj,Imk,Imkjml,Indix,Indiy,Indiz,Indjx,I
     &ndjy,Indjz,Indkx,Indky,Indkz,Indlx,Indly,Indlz,INTC
      integer Irange,Istart,ix,iy,iz,j,Jend,Jml,Jrange,Jstart,jx,jy,jz,k
     &,Kend,Kml,Krange,Kstart,kx,ky
      integer kz,l,Lamax,Lbmax,Lbound,Lcmax,Ldmax,Lend,Lentq,Lpmax,Lpqma
     &x,Lqmax,Lrange,Lstart,lx,ly,lz,N10ord,N5ord,N6ord
      integer N7ord,Nordr,Numdf
      double precision TQ,XIP,YIP,ZIP
      integer Ubound,Ulpure
      dimension TQ(*),XIP(*),YIP(*),ZIP(*)
      common/dump/Idmp,Idump
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      common/indxyz/Indix(20),Indiy(20),Indiz(20),Indjx(20),Indjy(20),In
     &djz(20),Indkx(20),Indky(20),Indkz(20),Indlx(20),Indly(20),Indlz(20
     &)
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      
      
      
      
      INTC=0
      do 100 i=Istart,Iend
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      ix=Indix(i)
      iy=Indiy(i)
      iz=Indiz(i)
      
      do 50 j=Jstart,Jend
      jx=Indjx(j)+ix
      jy=Indjy(j)+iy
      jz=Indjz(j)+iz
      
      do 20 k=Kstart,Kend
      Lend=Ubound(Ldmax)
      if(Kml.EQ.0)Lend=k
      if(Imkjml+iabs(i-k).EQ.0)Lend=j
      kx=Indkx(k)+jx
      ky=Indky(k)+jy
      kz=Indkz(k)+jz
      
      do 10 l=Lstart,Lend
      INTC=INTC+1
      lx=Indlx(l)+kx
      ly=Indly(l)+ky
      lz=Indlz(l)+kz
      TQ(INTC)=TQ(INTC)+(XIP(lx)*YIP(ly)*ZIP(lz))
10    continue
20    continue
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
