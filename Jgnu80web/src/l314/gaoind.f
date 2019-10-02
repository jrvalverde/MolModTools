
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gaoind"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gaoind.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "gaoind.web"
      subroutine gaoind(IOP)
      implicit none
      integer i,Idmp,Idump,Iend,Ifao,Ifcont,Imj,Imk,Imkjml,ind,Indao,Ind
     &ix,Indiy,Indiz,Indjx,Indjy,Indjz,Indkx,Indky,Indkz
      integer Indlx,Indly,Indlz,Irange,Istart,ix,iy,iz,j,Jend,Jml,Jrange
     &,Jstart,jx,jy,jz,k,Kend,Kml,Krange
      integer Kstart,kx,ky,kz,l,Lamax,Lbmax,Lbound,Lcmax,Ldmax,Lend,Lent
     &q,Limxyz,Lpmax,Lpqmax,Lqmax,Lrange,Lstart,lx,ly
      integer lz,Maxxyz,N10ord,N5ord,N6ord,N7ord,Nordr,Numdf
      integer Ubound,Ulpure
      integer IOP(*)
      common/dump/Idmp,Idump
      common/indxyz/Indix(20),Indiy(20),Indiz(20),Indjx(20),Indjy(20),In
     &djz(20),Indkx(20),Indky(20),Indkz(20),Indlx(20),Indly(20),Indlz(20
     &)
      common/aoinds/Ifcont,Limxyz,Maxxyz,Ifao,Indao(1296)
      common/limit/Imj,Imk,Jml,Kml,Imkjml,Istart,Jstart,Kstart,Lstart,Ie
     &nd,Jend,Kend,Lend,Irange,Jrange,Krange,Lrange,Lentq,Numdf
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      
      
      
      
      Ifao=1
      if(IOP(12).NE.0)Ifao=0
      Limxyz=0
      ind=0
      do 100 i=Istart,Iend
      ix=Indix(i)
      iy=Indiy(i)
      iz=Indiz(i)
      
      if(Imj.EQ.0)Jend=i
      if(Imkjml.EQ.0)Kend=i
      
      do 50 j=Jstart,Jend
      jx=Indjx(j)+ix
      jy=Indjy(j)+iy
      jz=Indjz(j)+iz
      
      do 20 k=Kstart,Kend
      kx=Indkx(k)+jx
      ky=Indky(k)+jy
      kz=Indkz(k)+jz
      
      Lend=Ubound(Ldmax)
      if(Kml.EQ.0)Lend=k
      if(Imkjml.EQ.0.AND.i.EQ.k)Lend=j
      
      do 10 l=Lstart,Lend
      Limxyz=Limxyz+1
      if(Limxyz.GT.Maxxyz)goto 200
      lx=Indlx(l)+kx
      ly=Indly(l)+ky
      lz=Indlz(l)+kz
      call ipt123(ind,lx,ly,lz,Indao)
10    continue
20    continue
50    continue
100   continue
      Ifao=1
      if(IOP(12).NE.0)Ifao=0
      
200   return
      
      end
C* :1 * 
      
