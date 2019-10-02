
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbopen"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbopen.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 109 "nbopen.web"
      subroutine nbopen(NEW,ERROR)
      
      implicit none
      integer BYTES
      integer i,Inbo,Ionbo,ISINGL,Ix,Ixdnbo,Ixsnbo,ldar,LENGTH,Lfnao,Lfn
     &arc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho
      integer Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,max,maxix,
     &min,Nav,NBDAR,Nbnav,nfile,nx
      logical NEW,ERROR
      
      
      
      parameter(ISINGL=2,LENGTH=256)
      parameter(BYTES=4*LENGTH)
      parameter(NBDAR=100)
      common/nbodaf/Inbo,Nav,Ionbo(NBDAR)
      common/nbonav/Ixdnbo(LENGTH),Nbnav
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension Ix(2+NBDAR),Ixsnbo(LENGTH/2)
      equivalence(Ixsnbo(1),Ixdnbo(1))
      equivalence(Ix(1),Inbo)
      
      Inbo=abs(Lfndaf)
      
      
      if(NEW)then
      open(unit=Inbo,status='NEW',access='DIRECT',recl=BYTES,form='UNFOR
     &MATTED',err=200)
      Nav=1
      Nbnav=1
      do 50 i=1,NBDAR
      Ionbo(i)=0
50    continue
      nfile=1
      nx=NBDAR/2+1
      call nbwrit(Ix,nx,nfile)
      
      
      else
      open(unit=Inbo,status='OLD',access='DIRECT',recl=BYTES,form='UNFOR
     &MATTED',err=200)
      Nbnav=1
      maxix=LENGTH*ISINGL/2
      ldar=(NBDAR/2+1)*ISINGL
      max=0
100   min=max+1
      max=max+maxix
      if(max.GT.ldar)max=ldar
      if(ISINGL.EQ.1)read(Inbo,rec=Nbnav)Ixsnbo
      if(ISINGL.EQ.2)read(Inbo,rec=Nbnav)Ixdnbo
      do 150 i=min,max
      Ix(i)=Ixdnbo(i-min+1)
150   continue
      Nbnav=Nbnav+1
      if(max.LT.ldar)goto 100
      Inbo=abs(Lfndaf)
      endif
      ERROR=.FALSE.
      return
      
      
200   ERROR=.TRUE.
      return
      end
C* :1 * 
      
