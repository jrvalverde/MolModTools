
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbread"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbread.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "nbread.web"
      subroutine nbread(IX,NX,IDAR)
      implicit none
      integer i,IDAR,Inbo,Ionbo,ISINGL,IX,Ixdnbo,ixmax,Ixsnbo,ldar,LENGT
     &H,max,maxix,min,Nav,NBDAR,Nbnav,NX
      parameter(ISINGL=2,LENGTH=256)
      parameter(NBDAR=100)
      common/nbodaf/Inbo,Nav,Ionbo(NBDAR)
      common/nbonav/Ixdnbo(LENGTH),Nbnav
      dimension IX(*),Ixsnbo(LENGTH/2)
      equivalence(Ixsnbo(1),Ixdnbo(1))
      
      ixmax=LENGTH/2
      Nbnav=Ionbo(IDAR)
      maxix=ixmax*ISINGL
      ldar=NX*ISINGL
      
      max=0
100   min=max+1
      max=max+maxix
      if(max.GT.ldar)max=ldar
      if(ISINGL.EQ.1)read(Inbo,rec=Nbnav)Ixsnbo
      if(ISINGL.EQ.2)read(Inbo,rec=Nbnav)Ixdnbo
      do 200 i=min,max
      IX(i)=Ixdnbo(i-min+1)
200   continue
      Nbnav=Nbnav+1
      if(max.LT.ldar)goto 100
      return
      end
C* :1 * 
      
