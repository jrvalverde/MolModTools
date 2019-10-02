
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbwrit"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbwrit.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "nbwrit.web"
      subroutine nbwrit(IX,NX,IDAR)
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
      maxix=ixmax*ISINGL
      ldar=NX*ISINGL
      if(Ionbo(IDAR).NE.0)then
      
      
      Nbnav=Ionbo(IDAR)
      max=0
50    min=max+1
      max=max+maxix
      if(max.GT.ldar)max=ldar
      do 100 i=min,max
      Ixdnbo(i-min+1)=IX(i)
100   continue
      if(ISINGL.EQ.1)write(Inbo,rec=Nbnav)Ixsnbo
      if(ISINGL.EQ.2)write(Inbo,rec=Nbnav)Ixdnbo
      Nbnav=Nbnav+1
      if(max.LT.ldar)goto 50
      else
      
      
      Ionbo(IDAR)=Nav
      Nbnav=Nav
      
      max=0
150   min=max+1
      max=max+maxix
      if(max.GT.ldar)max=ldar
      do 200 i=min,max
      Ixdnbo(i-min+1)=IX(i)
200   continue
      if(ISINGL.EQ.1)write(Inbo,rec=Nbnav)Ixsnbo
      if(ISINGL.EQ.2)write(Inbo,rec=Nbnav)Ixdnbo
      Nbnav=Nbnav+1
      if(max.LT.ldar)goto 150
      Nav=Nbnav
      return
      endif
      return
      end
C* :1 * 
      
