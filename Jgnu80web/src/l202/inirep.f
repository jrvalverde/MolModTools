
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 inirep"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "inirep.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "inirep.web"
      subroutine inirep(NGRP,NATOMS)
      implicit none
      integer blank,c,i,iord,j,n,NATOMS,NGRP,numer
      double precision zero
      dimension NGRP(4)
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data zero/0.0D0/,blank/' '/
      
      
      Nsymop=0
      Nreps=0
      do 100 i=1,32
      Lblrep(i)=blank
100   continue
      do 200 i=1,16
      do 150 j=1,10
      Chrtbl(j,i)=zero
150   continue
200   continue
      do 300 i=1,10
      do 250 j=1,9
      Symops(j,i)=zero
250   continue
300   continue
      do 400 i=1,10
      do 350 j=1,100
      Iprmut(j,i)=0
350   continue
400   continue
      
      c=NGRP(1)
      if(c.EQ.iord('C'))then
      
      c=NGRP(2)
      if(c.EQ.iord('I'))call repci
      if(c.EQ.iord('S'))call repcs
      if(c.EQ.iord('*'))call repcst(NATOMS)
      n=numer(NGRP)
      if(n.GT.0)then
      c=NGRP(4)
      if(c.EQ.iord('V'))call repcnv(n)
      if(c.EQ.iord('H'))call repcnh(n)
      if(c.EQ.iord(' '))call repcn(n)
      endif
      elseif(c.EQ.iord('D'))then
      
      if(NGRP(2).EQ.iord('*'))call repdst(NATOMS)
      n=numer(NGRP)
      if(n.GT.0)then
      c=NGRP(4)
      if(c.EQ.iord('H'))call repdnh(n)
      if(c.EQ.iord('D'))call repdnd(n)
      if(c.EQ.iord(' '))call repdn(n)
      endif
      elseif(c.EQ.iord('T'))then
      
      if(NGRP(2).EQ.iord('D'))call reptd
      if(NGRP(2).EQ.iord(' '))call rept
      elseif(c.EQ.iord('O'))then
      
      if(NGRP(2).EQ.iord('H'))call repoh
      if(NGRP(2).EQ.iord(' '))call repo
      elseif(c.NE.iord('K'))then
      if(c.EQ.iord('I'))then
      endif
      endif
      
      return
      
      end
C* :1 * 
      
