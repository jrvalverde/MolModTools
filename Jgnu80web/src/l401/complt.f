
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 complt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "complt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "complt.web"
      subroutine complt(AR,AI,B,AA,BB,CC,MDIM,NBASIS,NVEC,NORTH,ICMP,ICM
     &PLT,IDUMP)
      implicit none
      double precision AA,AI,AR,B,BB,big,CC,doti,dotmin,dotr,sumdot,veci
     &,vecr,zero
      integer i,ICMP,ICMPLT,IDUMP,im1,In,Iout,Ipunch,ivec,j,jb,jbmin,jve
     &c,k,lim,MDIM,NBASIS,NORTH,nstart,NVEC
      dimension AR(MDIM,MDIM),AI(MDIM,MDIM),B(MDIM,MDIM)
      dimension AA(MDIM),BB(MDIM),CC(MDIM)
      common/io/In,Iout,Ipunch
      data big/1.0D10/
      data zero/0.0D0/
      
      
      
      
      
      
      
      if(IDUMP.NE.0)write(Iout,99001)
      
99001 format('  ORTHGS')
      
      if(NVEC.NE.NORTH)then
      if(NORTH.LE.1)call gnorm(AR(1,1),AI(1,1),ICMP,NBASIS)
      lim=max(NORTH,2)
      
      do 100 i=lim,NVEC
      
      do 20 j=1,NBASIS
      AA(j)=zero
      BB(j)=zero
20    continue
      
      im1=i-1
      do 40 j=1,im1
      call gdot(AR(1,i),AI(1,i),AR(1,j),AI(1,j),dotr,doti,ICMP,NBASIS)
      
      do 30 k=1,NBASIS
      vecr=AR(k,j)
      veci=AI(k,j)
      AA(k)=AA(k)+dotr*vecr
      if(ICMP.NE.0)then
      AA(k)=AA(k)-doti*veci
      BB(k)=BB(k)+dotr*veci+doti*vecr
      endif
30    continue
40    continue
      
      do 60 j=1,NBASIS
      AR(j,i)=AR(j,i)-AA(j)
      if(ICMP.NE.0)AI(j,i)=AI(j,i)-BB(j)
60    continue
      call gnorm(AR(1,i),AI(1,i),ICMP,NBASIS)
100   continue
      endif
      
      
      if(ICMPLT.EQ.0)return
      nstart=NVEC+1
      if(nstart.GT.NBASIS)return
      
      do 200 i=1,NBASIS
      CC(i)=zero
200   continue
      
      do 500 ivec=nstart,NBASIS
      dotmin=big
      jvec=ivec-1
      
      do 250 jb=1,NBASIS
      sumdot=zero
      
      do 220 j=1,jvec
      call gdot(AR(1,j),AI(1,j),B(1,jb),CC,dotr,doti,ICMP,NBASIS)
      sumdot=sumdot+dotr*dotr+doti*doti
220   continue
      if(sumdot.LE.dotmin)then
      dotmin=sumdot
      jbmin=jb
      endif
250   continue
      
      do 300 i=1,NBASIS
      AR(i,ivec)=B(i,jbmin)
      if(ICMP.EQ.1)AI(i,ivec)=zero
      AA(i)=zero
      BB(i)=zero
300   continue
      
      do 350 j=1,jvec
      call gdot(AR(1,j),AI(1,j),AR(1,ivec),AI(1,ivec),dotr,doti,ICMP,NBA
     &SIS)
      
      do 320 k=1,NBASIS
      vecr=AR(k,j)
      veci=AI(k,j)
      AA(k)=AA(k)+dotr*vecr
      if(ICMP.NE.0)then
      AA(k)=AA(k)-doti*veci
      BB(k)=BB(k)+dotr*veci+doti*vecr
      endif
320   continue
350   continue
      
      do 400 k=1,NBASIS
      AR(k,ivec)=AR(k,ivec)-AA(k)
      if(ICMP.NE.0)AI(k,ivec)=AI(k,ivec)-BB(k)
400   continue
      call gnorm(AR(1,ivec),AI(1,ivec),ICMP,NBASIS)
500   continue
      return
      
      end
C* :1 * 
      
