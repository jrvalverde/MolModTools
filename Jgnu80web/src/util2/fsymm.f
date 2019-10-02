
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fsymm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fsymm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "fsymm.web"
      subroutine fsymm(NBASIS,F,NSYMOP,NEQBAS,LIND,FNEW)
      implicit none
      double precision dmup,dnup,F,FNEW,one,zero
      integer imunup,imup,inup,isym,k,LIND,MAXBAS,mu,mup,NBASIS,NEQBAS,N
     &SYMOP,nu,nup
      parameter(MAXBAS=150)
      dimension F(*),FNEW(*),NEQBAS(MAXBAS,8),LIND(*)
      data zero/0.0D0/,one/1.0D0/
      
      
      
      
      
      k=0
      do 100 mu=1,NBASIS
      do 50 nu=1,mu
      k=k+1
      FNEW(k)=zero
      
      do 20 isym=1,NSYMOP
      
      mup=NEQBAS(mu,isym)
      nup=NEQBAS(nu,isym)
      dmup=one
      if(mup.LT.0)dmup=-one
      dnup=one
      if(nup.LT.0)dnup=-one
      
      imup=iabs(mup)
      inup=iabs(nup)
      if(imup.LT.inup)then
      
      imunup=LIND(inup)+imup
      else
      imunup=LIND(imup)+inup
      endif
      
      FNEW(k)=FNEW(k)+dmup*dnup*F(imunup)
      
20    continue
      
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
