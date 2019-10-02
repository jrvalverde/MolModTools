
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 winc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "winc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "winc.web"
      subroutine winc(NOCC,NBASIS,IOCC,IRW,E,W,C)
      implicit none
      double precision C,E,gfloat,temp,W
      integer i,indc,IOCC,IRW,k,mu,NBASIS,NOCC,nsq,nu
      dimension E(*),W(*),C(*)
      
      
      
      
      
      
      nsq=NBASIS*NBASIS
      call tread(IRW,C(1),nsq,1,nsq,1,0)
      
      if(IOCC.NE.1)then
      temp=gfloat(IOCC)
      do 50 i=1,NBASIS
      E(i)=temp*E(i)
50    continue
      endif
      
      indc=0
      do 200 i=1,NOCC
      k=0
      do 100 mu=1,NBASIS
      do 60 nu=1,mu
      k=k+1
      W(k)=W(k)-C(mu+indc)*C(nu+indc)*E(i)
60    continue
100   continue
      indc=indc+NBASIS
200   continue
      
      if(IOCC.NE.1)then
      do 250 i=1,NBASIS
      E(i)=E(i)/temp
250   continue
      endif
      
      return
      
      end
C* :1 * 
      
