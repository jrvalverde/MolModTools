
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 symuni"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "symuni.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 33 "symuni.web"
      
      
      
      subroutine symuni(TSYM,A,COS,SIN,OVLP,BLK,EVAL,NROT,NIUNIQ,NJUNIQ,
     &ILIST,JLIST,NOFF,IOFF,JOFF,NDIM)
      implicit none
      double precision A,ave,BLK,COS,eps,EVAL,one,OVLP,rnorm,SIN,t,tot,T
     &SYM,u,zero
      integer i,ii,ILIST,iocc,IOFF,j,jemt,jj,JLIST,JOFF,jst,jst2,Lfnao,L
     &fnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab
      integer Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lf
     &nppa,Lfnpr,moff,NDIM,NIUNIQ,NJUNIQ,NOFF,noff2,NROT
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension TSYM(NROT,NROT),A(NDIM,NDIM),BLK(NROT,NROT),OVLP(NROT,NR
     &OT),EVAL(NROT)
      dimension IOFF(NOFF),JOFF(NOFF),ILIST(NOFF),JLIST(NOFF)
      data zero,one/0.0D0,1.0D0/
      data eps/1.0D-6/
      do 100 i=1,NROT
      do 50 j=1,NROT
      TSYM(i,j)=zero
50    continue
      TSYM(i,i)=one
100   continue
      do 200 moff=1,NOFF
      iocc=ILIST(moff)
      jemt=JLIST(moff)
      do 150 i=1,NROT
      t=TSYM(i,iocc)
      u=TSYM(i,jemt)
      TSYM(i,iocc)=COS*t-SIN*u
      TSYM(i,jemt)=SIN*t+COS*u
150   continue
200   continue
      
      
      
      jst=NIUNIQ+1
      NROT=jst-1+NJUNIQ
      
      if(NIUNIQ.NE.1)then
      tot=zero
      do 250 i=1,NIUNIQ
      tot=tot+TSYM(i,i)
250   continue
      ave=tot/NIUNIQ
      do 300 i=1,NIUNIQ
      TSYM(i,i)=ave
300   continue
      endif
      
      if(NJUNIQ.NE.1)then
      tot=zero
      do 350 j=jst,NROT
      tot=tot+TSYM(j,j)
350   continue
      ave=tot/NJUNIQ
      do 400 j=jst,NROT
      TSYM(j,j)=ave
400   continue
      endif
      
      if(NIUNIQ.NE.1)then
      do 450 i=2,NIUNIQ
      do 420 j=1,i
      if(i.NE.j)then
      TSYM(i,j)=zero
      TSYM(j,i)=zero
      endif
420   continue
450   continue
      endif
      
      if(NJUNIQ.NE.1)then
      jst2=jst+1
      do 500 i=jst2,NROT
      do 460 j=jst,i
      if(i.NE.j)then
      TSYM(i,j)=zero
      TSYM(j,i)=zero
      endif
460   continue
500   continue
      endif
      
      tot=zero
      do 600 moff=1,NOFF
      ii=ILIST(moff)
      jj=JLIST(moff)
      tot=tot+dabs(TSYM(ii,jj))+dabs(TSYM(jj,ii))
600   continue
      noff2=NOFF*2
      ave=tot/noff2
      do 700 moff=1,NOFF
      ii=ILIST(moff)
      jj=JLIST(moff)
      TSYM(ii,jj)=-ave
      TSYM(jj,ii)=ave
700   continue
      
      do 800 i=1,NIUNIQ
      do 750 j=jst,NROT
      do 720 moff=1,NOFF
      if(i.EQ.ILIST(moff).AND.j.EQ.JLIST(moff))goto 750
720   continue
      TSYM(i,j)=zero
      TSYM(j,i)=zero
750   continue
800   continue
      
      do 900 j=1,NROT
      tot=zero
      do 850 i=1,NROT
      tot=tot+TSYM(i,j)*TSYM(i,j)
850   continue
      rnorm=dsqrt(tot)
      if(rnorm.GT.eps)then
      do 860 i=1,NROT
      TSYM(i,j)=TSYM(i,j)/rnorm
860   continue
      else
      write(Lfnpr,99001)NROT,tot,eps,rnorm
99001 format('NROT,TOT,EPS,RNORM:',i3,3F14.9)
      call output(TSYM,NROT,NROT,NROT,NROT)
      stop
      endif
900   continue
      
      do 1000 moff=1,NOFF
      i=IOFF(moff)
      j=JOFF(moff)
      if(A(i,j).LE.zero)then
      ii=ILIST(moff)
      jj=JLIST(moff)
      TSYM(ii,jj)=-TSYM(ii,jj)
      TSYM(jj,ii)=-TSYM(jj,ii)
      endif
1000  continue
      
      call symort(OVLP,TSYM,BLK,NROT,NROT,EVAL)
      return
      
      end
C* :1 * 
      
