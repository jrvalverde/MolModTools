
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nlmo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nlmo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "nlmo.web"
      subroutine nlmo(N,A,EVAL,EVEC,TSYM,RESON,NOCC,IALARM)
      implicit none
      double precision A,absaij,abss,aii,aiii,aiil,aiiu,ajj,ajjj,ajjl,aj
     &ju,amax,c,degthr,differ,done,eps,EVAL,EVEC,fract
      double precision hundrd,occmin,offtop,offtst,one,RESON,rot,s,t,ten
     &,test,thr1,thr2,tot,totele,totp,TSYM,virmax,x,zero
      integer i,IALARM,ii,iimax,ilist,iocc,ioff,Ispin,iuniq,j,jemt,jj,jj
     &max,jlist,jm1,joff,juniq,Lfnao,Lfnarc,Lfndaf
      integer Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnl
     &m,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,moff,Munit,Mxao,Mxaolm,
     &Mxbo
      integer N,n1,n2,Natoms,Nbas,Ndim,nel,niuniq,njuniq,NOCC,noff,noffm
     &x,nrot,nrot2,ntime,nvirst
      
      
      
      
      
      
      
      logical Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      logical zeroj
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbflag/Rohf,Uhf,Ci,Open,Complx,Alpha,Beta,Mcscf,Auhf,Ortho
      dimension A(Ndim,Ndim),EVEC(Ndim,1),EVAL(1),TSYM(1),RESON(Ndim)
      dimension rot(2,2)
      dimension ilist(100),jlist(100),ioff(100),joff(100),iuniq(100),jun
     &iq(100)
      
      
      data differ,done,eps/1.0D-5,1.0D-10,1.0D-11/
      
      
      data degthr,noffmx/1.0D-3,100/
      data zero,one,ten,hundrd/0.0D0,1.0D0,10.0D0,100.0D0/
      
      write(Lfnpr,99003)
      thr1=one-degthr
      thr2=one-degthr*5
      ntime=0
      
      
      if(N.GT.1)then
      
      do 50 j=1,N
      do 20 i=1,N
      EVEC(i,j)=zero
20    continue
      EVEC(j,j)=one
50    continue
      
      
      totele=zero
      do 100 i=1,N
      totele=totele+A(i,i)
100   continue
      totele=totele+differ
      NOCC=totele
      if(Ispin.EQ.0)NOCC=NOCC/2+mod(NOCC,2)
      nvirst=NOCC+1
      
      
      IALARM=0
      virmax=zero
      do 150 j=nvirst,N
      if(A(j,j).GE.virmax)virmax=A(j,j)
150   continue
      occmin=hundrd
      do 200 i=1,NOCC
      if(A(i,i).LE.occmin)occmin=A(i,i)
200   continue
      x=occmin-virmax
      
      
      
      
      if(x.GT.differ)then
      
      
220   ntime=ntime+1
      
      
      offtop=zero
      do 240 jemt=nvirst,N
      do 230 iocc=1,NOCC
      absaij=dabs(A(iocc,jemt))
      if(absaij.GE.offtop)then
      offtop=absaij
      aii=A(iocc,iocc)
      ajj=A(jemt,jemt)
      endif
230   continue
240   continue
      
      
      if(offtop.LT.done)then
      
      totele=zero
      do 250 j=1,N
      EVAL(j)=A(j,j)
      totele=totele+EVAL(j)
      x=EVEC(j,j)
      RESON(j)=x*x*hundrd
250   continue
      totp=totele+differ
      nel=totp
      tot=nel
      fract=dabs(totele-tot)
      if(fract.GT.differ)then
      write(Lfnpr,99008)differ,totele
      write(Lfnpr,99006)
      write(Lfnpr,99007)(EVAL(i),i=1,Nbas)
      IALARM=1
      return
      
99001 format(/1x,'Highest occupied NBOs are not at the beginning',' of t
     &he NBO list;',/,1x,'The NLMO program is not ','currently set up to
     & handle this.')
99002 format(/1x,'Degeneracy between orbitals in the (a) and (b)',' sets
     & detected;',/1x,'NLMO program cannot always handle this situation.
     &')
99003 format(//1x,'NATURAL LOCALIZED MOLECULAR ORBITAL (NLMO) ','ANALYSI
     &S:')
99004 format(/1x,'Maximum off-diagonal element of DM in NLMO basis:',f10
     &.5)
99005 format(/1x,'Something went wrong in the NLMO procedure; density','
     & matrix of SCF',/1x,'wave function has not been diagonalized')
99006 format(/1x,'Occupancies of NLMOs:')
99007 format(/1x,8F10.5)
99008 format(/1x,'Number of electrons (trace of DM, NLMO basis) is not',
     &' within ',f10.5,/,' of an integer:',f10.5,' - - PROGRAM ABORT')
      endif
      else
      
      
      offtst=offtop*thr1
      aiil=aii*thr2
      ajjl=ajj*thr2
      aiiu=aii/thr2
      ajju=ajj/thr2
      zeroj=.FALSE.
      if(ajj.LT.differ)zeroj=.TRUE.
      noff=0
      do 260 jemt=nvirst,N
      do 255 iocc=1,NOCC
      absaij=dabs(A(iocc,jemt))
      if(absaij.GE.offtst)then
      aiii=A(iocc,iocc)
      ajjj=A(jemt,jemt)
      if((aiii.GE.aiil).AND.(aiii.LE.aiiu))then
      
      
      if(.NOT.(zeroj))then
      if((ajjj.LT.ajjl).OR.(ajjj.GT.ajju))goto 255
      endif
      noff=noff+1
      ioff(noff)=iocc
      joff(noff)=jemt
      endif
      endif
255   continue
260   continue
      if(noff.LT.noffmx)then
      
      s=ajj-aii
      abss=dabs(s)
      
      
      test=eps*offtop
      if(abss.GT.test)then
      
      t=offtop/s
      s=0.25D0/dsqrt(0.25D0+t*t)
      
      c=dsqrt(0.5D0+s)
      s=2.D0*t*s/c
      else
      s=.707106781D0
      c=s
      endif
      
      if(noff.GT.1)then
      
      
      
      
      
      iuniq(1)=ioff(1)
      ilist(1)=1
      niuniq=1
      do 264 moff=2,noff
      i=ioff(moff)
      iimax=moff-1
      do 262 ii=1,iimax
      if(ioff(ii).EQ.i)then
      ilist(moff)=ilist(ii)
      goto 264
      endif
262   continue
      niuniq=niuniq+1
      ilist(moff)=niuniq
      iuniq(niuniq)=i
264   continue
      
      juniq(1)=joff(1)
      jlist(1)=niuniq+1
      njuniq=1
      do 268 moff=2,noff
      j=joff(moff)
      jjmax=moff-1
      do 266 jj=1,jjmax
      if(joff(jj).EQ.j)then
      jlist(moff)=jlist(jj)
      goto 268
      endif
266   continue
      njuniq=njuniq+1
      jlist(moff)=njuniq+niuniq
      juniq(njuniq)=j
268   continue
      nrot=niuniq+njuniq
      nrot2=nrot*nrot
      n1=nrot2+1
      n2=nrot2+n1
      call symuni(TSYM,A,c,s,TSYM(n1),TSYM(n2),EVAL,nrot,niuniq,njuniq,i
     &list,jlist,noff,ioff,joff,Ndim)
      
      ii=niuniq
      do 270 i=1,njuniq
      ii=ii+1
      iuniq(ii)=juniq(i)
270   continue
      call limtrn(EVEC,iuniq,TSYM,EVAL,Ndim,N,nrot,nrot,1)
      
      call limtrn(A,iuniq,TSYM,EVAL,Ndim,N,nrot,nrot,0)
      else
      iocc=ioff(1)
      jemt=joff(1)
      if(A(iocc,jemt).LT.zero)s=-s
      rot(1,1)=c
      rot(2,2)=c
      rot(1,2)=s
      rot(2,1)=-s
      ioff(2)=joff(1)
      call limtrn(A,ioff,rot,EVAL,Ndim,N,2,2,0)
      
      do 272 i=1,N
      t=EVEC(i,iocc)
      EVEC(i,iocc)=c*t-EVEC(i,jemt)*s
      EVEC(i,jemt)=s*t+EVEC(i,jemt)*c
272   continue
      endif
      goto 220
      else
      write(Lfnpr,99009)noff,noffmx
99009 format(//1x,'NOFF = ',i5,' IS GREATER THAN NOFFMX =',i5,/5x,'  MUS
     &T ABORT NLMO PROCEDURE')
      IALARM=1
      return
      endif
      endif
      else
      
      
      IALARM=1
      if(dabs(x).GT.differ)then
      write(Lfnpr,99001)
      else
      write(Lfnpr,99002)
      endif
      return
      endif
      else
      EVEC(1,1)=one
      EVAL(1)=A(1,1)
      return
      endif
      amax=zero
      do 400 j=2,N
      jm1=j-1
      do 300 i=1,jm1
      if(dabs(A(i,j)).GE.amax)amax=dabs(A(i,j))
300   continue
400   continue
      write(Lfnpr,99004)amax
      if(Ci.OR.Auhf.OR.Mcscf)return
      if(amax.LT.hundrd*ten*done)return
      write(Lfnpr,99005)
      IALARM=1
      return
      end
C* :1 * 
      
