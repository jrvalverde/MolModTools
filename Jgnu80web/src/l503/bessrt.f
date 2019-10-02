
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bessrt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bessrt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "bessrt.web"
      subroutine bessrt(NBASIS,ISW,TAU,A,B,MDIM,IDUMP)
      implicit none
      double precision A,a0,a1,ai,ar,B,Big,blow,Crit,Dumscr,E,Four,gmin1
     &,One,Onept5,Pt5,reduct,S,Small,TAU
      double precision Three,Two,Zero
      integer i,Idscr,IDUMP,Ieval,Ievals,Ifill,Igeno,ii,In,Iob,Ioc0,iocc
     &,Iocs,Iod,Iof1p,Iofa,Ione,Ioq,Ior,Ios
      integer Iou,Iouab,Ious,Iout,Ipspin,Iptot,Ipunch,ircmax,ISW,Itcnt,I
     &uo,j,jj,jjmdim,Jmat,k,Ksm,Kspin,Ksw,MDIM
      integer minprt,NBASIS,Nesk,Nest,Nest1,nocc,Nse,Nsep
      logical Key,kej,Cmp,Rhf,Skpsym
      dimension A(MDIM,*),B(MDIM,*)
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/scr/S(140),Iou(70),Iuo(70),Ior(70,2),E(140),Key,Itcnt,Crit,
     &Skpsym,Idscr,Dumscr(5209),Ifill
      common/rwf503/Igeno,Ieval,Ios,Ione,Iofa(4),Iod(4),Iob(4),Iocs(4),I
     &oc0(4),Ioq(4),Ious(4),Jmat(4),Iof1p(4),Ievals,Iouab(4),Iptot(2),Ip
     &spin(2)
      common/io/In,Iout,Ipunch
      data ircmax,reduct,blow/20,.5D0,.75D0/
      
      
      
      
99001 format(' CONTINUITY OF WAVEFUNCTION CAN NOT BE ACHIEVED')
99002 format(72x,'FURTHER REDUCTION OF TAU MEANINGLESS. KEEP IT.')
      call ilsw(2,21,minprt)
      if(ISW.EQ.2)then
      
      
      call tioc(NBASIS,2,Ious,A,4,1,IDUMP)
      do 50 i=1,NBASIS
      Ior(i,Kspin)=0
50    continue
      Key=.FALSE.
      iocc=0
      nocc=Nse
      do 100 i=1,NBASIS
      a0=Zero
      do 60 j=1,Nse
      call dot(NBASIS,A(1,j),B(1,i),ar,ai)
      a0=a0+ar**2+ai**2
60    continue
      if(a0.LT.blow)then
      
      nocc=nocc+1
      Ior(nocc,Kspin)=i
      else
      iocc=iocc+1
      Crit=gmin1(Crit,a0)
      if(iocc.GT.Nse)goto 400
      Ior(iocc,Kspin)=i
      endif
100   continue
      if(iocc.GE.Nse)return
      elseif(ISW.EQ.3)then
      
      
      call tioc(NBASIS,2,Ious,A,4,1,IDUMP)
      Key=.FALSE.
      do 150 i=1,NBASIS
      Ior(i,Kspin)=0
150   continue
      do 200 i=1,NBASIS
      a0=Zero
      do 160 j=1,NBASIS
      call dot(NBASIS,A(1,i),B(1,j),ar,ai)
      a1=ar**2+ai**2
      if(a1.GT.a0)then
      a0=a1
      jj=j
      endif
160   continue
      Crit=gmin1(Crit,a0)
      if(a0.LT.blow)goto 400
      jjmdim=jj+MDIM
      do 180 k=1,NBASIS
      if(Cmp)B(k,jjmdim)=Zero
      B(k,jj)=Zero
180   continue
      Ior(i,Kspin)=jj
200   continue
      return
      elseif(ISW.EQ.4)then
      goto 600
      else
      
      
      do 250 i=1,NBASIS
      do 220 j=1,NBASIS
      B(i,j)=Zero
220   continue
250   continue
      if(Cmp)then
      do 260 Kspin=1,Ksm
      call tioc(NBASIS,1,Ious(2*Kspin),B,0,0,IDUMP)
260   continue
      endif
      do 300 i=1,NBASIS
      B(i,i)=One
300   continue
      do 350 Kspin=1,Ksm
      call tioc(NBASIS,1,Ious(2*Kspin-1),B,0,0,IDUMP)
350   continue
      return
      endif
      
      
400   do 500 i=1,NBASIS
      if(Ior(i,Kspin).EQ.0)then
      do 420 j=1,NBASIS
      jj=j
      do 410 k=1,NBASIS
      if(j.EQ.Ior(k,Kspin))goto 420
410   continue
      goto 440
      
420   continue
      call lnk1e
440   Ior(i,Kspin)=jj
      endif
500   continue
      
      if(Itcnt.LE.ircmax)then
      TAU=TAU*reduct
      Key=.TRUE.
      kej=.TRUE.
      return
      
      elseif(minprt.NE.0)then
      
      write(Iout,99001)
      call lnk1e
      else
      if(kej)write(Iout,99002)
      kej=.FALSE.
      return
      endif
      
      
600   do 700 i=1,NBASIS
      S(i)=E(i+Nest)
700   continue
      do 800 i=1,NBASIS
      ii=Ior(i,Kspin)
      if(Cmp)then
      do 720 j=1,NBASIS
      B(j,i+MDIM)=A(j,ii+MDIM)
720   continue
      endif
      do 750 j=1,NBASIS
      B(j,i)=A(j,ii)
750   continue
      E(i+Nest)=S(ii)
800   continue
      call tioc(NBASIS,1,Ious,B,4,1,IDUMP)
      return
      
      end
C* :1 * 
      
