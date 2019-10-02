
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 conuso"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "conuso.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "conuso.web"
      subroutine conuso(NBASIS,IEXP,KEY,IOPER,IDUMP)
      implicit none
      double precision A,a0,a1,a2,a3,Acurcy,B,Big,cosphi,cospsi,den,Deri
     &v,Energy,Fillab,Four,gabs,gfloat,gsqrt,One,Onept5
      double precision onept9,Pt5,pt99,Rms,Small,t11,t12,t13,t22,t23,t33
     &,Three,tracab,Two,xy,Zero
      integer i,Icount,IDUMP,Ieval,Ievals,IEXP,Igeno,In,Ioc,Ioc0,Iocs,Io
     &d,Iof1p,Iofa,Ione,IOPER,Ioq,Ios,Iouab,Ious
      integer Iout,Ipscr,Ipspin,Iptot,Ipunch,Jmat,KEY,Ksm,Kspin,Ksw,Maxc
     &yc,Mdim,Mdsq,minprt,Mshifs,Mtt,NBASIS,Nes,Nesk,Nest
      integer Nse,Nsep,Ntt
      logical Cmp,Rhf,Class,Extrap
      integer D1,D2,D3
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nes
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/memry/A(4970),B(4970),Fillab(40060)
      common/io/In,Iout,Ipunch
      common/rwf503/Igeno,Ieval,Ios,Ione,Iofa(4),Iod(4),Ioc(4),Iocs(4),I
     &oc0(4),Ioq(4),Ious(4),Jmat(4),Iof1p(4),Ievals,Iouab(4),Iptot(2),Ip
     &spin(2)
      common/cut/Energy,Deriv,Acurcy,Rms,Maxcyc,Icount,Class,Extrap
      common/pexscr/Ipscr(4)
      equivalence(D1,Iof1p(1)),(D2,Jmat(1)),(D3,Ious(1))
      data pt99,onept9/.99D0,1.9D0/
      
      
      
      
99001 format(32x,d11.4)
99002 format(' ITERATION TERMINATED: DENSITY MATRIX CONVERGES')
99003 format(45x,'4-POINT.')
99004 format(45x,'3-POINT.')
      call ilsw(2,21,minprt)
      if(IOPER.EQ.2)then
      
      
      
      do 50 Kspin=1,Ksm
      call tioc(NBASIS,2,Iod,A,2,1,IDUMP)
      call tioc(NBASIS,2,Ipscr,B,2,1,IDUMP)
      call tioc(NBASIS,1,Ipscr,A,2,1,IDUMP)
      do 20 i=1,Ntt
      if(Cmp)A(i+Mtt)=A(i+Mtt)-B(i+Mtt)
      A(i)=A(i)-B(i)
20    continue
      call tioc(NBASIS,1,D1,A,2,1,IDUMP)
50    continue
      t11=tracab(NBASIS,D1,D1)
      Rms=gsqrt(t11)/gfloat(NBASIS)
      if(minprt.EQ.0)write(Iout,99001)Rms
      KEY=1
      if(Rms.GT.Acurcy.OR.Extrap)then
      
      
      Extrap=.FALSE.
      if(.NOT.Class)return
      Icount=Icount+1
      if(IEXP.GE.2)return
      if(Icount.NE.1)then
      t12=tracab(NBASIS,D1,D2)
      if(Icount.NE.2)then
      t12=tracab(NBASIS,D1,D2)
      t13=tracab(NBASIS,D1,D3)
      if(Icount.GT.3)then
      cosphi=t12/gsqrt(t11*t22)
      
      den=t11*t22-t12**2
      a0=(t13*t22-t12*t23)/den
      a1=(t23*t11-t12*t13)/den
      cospsi=gsqrt((t11*a0**2+t12*Two*a0*a1+t22*a1**2)/t33)
      if(cospsi.GT.pt99)then
      a1=-a1/a0
      a0=One/a0
      xy=a1**2+Four*a0
      if(xy.GE.Zero)then
      xy=gabs(a1)+gsqrt(xy)
      if(xy.LE.onept9)then
      
      if(minprt.EQ.0)write(Iout,99003)
      den=One-a0-a1
      a3=a0/den
      a2=(a0+a1)/den
      do 56 Kspin=1,Ksm
      call tioc(NBASIS,2,Iod,A,2,1,IDUMP)
      call tioc(NBASIS,2,D1,B,2,1,IDUMP)
      do 52 i=1,Ntt
      if(Cmp)A(i+Mtt)=A(i+Mtt)+a2*B(i+Mtt)
      A(i)=A(i)+a2*B(i)
52    continue
      call tioc(NBASIS,2,D2,B,2,1,IDUMP)
      do 54 i=1,Ntt
      if(Cmp)A(i+Mtt)=A(i+Mtt)+a3*B(i+Mtt)
      A(i)=A(i)+a3*B(i)
54    continue
      call tioc(NBASIS,1,Iod,A,2,1,IDUMP)
56    continue
      Extrap=.TRUE.
      Icount=0
      endif
      endif
      endif
      endif
      endif
      
      do 60 Kspin=1,Ksm
      call tioc(NBASIS,2,D2,A,2,1,IDUMP)
      call tioc(NBASIS,1,D3,A,2,1,IDUMP)
60    continue
      t33=t22
      endif
      else
      KEY=0
      if(minprt.EQ.0)write(Iout,99002)
      return
      endif
      else
      
      
      KEY=1
      do 100 Kspin=1,Ksm
      call tioc(NBASIS,2,Iod,A,2,1,IDUMP)
      call tioc(NBASIS,1,Ipscr,A,2,1,IDUMP)
100   continue
      return
      endif
      
      do 200 Kspin=1,Ksm
      call tioc(NBASIS,2,D1,A,2,1,IDUMP)
      call tioc(NBASIS,1,D2,A,2,1,IDUMP)
200   continue
      t22=t11
      t23=t12
      return
      
      end
C* :1 * 
      
