
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ofix"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ofix.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "ofix.web"
      subroutine ofix(NBASIS,IERR,IDUMP)
      implicit none
      double precision A,a0,Aa,B,Bb,Big,Dumscr,Fillab,Four,gabs,gsqrt,On
     &e,Onept5,Pt5,Scr1,Scr2,Scr3,Small,Three,thresh
      double precision Two,Zero
      integer i,i1,IDUMP,IERR,Ieval,Ievals,Igeno,In,Ioc,Ioc0,Iocs,Iod,Io
     &f1p,Iofa,Ione,Ioq,Ios,Iouab,Ious,Iout
      integer Ipspin,Iptot,Ipunch,itried,j,Jmat,k,Ksm,Kspin,Ksw,Mdim,Mds
     &q,Mshifs,Mtt,NBASIS,Nes,Nesk,Nest,nn,nnj
      integer Nse,Nsep,Ntt
      logical Cmp,Rhf
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nes
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/memry/A(4900),Aa(70),B(4900),Bb(70),Fillab(40060)
      common/scr/Scr1(140),Scr2(140),Scr3(140),Dumscr(5212)
      common/rwf503/Igeno,Ieval,Ios,Ione,Iofa(4),Iod(4),Ioc(4),Iocs(4),I
     &oc0(4),Ioq(4),Ious(4),Jmat(4),Iof1p(4),Ievals,Iouab(4),Iptot(2),Ip
     &spin(2)
      common/io/In,Iout,Ipunch
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      data thresh/1.0D-9/
      
      
      
99001 format(29H ORTHONORMALIZATION REQUIRED.)
99002 format(26H UNABLE TO ORTHONORMALIZE.)
99003 format(34H C(TRANSPOSED) * S * C.  SPINSPACE,i2)
      IERR=0
      itried=0
      do 300 Kspin=1,Ksm
50    call tioc(NBASIS,2,Ios,A,1,0,IDUMP)
      call tioc(NBASIS,2,Ioc,B,4,1,IDUMP)
      
      call matmul(NBASIS,A,B,2,0,0)
      call matmul(NBASIS,B,A,1,0,0)
      
      do 100 i=1,NBASIS
      k=(i-1)*Mdim+i
      a0=gabs(One-B(k))
      if(a0.GT.thresh)goto 200
100   continue
      do 150 i=2,NBASIS
      i1=i-1
      nn=i1*Mdim
      do 120 j=1,i1
      if(.NOT.Cmp)a0=gabs(B(nn+j))
      if(Cmp)a0=gsqrt(B(nn+j)**2+B(nn+j+Mdsq)**2)
      if(a0.GT.thresh)goto 200
      
120   continue
150   continue
      itried=0
      goto 300
      
      
200   if(itried.GT.0)then
      
      write(Iout,99002)
      write(Iout,99003)Kspin
      call matout(B,Mdim,Mdim,NBASIS,NBASIS)
      IERR=1
      else
      write(Iout,99001)
      if(IDUMP.NE.0)then
      call cmat(B,Mdim,Mdim,NBASIS,NBASIS,Cmp)
      call matout(B,Mdim,Mdim,NBASIS,NBASIS)
      call matout(B(1+Mdsq),Mdim,Mdim,NBASIS,NBASIS)
      endif
      
      call sls(1,B,Mdim,NBASIS)
      call diagd(B,A,Aa,NBASIS,Scr1,Scr2,Mdim,Cmp)
      
      do 220 i=1,NBASIS
      a0=One/gsqrt(Aa(i))
      nn=Mdim*(i-1)
      do 210 j=1,NBASIS
      nnj=nn+j
      B(nnj)=A(nnj)*a0
      if(Cmp)B(nnj+Mdsq)=A(nnj+Mdsq)*a0
210   continue
220   continue
      call matmul(NBASIS,B,A,0,1,0)
      
      call tioc(NBASIS,2,Ioc,A,4,1,IDUMP)
      call matmul(NBASIS,A,B,0,0,0)
      call tioc(NBASIS,1,Ioc,A,4,1,IDUMP)
      itried=1
      goto 50
      endif
300   continue
      return
      
      end
C* :1 * 
      
