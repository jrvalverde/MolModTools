
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 spindm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "spindm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "spindm.web"
      subroutine spindm(NAE,NBE,NBASIS,SP,A,B,IDUMP)
      implicit none
      double precision A,amb,apb,B,Big,Four,gfloat,gsqrt,One,Onept5,pt25
     &,Pt5,Small,SP,Three,trace,Two,xs,Zero
      integer i,i1,i2,IDUMP,Ieval,Ievals,Igeno,In,Ioc,Ioc0,Iocs,Iod,Iof1
     &p,Iofa,Ione,Ioq,Ios,Iouab,Ious,Iout
      integer Ipspin,Iptot,Ipunch,j,Jmat,Ksm,Kspin,Ksw,Mdim,Mdsq,minprt,
     &Mshifs,Mtt,NAE,NBASIS,NBE,ne,Nesk,Nest,Nest1
      integer Nse,Nsep,Ntt
      logical Cmp,Rhf
      dimension A(*),B(*)
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/rwf503/Igeno,Ieval,Ios,Ione,Iofa(4),Iod(4),Ioc(4),Iocs(4),I
     &oc0(4),Ioq(4),Ious(4),Jmat(4),Iof1p(4),Ievals,Iouab(4),Iptot(2),Ip
     &spin(2)
      common/io/In,Iout,Ipunch
      data pt25/.25D0/
      
      
      
      
99001 format(20x,3HS= ,f10.4,20x,6HS**2= ,f10.4)
      call ilsw(2,21,minprt)
      Kspin=1
      ne=NAE+NBE
      
      call tioc(NBASIS,2,Ios,B,1,0,IDUMP)
      call tioc(NBASIS,2,Ioc,A,4,1,IDUMP)
      call matmul(NBASIS,A,B,1,2,0)
      call tioc(NBASIS,2,Ioc(3),B,4,1,IDUMP)
      call matmul(NBASIS,A,B,0,0,0)
      call tioc(NBASIS,1,Iouab,A,4,1,IDUMP)
      
      trace=Zero
      do 100 i=1,NBE
      i1=(i-1)*Mdim
      do 50 j=1,NAE
      i2=i1+j
      if(Cmp)trace=trace+A(i2+Mdsq)**2
      trace=trace+A(i2)**2
50    continue
100   continue
      apb=gfloat(ne)
      amb=gfloat(NAE-NBE)
      SP=Pt5*apb+pt25*amb**2-trace
      xs=-Pt5+Pt5*gsqrt(One+Four*SP)
      if(minprt.EQ.0)write(Iout,99001)xs,SP
      return
      
      end
C* :1 * 
      
