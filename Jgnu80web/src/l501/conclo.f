
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 conclo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "conclo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "conclo.web"
      subroutine conclo(N,ACURCY,FINAC,A,B,KOP,PREDUC)
      implicit none
      double precision A,ACURCY,B,cosphi,cospsi,dp1,dp2,dp3,FINAC,four,g
     &abs,gfloat,one,onept9,PREDUC,pt99,pt995,rmsdp,scftrc,sp11
      double precision sp12,sp13,sp22,sp23,sp33,x,xxx,xy,y,yyy
      integer i,Icount,Iflag,ifour,In,inone,Iout,Ipunch,Irwc,Irwc1,Irwc2
     &,Irwc3,Irweig,Irwf,Irwgen,Irwh,Irwibf,Irwlc,Irwle,Irwlp
      integer Irwp,Irwpt,Irws,Irwt,Irwtm,Irww,iskpd,ithree,itype,Key,KOP
     &,Length,Lenibf,loc1,loc2,N,Nsq,Ntt
      integer Psave
      dimension ithree(2),ifour(2),itype(2),iskpd(3)
      dimension A(100),B(100)
      common/ntt/Ntt,Length,Nsq
      common/irw501/Irwgen,Irws,Irwh,Irwt,Irweig,Irwc,Irwp,Irwpt,Irwf,Ir
     &wc1,Irwc2,Irwc3,Irwtm,Irwibf,Lenibf,Irwle,Irwlc,Irwlp,Irww
      common/psave/Psave
      common/convg/Icount,Iflag,Key
      common/io/In,Iout,Ipunch
      save
      data pt99/0.99D0/,pt995/0.995D0/,one/1.0D0/,onept9/1.9D0/
      data four/4.0D0/
      data inone/4H    /,ithree/4H3-PO,4HINT./
      data ifour/4H4-PO,4HINT./,iskpd/4H3PT ,4H4PT ,4HSKPD/
      
      
      
      
      
      
99001 format(1x,30x,d11.4)
99002 format(1x,48x,2A4)
      
      Iflag=0
      Icount=Icount+1
      itype(1)=inone
      itype(2)=inone
      loc1=Irwc3
      if(mod(Icount,2).NE.0)loc1=Irwc2
      loc2=(Irwc2+Irwc3)-loc1
      if(Icount.NE.1)then
      call tread(Irwc1,B,Ntt,1,Ntt,1,0)
      call twrite(Irwc1,A,Ntt,1,Ntt,1,0)
      do 50 i=1,Ntt
      B(i)=A(i)-B(i)
50    continue
      sp11=scftrc(B,B,N,1)
      dp1=sqrt(sp11)
      rmsdp=dp1/gfloat(N)
      FINAC=rmsdp
      if(rmsdp.LT.ACURCY)then
      Key=0
      
      elseif(Icount.NE.2)then
      
      if(Icount.GE.4)then
      call tread(loc1,A,Ntt,1,Ntt,1,0)
      sp23=sp12
      sp33=sp22
      sp13=scftrc(A,B,N,1)
      dp3=sqrt(sp33)
      endif
      call twrite(loc1,B,Ntt,1,Ntt,1,0)
      call tread(loc2,A,Ntt,1,Ntt,1,0)
      sp12=scftrc(A,B,N,1)
      sp22=scftrc(A,A,N,1)
      dp2=sqrt(sp22)
      call tread(Irwc1,A,Ntt,1,Ntt,1,0)
      cosphi=sp12/(dp1*dp2)
      if(Icount.NE.3)then
      x=(sp13*sp22-sp12*sp23)/(sp11*sp22-sp12*sp12)
      y=(sp23*sp11-sp12*sp13)/(sp11*sp22-sp12*sp12)
      cospsi=sqrt(x*x*sp11+y*y*sp22+(x+x)*y*sp12)/dp3
      if(Psave.EQ.0)write(Iout,99001)rmsdp
      if(cospsi.GT.pt99)then
      y=-y/x
      x=one/x
      xy=y*y+four*x
      if(xy.GE.0)then
      xy=gabs(y)+sqrt(xy)
      if(xy.LE.onept9)then
      
      if(KOP.LE.1)then
      
      xxx=x/(one-x-y)
      yyy=(x+y)/(one-x-y)
      do 52 i=1,Ntt
      A(i)=A(i)+yyy*B(i)
52    continue
      call tread(loc2,B,Ntt,1,Ntt,1,0)
      do 54 i=1,Ntt
      A(i)=A(i)+xxx*B(i)
54    continue
      itype(1)=ifour(1)
      itype(2)=ifour(2)
      Iflag=1
      Icount=0
      call twrite(Irwc1,A,Ntt,1,Ntt,1,0)
      if(Psave.EQ.0)write(Iout,99002)itype(1),itype(2)
      else
      itype(1)=iskpd(2)
      itype(2)=iskpd(3)
      if(Psave.EQ.0)write(Iout,99002)itype(1),itype(2)
      endif
      goto 100
      endif
      endif
      if(gabs(cosphi).GT.pt995)then
      if(KOP.LE.0)then
      
      x=dp1/(dp2*cosphi-dp1)
      do 56 i=1,Ntt
      A(i)=A(i)+x*B(i)
56    continue
      itype(1)=ithree(1)
      itype(2)=ithree(2)
      Iflag=1
      Icount=0
      call twrite(Irwc1,A,Ntt,1,Ntt,1,0)
      if(Psave.EQ.0)write(Iout,99002)itype(1),itype(2)
      else
      itype(1)=iskpd(1)
      itype(2)=iskpd(3)
      if(Psave.EQ.0)write(Iout,99002)itype(1),itype(2)
      endif
      endif
      endif
      else
      if(Psave.EQ.0)write(Iout,99001)rmsdp
      endif
      else
      call twrite(loc1,B,Ntt,1,Ntt,1,0)
      if(Psave.EQ.0)write(Iout,99001)rmsdp
      endif
      else
      call twrite(Irwc1,A,Ntt,1,Ntt,1,0)
      if(Psave.EQ.0)write(Iout,99002)itype(1),itype(2)
      endif
100   return
      
      end
C* :1 * 
      
