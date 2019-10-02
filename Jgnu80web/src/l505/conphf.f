
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 conphf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "conphf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "conphf.web"
      subroutine conphf(N,KEY,ACURCY,INSIDE,IFLAG)
      implicit none
      double precision A1,A2,A3,ACURCY,cosphi,cospsi,dp1,dp2,dp3,Filabc,
     &four,gabs,gsqrt,one,onept9,pt99,pt995,rmsdpi,sp11,sp12
      double precision sp13,sp22,sp23,sp33,traphf,Trmsdp,two,x,xxx,xy,y,
     &yyy,zero
      integer i,i1,i2,Ia,Icnvg,Icount,Icyc,Idumifill,Idump,Iext,Ifcnvg,I
     &FLAG,Iguess,In,INSIDE,Iout,Ipch,Iprint,Ipunch,Irstrt
      integer Irwc1,Irwc2,Irwc3,Irwc4,Irwca,Irwcb,Irwev,Irwfa,Irwfb,Irwg
     &en,Irwh,Irwpa,Irwpb,Irwpt,Irws,Irwt,KEY,loc1,loc2,Maxnbf
      integer Maxntt,N,nam3,nam4,Ntt,Ntt2,nttm2
      integer Psave
      dimension nam3(2),nam4(2)
      common/memry/A3(4970),A2(4970),A1(4970),Idumifill,Filabc(35089)
      common/psave/Psave
      common/ops505/Ipch,Iprint,Idump,Iguess,Icnvg,Icyc,Irstrt,Iext,Ifcn
     &vg
      common/irw505/Irwgen,Irws,Irwt,Irwh,Irwev,Irwca,Irwcb,Irwpa,Irwpb,
     &Irwpt,Irwfa,Irwfb,Irwc1,Irwc2,Irwc3,Irwc4
      common/max505/Maxnbf,Maxntt
      common/rhfcvg/Ntt2,Icount,Trmsdp
      common/io/In,Iout,Ipunch
      common/jnkphf/Ntt,Ia(71)
      data pt99/0.99D0/,pt995/0.995D0/,one/1.0D0/,onept9/1.90D0/
      data two/2.0D0/,four/4.0D0/,zero/0.0D0/
      data nam3/4H3-PO,4HINT./,nam4/4H4-PO,4HINT./
      
      
      
      
      
      
99001 format(' ',30x,d11.4)
99002 format(' ',48x,2A4)
      
      
      KEY=1
      
      IFLAG=0
      nttm2=2*Maxntt
      
      do 100 i=1,Ntt
      A3(Ntt+i)=A3(Maxntt+i)
100   continue
      call tread(Irwc1,A1,nttm2,1,Ntt2,1,0)
      do 200 i=1,Ntt2
      A2(i)=A3(i)-A1(i)
200   continue
      call twrite(Irwc1,A3,nttm2,1,Ntt2,1,0)
      sp11=traphf(A2,A2,N)
      rmsdpi=gsqrt(sp11/two)/N
      Trmsdp=Trmsdp+rmsdpi
      if(INSIDE.LE.2)then
      
      return
      else
      Icount=Icount+1
      if(Icount.NE.1)then
      if(Trmsdp.LT.gabs(ACURCY))then
      KEY=0
      ACURCY=Trmsdp
      else
      
      loc1=Irwc4
      if(Icount.NE.(Icount/2)*2)loc1=Irwc3
      loc2=(Irwc3+Irwc4)-loc1
      call tread(Irwc2,A1,nttm2,1,Ntt2,1,0)
      do 210 i=1,Ntt2
      A2(i)=A3(i)-A1(i)
210   continue
      sp11=traphf(A2,A2,N)
      dp1=gsqrt(sp11/two)
      if(Icount.NE.2)then
      
      if(Icount.GE.4)then
      call tread(loc1,A1,nttm2,1,Ntt2,1,0)
      sp23=sp12
      sp33=sp22
      sp13=traphf(A1,A2,N)
      dp3=gsqrt(sp33/two)
      endif
      call tread(loc2,A1,nttm2,1,Ntt2,1,0)
      sp12=traphf(A1,A2,N)
      sp22=traphf(A1,A1,N)
      dp2=gsqrt(sp22/two)
      cosphi=sp12/(two*dp1*dp2)
      if(Icount.NE.3)then
      x=(sp13*sp22-sp12*sp23)/(sp11*sp22-sp12*sp12)
      y=(sp23*sp11-sp12*sp13)/(sp11*sp22-sp12*sp12)
      cospsi=gsqrt((x*x*sp11+y*y*sp22+two*x*y*sp12)/two)/dp3
      if(Psave.EQ.0)write(Iout,99001)Trmsdp
      if(Iext.NE.2)then
      if(cospsi.GT.pt99)then
      y=-y/x
      x=one/x
      xy=y*y+four*x
      if(xy.GE.0)then
      xy=gabs(y)+gsqrt(xy)
      if(xy.LE.onept9)then
      
      if(Iext.LT.2)then
      xxx=x/(one-x-y)
      yyy=(x+y)/(one-x-y)
      do 212 i=1,Ntt2
      A3(i)=A3(i)+xxx*A1(i)+yyy*A2(i)
212   continue
      if(Psave.EQ.0)write(Iout,99002)nam4
      IFLAG=1
      Icount=0
      endif
      goto 220
      endif
      endif
      if(Iext.LT.1)then
      if(gabs(cosphi).GT.pt995)then
      x=dp1/(dp2*cosphi-dp1)
      do 214 i=1,Ntt2
      A3(i)=A3(i)+x*A2(i)
214   continue
      if(Psave.EQ.0)write(Iout,99002)nam3
      IFLAG=1
      Icount=0
      endif
      endif
      endif
      endif
      else
      if(Psave.EQ.0)write(Iout,99001)Trmsdp
      endif
      else
      if(Psave.EQ.0)write(Iout,99001)Trmsdp
      endif
220   do 230 i=1,Ntt2
      A1(i)=A2(i)
230   continue
      call twrite(loc1,A1,nttm2,1,Ntt2,1,0)
      endif
      endif
      endif
      do 300 i=1,Ntt2
      A1(i)=A3(i)
300   continue
      call twrite(Irwc2,A1,nttm2,1,Ntt2,1,0)
      do 400 i=1,Ntt
      i1=(Maxntt+1)+Ntt-i
      i2=Ntt2+1-i
      A3(i1)=A3(i2)
400   continue
      Trmsdp=zero
      return
      
      
      
      end
C* :1 * 
      
