
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 conopn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "conopn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "conopn.web"
      subroutine conopn(N,KEY,ACURCY,IFLAG,IEXTP)
      implicit none
      double precision A1,A2,A3,ACURCY,cosphi,cospsi,Crit,dp1,dp2,dp3,Fi
     &labc,four,gabs,gfloat,gsqrt,one,onept9,pt99,pt995,rmsdp
      double precision sp11,sp12,sp13,sp22,sp23,sp33,traopn,two,x,xxx,xy
     &,y,yyy
      integer i,Icount,IEXTP,IFLAG,ifour,Ij,In,inone,Iout,Ipunch,Irwc1,I
     &rwc2,Irwc3,Irwca,Irwcb,Irweig,Irwfa,Irwfb,Irwgen,Irwh
      integer Irwibf,Irwpa,Irwpb,Irwps,Irwpt,Irws,Irwt,Irwtm,Irwur,iskpd
     &,ithree,itype,KEY,Lenibf,loc1,loc2,Maxnbf,N,nq,Ntt
      integer Ntt2,Nttmax
      integer Psave
      dimension ithree(2),ifour(2),itype(2),iskpd(3)
      common/psave/Psave
      common/memry/A3(5700),A2(5700),A1(5700),Filabc(32900)
      common/io/In,Iout,Ipunch
      common/ind/Ntt,Ij(127)
      common/max502/Maxnbf,Nttmax
      common/icount/Crit,Icount,Ntt2
      common/irw502/Irwgen,Irweig,Irwca,Irwcb,Irwpa,Irwpb,Irwpt,Irwps,Ir
     &wfa,Irwfb,Irwur,Irws,Irwh,Irwt,Irwtm,Irwc1,Irwc2,Irwc3,Irwibf,Leni
     &bf
      data pt99/0.99D0/,pt995/0.995D0/,one/1.0D0/,onept9/1.9D0/
      data two/2.0D0/,four/4.0D0/
      data inone/4H    /,ithree/4H3-PO,4HINT./
      data ifour/4H4-PO,4HINT./
      
      
      
      
      
      
      
99001 format(' ',30x,d11.4)
99002 format(' ',48x,2A4)
      
      itype(1)=inone
      itype(2)=inone
      KEY=1
      nq=Maxnbf*Maxnbf+Maxnbf
      IFLAG=0
      Icount=Icount+1
      loc1=Irwc3
      if(Icount.NE.(Icount/2)*2)loc1=Irwc2
      loc2=(Irwc2+Irwc3)-loc1
      do 100 i=1,Ntt
      A3(Ntt+i)=A3(Nttmax+i)
100   continue
      if(Icount.NE.1)then
      call tread(Irwc1,A1,nq,1,Ntt2,1,0)
      do 150 i=1,Ntt2
      A2(i)=A3(i)-A1(i)
150   continue
      sp11=traopn(A2,A2,N)
      dp1=gsqrt(sp11/two)
      rmsdp=dp1/gfloat(N)
      ACURCY=rmsdp
      if(rmsdp.LT.Crit)then
      KEY=0
      else
      
      if(Icount.NE.2)then
      
      if(Icount.GE.4)then
      call tread(loc1,A1,nq,1,Ntt2,1,0)
      sp23=sp12
      sp33=sp22
      sp13=traopn(A1,A2,N)
      dp3=gsqrt(sp33/two)
      endif
      call tread(loc2,A1,nq,1,Ntt2,1,0)
      sp12=traopn(A1,A2,N)
      sp22=traopn(A1,A1,N)
      dp2=gsqrt(sp22/two)
      cosphi=sp12/(two*dp1*dp2)
      if(Icount.NE.3)then
      x=(sp13*sp22-sp12*sp23)/(sp11*sp22-sp12*sp12)
      y=(sp23*sp11-sp12*sp13)/(sp11*sp22-sp12*sp12)
      cospsi=gsqrt((x*x*sp11+y*y*sp22+two*x*y*sp12)/two)/dp3
      if(Psave.EQ.0)write(Iout,99001)rmsdp
      if(IEXTP.LT.2)then
      if(cospsi.GT.pt99)then
      y=-y/x
      x=one/x
      xy=y*y+four*x
      if(xy.GE.0)then
      xy=gabs(y)+gsqrt(xy)
      if(xy.LE.onept9)then
      if(IEXTP.LT.2)then
      xxx=x/(one-x-y)
      yyy=(x+y)/(one-x-y)
      do 152 i=1,Ntt2
      A3(i)=A3(i)+xxx*A1(i)+yyy*A2(i)
152   continue
      itype(1)=ifour(1)
      itype(2)=ifour(2)
      IFLAG=1
      Icount=0
      endif
      goto 160
      endif
      endif
      if(IEXTP.LT.1)then
      if(gabs(cosphi).GT.pt995)then
      x=dp1/(dp2*cosphi-dp1)
      do 154 i=1,Ntt2
      A3(i)=A3(i)+x*A2(i)
154   continue
      itype(1)=ithree(1)
      itype(2)=ithree(2)
      IFLAG=1
      Icount=0
      endif
      endif
      endif
      endif
      else
      if(Psave.EQ.0)write(Iout,99001)rmsdp
      endif
      else
      if(Psave.EQ.0)write(Iout,99001)rmsdp
      endif
160   call twrite(loc1,A2,nq,1,Ntt2,1,0)
      endif
      endif
      call twrite(Irwc1,A3,nq,1,Ntt2,1,0)
      do 200 i=1,Ntt
      A3(Nttmax+1+Ntt-i)=A3(Ntt2-i+1)
200   continue
      if(Psave.EQ.0)write(Iout,99002)itype(1),itype(2)
      return
      
      end
C* :1 * 
      
