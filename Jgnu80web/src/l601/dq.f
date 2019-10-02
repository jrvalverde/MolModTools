
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dq"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dq.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "dq.web"
      subroutine dq(NATOMS,NBASIS,ATMCHG,ICHARG,C,MDIM,A,B,TOE,TOANG)
      implicit none
      double precision A,an,ATMCHG,B,C,d,debye,dtotal,dx,dy,dz,Gen,gsqrt
     &,tenten,TOANG,TOE,zero
      integer i,ICHARG,Idum,idx,Ifoorc,In,Iolbl,Iout,Ipunch,Irwca,Irwcb,
     &Irwdip,Irweig,Irwgen,Irwpa,Irwpb,Irwpt,Irws,Irwsc1,Irwscr
      integer Irwx,Irwy,Irwz,j,k,len,MDIM,NATOMS,NBASIS
      dimension A(MDIM,MDIM),B(MDIM,MDIM),Irwdip(3),d(3)
      dimension ATMCHG(*),C(*)
      common/io/In,Iout,Ipunch
      common/iopt/Ifoorc,Idum(9)
      common/irw601/Irweig,Irwca,Irwcb,Irwpa,Irwpb,Irws,Iolbl,Irwx,Irwy,
     &Irwz,Irwpt,Irwgen,Irwscr,Irwsc1
      common/gen/Gen(47)
      equivalence(dx,d(1)),(dy,d(2)),(dz,d(3)),(Irwx,Irwdip(1))
      data zero/0.0D0/,tenten/1.D-10/
      
      
      
      
      
      
      
      
      
99001 format(1x,'DIPOLE MOMENT (DEBYE): X=',f7.4,3x,'Y=',f7.4,3x,'Z=',f7
     &.4,3x,'TOTAL=',f7.4)
      
      call tquery(Irwx,len)
      if(len.EQ.0)return
      if(ICHARG.EQ.0)then
      debye=TOANG*TOE/tenten
      call tread(Irwpa,A,MDIM,MDIM,NBASIS,NBASIS,1)
      if(Ifoorc.GT.0)then
      call tread(Irwpb,B,MDIM,MDIM,NBASIS,NBASIS,1)
      do 20 i=1,NBASIS
      do 10 j=1,NBASIS
      A(i,j)=A(i,j)+B(i,j)
10    continue
20    continue
      endif
      
      do 100 k=1,3
      d(k)=zero
      call tread(Irwdip(k),B,MDIM,MDIM,NBASIS,NBASIS,1)
      do 40 i=1,NBASIS
      do 30 j=1,NBASIS
      d(k)=d(k)-A(i,j)*B(i,j)
30    continue
40    continue
      do 60 i=1,NATOMS
      an=ATMCHG(i)
      idx=3*(i-1)
      d(k)=d(k)+C(idx+k)*an
60    continue
100   continue
      dx=d(1)*debye
      dy=d(2)*debye
      dz=d(3)*debye
      dtotal=gsqrt(dx*dx+dy*dy+dz*dz)
      write(Iout,99001)dx,dy,dz,dtotal
      
      call tread(Irwgen,Gen,47,1,47,1,0)
      Gen(22)=dtotal
      call twrite(Irwgen,Gen,47,1,47,1,0)
      endif
      
      return
      
      end
C* :1 * 
      
