
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rotcor"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rotcor.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "rotcor.web"
      subroutine rotcor(NATOMS,C)
      implicit none
      double precision C,chi,conv,ct,f180,four,gatan,one,phi,pi,sum,t,th
     &eta,zero
      integer i,idx,In,Iout,Ipunch,j,k,NATOMS
      dimension C(*)
      dimension ct(100,3),t(3,3)
      common/io/In,Iout,Ipunch
      data f180/180.0D0/,one/1.0D0/,four/4.0D0/
      data zero/0.0D0/
      
      
      
      
      
      
      
99001 format(3E20.10)
99002 format(25H0THE ROTATION ANGLES ARE,/7H PHI   ,e20.13/7H THETA ,e20
     &.13/7H CHI   ,e20.13//)
99003 format(//22H0TRANSFORMATION MATRIX/)
99004 format(//24H0TRANSFORMED COORDINATES/)
      
      
      read(In,99001)phi,theta,chi
      write(Iout,99002)phi,theta,chi
      pi=four*gatan(one)
      conv=pi/f180
      phi=phi*conv
      theta=theta*conv
      chi=chi*conv
      call rotmat(phi,theta,chi,t)
      write(Iout,99003)
      call matout(t,3,3,3,3)
      do 100 i=1,NATOMS
      idx=3*(i-1)
      do 50 j=1,3
      sum=zero
      do 20 k=1,3
      sum=sum+t(j,k)*C(idx+k)
20    continue
      ct(i,j)=sum
50    continue
100   continue
      write(Iout,99004)
      call matout(ct,100,3,NATOMS,3)
      k=0
      do 200 i=1,NATOMS
      do 150 j=1,3
      k=k+1
      C(k)=ct(i,j)
150   continue
200   continue
      
      return
      
      end
C* :1 * 
      
