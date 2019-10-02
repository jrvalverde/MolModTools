
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repdst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repdst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "repdst.web"
      subroutine repdst(NATOMS)
      implicit none
      double precision angle,f180,four,gatan,gcos,gsin,one,pi,ten,three,
     &two,zero
      integer i,Icnt,Isave,NATOMS,Nsave
      double precision dsttbl(3,8)
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      common/reploc/Nsave,Icnt,Isave(15)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data dsttbl/0.0,1.0,1.0,0.0,1.0,-1.0,0.0,0.0,2.0,0.0,0.0,-2.0,0.0,
     &0.0,2.0,0.0,0.0,-2.0,0.0,0.0,2.0,0.0,0.0,-2.0/
      data four/4.0D0/,one/1.0D0/,f180/180.0D0/,three/3.0D0/
      data ten/10.0D0/,two/2.0D0/,zero/0.0D0/
      
      
      pi=four*gatan(one)
      angle=ten*pi/f180
      dsttbl(2,3)=two*gcos(angle)
      dsttbl(2,4)=two*gcos(angle)
      dsttbl(2,5)=two*gcos(two*angle)
      dsttbl(2,6)=two*gcos(two*angle)
      dsttbl(2,7)=two*gcos(three*angle)
      dsttbl(2,8)=two*gcos(three*angle)
      
      Icnt=0
      Nsave=1
      Nreps=8
      Nsymop=2
      Isave(1)=1
      
      Symops(1,2)=gcos(angle)
      Symops(2,2)=gsin(angle)
      Symops(3,2)=zero
      Symops(4,2)=-gsin(angle)
      Symops(5,2)=gcos(angle)
      Symops(6,2)=zero
      Symops(7,2)=zero
      Symops(8,2)=zero
      Symops(9,2)=one
      
      do 100 i=1,NATOMS
      Iprmut(i,2)=i
100   continue
      
      do 200 i=1,8
      Chrtbl(2,i)=dsttbl(2,i)
      Chrtbl(1,i)=dsttbl(3,i)
200   continue
      call labrep('SGG,SGU,PIG,PIU,DLTG,DLTU,PHIG,PHIU,',8)
      return
      
      end
C* :1 * 
      
