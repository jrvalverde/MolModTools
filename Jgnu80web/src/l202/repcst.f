
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 repcst"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "repcst.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "repcst.web"
      subroutine repcst(NATOMS)
      implicit none
      double precision angle,f180,four,gatan,gcos,gsin,one,pi,ten,three,
     &two,zero
      integer i,Icnt,Isave,NATOMS,Nsave
      double precision csttbl(2,4)
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      common/reploc/Nsave,Icnt,Isave(15)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data csttbl/0.0,1.0,0.0,9999.0,0.0,9999.0,0.0,9999.0/
      data four/4.0D0/,one/1.0D0/,f180/180.0D0/,three/3.0D0/
      data ten/10.0D0/,two/2.0D0/,zero/0.0D0/
      
      pi=four*gatan(one)
      angle=ten*pi/f180
      csttbl(2,2)=two*gcos(angle)
      csttbl(2,3)=two*gcos(two*angle)
      csttbl(2,4)=two*gcos(three*angle)
      
      Icnt=0
      Nsave=0
      Nreps=4
      Nsymop=1
      
      Symops(1,1)=gcos(angle)
      Symops(2,1)=gsin(angle)
      Symops(3,1)=zero
      Symops(4,1)=-gsin(angle)
      Symops(5,1)=gcos(angle)
      Symops(6,1)=zero
      Symops(7,1)=zero
      Symops(8,1)=zero
      Symops(9,1)=one
      
      do 100 i=1,NATOMS
      Iprmut(i,1)=i
100   continue
      
      do 200 i=1,4
      Chrtbl(1,i)=csttbl(2,i)
200   continue
      call labrep('SG,PI,DLTA,PHI,',4)
      return
      
      end
C* :1 * 
      
