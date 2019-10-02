
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 vibtbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "vibtbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "vibtbl.web"
      subroutine vibtbl(A,B,TABLE,AA,LEN,NCOL,AMASS)
      implicit none
      integer i,id,iop,j,LEN,mapdgn,mapper,maprot,n,natoms,NCOL,ndgn,nos
     &ym
      real sum
      double precision TABLE(NCOL,3),A(LEN,NCOL),B(LEN,NCOL)
      double precision Symops,Chrtbl,AMASS(*),AA(*),zero
      integer Nsymop,Nreps,Lblrep,Iprmut
      dimension maprot(100),mapper(127),mapdgn(100)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data zero/0.0D0/
      
      ndgn=0
      n=1
      natoms=LEN/3
      do 100 i=1,natoms
      mapper(i)=n
      n=n+3
      maprot(i)=3
100   continue
      
      do 300 iop=1,Nsymop
      
      do 150 i=1,NCOL
      do 120 j=1,LEN
      B(j,i)=A(j,i)
120   continue
150   continue
      call dorot(Symops(1,iop),0,0,B,LEN,NCOL,maprot,natoms)
      call permut(B,AA,mapper,Iprmut(1,iop),natoms,LEN,NCOL)
      
      call vibovl(A,B,AA,LEN,NCOL,AMASS)
      
      call dgnmap(B,LEN,NCOL,mapdgn,ndgn,nosym)
      
      do 200 i=1,NCOL
      TABLE(i,iop)=B(i,i)
200   continue
300   continue
      
      
      if(ndgn.EQ.0)return
      do 400 id=1,ndgn
      do 350 iop=1,Nsymop
      sum=zero
      do 320 i=1,NCOL
      if(mapdgn(i).EQ.id)sum=sum+TABLE(i,iop)
320   continue
      do 340 i=1,NCOL
      if(mapdgn(i).EQ.id)TABLE(i,iop)=sum
340   continue
350   continue
400   continue
      return
      
      end
C* :1 * 
      
