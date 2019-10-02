
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bldtbl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bldtbl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "bldtbl.web"
      subroutine bldtbl(A,B,TABLE,NB,NCOL,AA,IRWC,IRWS,NATOMS,NOSYM)
      implicit none
      double precision A,AA,B,sum,TABLE,zero
      integer i,id,iop,IRWC,irwmap,IRWS,irwsc,lrwmap,mapdgn,Mapper,Mapro
     &t,MAXATM,MAXBAS,NATOMS,NB,NCOL,ndgn,NOSYM,Nrot,nsq
      parameter(MAXBAS=150,MAXATM=100)
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      dimension A(NB,NCOL),B(NB,NCOL),TABLE(NCOL,6),AA(*)
      dimension mapdgn(MAXBAS)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      common/maps/Nrot,Maprot(MAXBAS),Mapper(MAXATM)
      data irwsc/3555/,irwmap/559/,lrwmap/125/
      data zero/0.0D0/
      
      
      
      
      
      nsq=NB*NB
      
      do 100 i=1,NCOL
      mapdgn(i)=0
100   continue
      ndgn=0
      
      call tread(IRWS,A,NB,NB,NB,NB,1)
      call tread(IRWC,B,nsq,1,nsq,1,0)
      call matrec(A,B,AA,NB,NB,NB,NCOL,1)
      call twrite(irwsc,A,nsq,1,nsq,1,0)
      
      call tread(irwmap,Nrot,lrwmap,1,lrwmap,1,0)
      
      do 200 iop=1,Nsymop
      if(iop.NE.1)call tread(irwsc,A,nsq,1,nsq,1,0)
      call tread(IRWC,B,nsq,1,nsq,1,0)
      call trnfrm(A,Symops(1,iop),Iprmut(1,iop),NB,NCOL,NATOMS,AA)
      call matrec(B,A,AA,NB,NCOL,NB,NCOL,5)
      
      call dgnmap(A,NB,NCOL,mapdgn,ndgn,NOSYM)
      
      do 150 i=1,NCOL
      TABLE(i,iop)=A(i,i)
150   continue
200   continue
      
      if(ndgn.EQ.0)return
      do 300 id=1,ndgn
      do 250 iop=1,Nsymop
      sum=zero
      do 220 i=1,NCOL
      if(mapdgn(i).EQ.id)sum=sum+TABLE(i,iop)
220   continue
      do 240 i=1,NCOL
      if(mapdgn(i).EQ.id)TABLE(i,iop)=sum
240   continue
250   continue
300   continue
      return
      
      end
C* :1 * 
      
