
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 outrep"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "outrep.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "outrep.web"
      subroutine outrep(IDUMP)
      implicit none
      integer blank,i,IDUMP,In,Iout,Ipunch,irwrep,Isave,j,lbcur,len,lrwr
     &ep,Ncnt,Nsave,tmp
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      dimension tmp(10)
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      common/reploc/Nsave,Ncnt,Isave(15)
      common/io/In,Iout,Ipunch
      data irwrep/562/,lrwrep/767/
      data blank/' '/
      
      
      call twrite(irwrep,Nsymop,lrwrep,1,lrwrep,1,0)
      
      if(IDUMP.EQ.0)return
      if(Nreps.EQ.0)return
      write(Iout,99001)Nsymop,Nreps,(Lblrep(i),i=1,Nreps)
      
99001 format('  NSYMOP =',i2,',NREPS =',i2,/,'  ORBITAL LABELS:  ',15A4)
      
      write(Iout,99002)
      
99002 format('  CHARACTER TABLE.')
      
      lbcur=0
      do 100 i=1,Nreps
      tmp(1)=blank
      call getb(2,tmp,len,Lblrep,lbcur)
      write(Iout,99003)tmp(1),(Chrtbl(j,i),j=1,Nsymop)
      
99003 format(1x,a4,2x,10F7.1)
      
100   continue
      
      write(Iout,99004)Nsave,(Isave(i),i=1,Nsave)
      
99004 format('   # OF SAVED OPERATIONS =',i2,', SAVED:',10I3)
      
      do 200 i=1,Nsymop
      write(Iout,99005)(Symops(j,i),j=1,9)
      
99005 format(3(10x,3D15.4,/))
      
      write(Iout,99006)(Iprmut(j,i),j=1,10)
      
99006 format('  NUCLEAR PERMUTATIONS:',10I3)
      
200   continue
      return
      
      end
C* :1 * 
      
