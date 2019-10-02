
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0901"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0901.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "uu0901.web"
      blockdata uu0901
      
      
      
      implicit none
      double precision Dummy,V
      integer Igeno,Inforb,La0,Lanorm,Lehf,Lenrgy,Ligen,Lisd,Lmp2,Lnforb
     &,Ls20,Ls21,Mdv,Nobuc
      common/bd0901/Dummy
      common/v/V(20000),Mdv
      common/rwfl/Igeno,Ligen,Inforb,Lnforb
      common/lgen/Lehf,Lmp2,Ls20,Ls21,Lenrgy,Lanorm,La0,Lisd
      common/nobuc/Nobuc
      data Mdv/20000/
      data V/20000*0./
      data Igeno,Ligen/1,47/
      data Inforb,Lnforb/45,14/
      data Lehf,Lmp2,Lanorm,Lenrgy,Ls20,Ls21/32,33,42,43,44,45/
      data Lisd,La0/6,47/
      data Nobuc/31/
      
      
      end
C* :1 * 
      
