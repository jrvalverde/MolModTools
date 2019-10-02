
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bldmap"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bldmap.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "bldmap.web"
      subroutine bldmap(C,SHELLT,SHELLC,NSHELL,I56D)
      implicit none
      integer I56D,irwmap,lrwmap,Mapper,Maprot,MAXATM,MAXBAS,Nrot,NSHELL
      parameter(MAXBAS=150,MAXATM=100)
      double precision C(*)
      integer SHELLT(*),SHELLC(*)
      common/maps/Nrot,Maprot(MAXBAS),Mapper(MAXATM)
      data irwmap/559/,lrwmap/125/
      
      
      
      
      call permap(NSHELL,SHELLT,SHELLC,C,I56D,Mapper)
      
      call rotmap(NSHELL,SHELLT,SHELLC,I56D,Maprot,Nrot)
      
      call twrite(irwmap,Nrot,lrwmap,1,lrwmap,1,0)
      return
      
      end
C* :1 * 
      
