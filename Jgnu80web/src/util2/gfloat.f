
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gfloat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gfloat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "gfloat.web"
      double precision function gfloat(IARG)
      implicit none
      integer IARG
      double precision dfloat
      
      
      
      
      gfloat=dfloat(IARG)
      return
      
      end
C* :1 * 
      
