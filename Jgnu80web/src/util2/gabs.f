
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gabs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gabs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "gabs.web"
      double precision function gabs(ARG)
      implicit none
      double precision dbas,ARG
      
      
      
      
      gabs=dabs(ARG)
      return
      
      end
C* :1 * 
      
