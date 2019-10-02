
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gsign"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gsign.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "gsign.web"
      double precision function gsign(ARG1,ARG2)
      implicit none
      double precision dsign,ARG1,ARG2
      
      
      
      
      gsign=dsign(ARG1,ARG2)
      return
      
      end
C* :1 * 
      
