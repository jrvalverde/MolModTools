
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gatan2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gatan2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "gatan2.web"
      double precision function gatan2(ARG1,ARG2)
      implicit none
      double precision ARG1,ARG2
      
      
      
      
      gatan2=datan2(ARG1,ARG2)
      return
      
      end
C* :1 * 
      
