
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 glog"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "glog.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "glog.web"
      double precision function glog(ARG)
      implicit none
      double precision dlog,ARG
      
      
      
      
      glog=dlog(ARG)
      return
      
      end
C* :1 * 
      
