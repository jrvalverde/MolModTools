
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 crop"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "crop.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "crop.web"
      integer function crop(CHR)
      implicit none
      integer lcur
      integer CHR,getchr
      
      lcur=0
      crop=getchr(CHR,lcur)
      return
      
      end
C* :1 * 
      
