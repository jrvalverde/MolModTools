
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 not"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "not.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "not.web"
      integer function not(ITEM)
      implicit none
      integer ITEM,mask
      data mask/-1/
      
      not=ieor(ITEM,mask)
      return
      
      end
C* :1 * 
      
