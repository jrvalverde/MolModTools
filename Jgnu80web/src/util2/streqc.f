
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 streqc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "streqc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "streqc.web"
      logical function streqc(I,J,LEN)
      implicit none
      integer I,J
      integer LEN
      logical streq
      dimension I(LEN),J(LEN)
      streqc=streq(I,J,LEN)
      return
      
      end
C* :1 * 
      
