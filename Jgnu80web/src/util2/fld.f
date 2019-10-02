
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fld"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fld.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "fld.web"
      integer function fld(STB,NBITS,LOC)
      implicit none
      integer lloc,LOC,loc1,NBITS,power,STB
      
      power=2**(STB+NBITS)
      loc1=LOC/power
      lloc=LOC-loc1*power
      fld=lloc/(2**STB)
      return
      
      end
C* :1 * 
      
