
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ffset"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ffset.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "ffset.web"
      subroutine ffset(ID)
      implicit none
      integer ID,Idump,In,Iold,Iout,Ipunch,Lcursr,Line,Ncom
      common/io/In,Iout,Ipunch
      common/fffcom/Idump,Lcursr,Iold,Ncom,Line(40)
      Idump=ID
      if(ID.NE.0)write(Iout,99001)
      
99001 format('  FREE-FIELD ROUTINES INITIALIZED.')
      
      return
      
      end
C* :1 * 
      
