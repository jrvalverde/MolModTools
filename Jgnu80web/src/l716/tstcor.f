
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tstcor"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tstcor.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "tstcor.web"
      subroutine tstcor(IEND,MXCORE,NAME)
      implicit none
      integer IEND,In,Iout,Ipunch,MXCORE
      character*6 NAME
      common/io/In,Iout,Ipunch
      
      
      
      
99001 format(' STORAGE ALLOCATION FAILURE FOR ',a6/'   NEEDS:',i6,'   AV
     &AILABLE:',i6)
      
      if(IEND.LE.MXCORE)return
      write(Iout,99001)NAME,IEND,MXCORE
      call lnk1e
      return
      
      end
C* :1 * 
      
