
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fperr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fperr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "fperr.web"
      subroutine fperr(MODE)
      implicit none
      integer In,Iout,Ipunch,MODE
      common/io/In,Iout,Ipunch
      
      
      
      write(Iout,99001)MODE
      
99001 format(' FPERR ACTIVE: ',i9)
      
      call fpdump
      call lnk1e
      stop
      
      end
C* :1 * 
      
