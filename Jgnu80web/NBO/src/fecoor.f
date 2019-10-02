
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fecoor"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fecoor.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "fecoor.web"
      subroutine fecoor(ATCOOR)
      implicit none
      double precision ATCOOR
      integer Ispin,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,nfile
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension ATCOOR(3*Natoms)
      
      
      nfile=9
      call nbread(ATCOOR,3*Natoms,nfile)
      return
      end
C* :1 * 
      
