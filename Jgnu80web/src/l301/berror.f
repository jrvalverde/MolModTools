
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 berror"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "berror.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "berror.web"
      subroutine berror(IERR)
      implicit none
      integer IERR,In,Iout,Ipunch
      double precision ibasis,jbasis
      dimension ibasis(8),jbasis(8)
      common/io/In,Iout,Ipunch
      data ibasis/6H     M,6H    EX,6H LP--M,6HLP--EX,6H      ,6HN-21G ,
     &6H      ,6H     G/
      data jbasis/6HINIMAL,6HTENDED,6HINIMAL,6HTENDED,6H6-311G,6HBASIS ,
     &6H      ,6HENERAL/
      
      
      
      
      
      
      
      
      
99001 format('1ERROR TERMINATION IN BERROR, K=',i9/' BASIS IN ERROR: ',2
     &A6)
      
      write(Iout,99001)IERR,ibasis(IERR),jbasis(IERR)
      call lnk1e
      stop
      
      end
C* :1 * 
      
