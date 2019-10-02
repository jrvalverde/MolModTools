
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 geserr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "geserr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "geserr.web"
      subroutine geserr(NERR)
      implicit none
      integer In,Iout,Ipunch,NERR
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
99001 format('  NO MO''S OR DENSITY FOUND.')
99002 format('  PROJECTION SUPPRESSED OR BASES IDENTICAL; YET INPUT DATA
     & HAS WR           ONG NBASIS.')
99003 format('  NO BASIS READ IN; YET INPUT DATA IS FOR NEITHER THE CURR
     &ENT BAS           IS NOR FOR A MINIMAL BASIS.')
99004 format('  DO NOT USE IOP(8)=2 WITH PHF.')
99005 format('  INPUT BASIS AND INPUT MATRIX HAVE DIFFERENT NBASIS.')
99006 format('  INPUT MATRICES HAVE DIFFERENT NBASIS.')
99007 format('  OVERLAP HAS NEGATIVE OR ZERO EIGENVALUE.')
99008 format('  CANNOT COMPLETE COMPLEX MO''S FOR NBASIS.GT.65')
99009 format('  CANNOT HANDLE THIRD ROW ATOMS')
      
      
      if(NERR.EQ.2)then
      
      write(Iout,99002)
      elseif(NERR.EQ.3)then
      
      write(Iout,99003)
      elseif(NERR.EQ.4)then
      
      write(Iout,99004)
      elseif(NERR.EQ.5)then
      
      write(Iout,99005)
      elseif(NERR.EQ.6)then
      
      write(Iout,99006)
      elseif(NERR.EQ.7)then
      
      write(Iout,99007)
      elseif(NERR.EQ.8)then
      
      write(Iout,99008)
      elseif(NERR.EQ.9)then
      
      write(Iout,99009)
      elseif(NERR.EQ.10)then
      elseif(NERR.NE.11)then
      
      write(Iout,99001)
      endif
      
      
      call lnk1e
      return
      
      end
C* :1 * 
      
