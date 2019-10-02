
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uuqpar"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uuqpar.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "uuqpar.web"
      blockdata uuqpar
      implicit none
      integer Maxdep,Maxlen,Qpabrv,Qpambg,Qpdpth,Qpend,Qperr,Qpexit,Qpfa
     &il,Qpok,Qprecr,Qpret,Stack
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxlen
      data Qpok/0/,Qpret/1/,Qpfail/-1/,Qpambg/-2/,Qperr/-3/,Qpexit/-10/,
     &Qpabrv/-11/,Qpend/-13/,Qprecr/23/,Qpdpth/0/,Maxdep/10/,Maxlen/64/
      
      
      end
C* :1 * 
      
