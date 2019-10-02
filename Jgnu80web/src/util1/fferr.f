
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fferr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fferr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "fferr.web"
      subroutine fferr(WANTED,FOUND)
      implicit none
      integer blank,FOUND,i,Idump,In,Iold,iord,Iout,Ipunch,Lcursr,Line,N
     &com,questm,WANTED
      common/io/In,Iout,Ipunch
      common/fffcom/Idump,Lcursr,Iold,Ncom,Line(40)
      data blank/4H    /,questm/1H?/
      
99001 format('  WANTED AN INTEGER AS INPUT.')
99002 format('  WANTED A FLOATING POINT NUMBER AS INPUT.')
99003 format('  WANTED A STRING AS INPUT.')
99004 format('  FOUND AN INTEGER AS INPUT.')
99005 format('  FOUND A FLOATING POINT NUMBER AS INPUT.')
99006 format('  FOUND A STRING AS INPUT.')
99007 format('  FOUND AN END-OF-LINE FOR INPUT.')
99008 format('  FOUND A NULL FIELD AS INPUT.')
      
      if(WANTED.NE.0)then
      if(WANTED.EQ.iord('INT'))write(Iout,99001)
      if(WANTED.EQ.iord('FP'))write(Iout,99002)
      if(WANTED.EQ.iord('STR'))write(Iout,99003)
      endif
      
      if(FOUND.NE.0)then
      if(FOUND.EQ.iord('INT'))write(Iout,99004)
      if(FOUND.EQ.iord('FP'))write(Iout,99005)
      if(FOUND.EQ.iord('STR'))write(Iout,99006)
      if(FOUND.EQ.iord('END'))write(Iout,99007)
      if(FOUND.EQ.iord('NUL'))write(Iout,99008)
      endif
      
      call strout(Iout,Line,80,1)
      write(Iout,99009)(blank,i=1,Iold),questm
      
99009 format(1x,80A1)
      
      call lnk1e
      return
      
      end
C* :1 * 
      
