
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ind401"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ind401.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "ind401.web"
      subroutine ind401(NB,INDB,INDAA,INDBB,INDSCR,INDTAB)
      implicit none
      integer INDAA,INDB,INDBB,INDSCR,INDTAB,indtot,mxcore,NB
      data mxcore/50000/
      INDB=NB*NB+1
      INDAA=INDB+NB*NB
      INDBB=INDAA+NB
      INDSCR=INDBB+NB
      INDTAB=INDSCR+36*11
      indtot=INDTAB+3*NB
      if(indtot.LT.50000)return
      write(6,99001)indtot,mxcore
99001 format(' Not enough MEMORY in L401; REQUIRED ',i8,'  PROVIDED ',i8
     &)
      call lnk1e
      
      end
C* :1 * 
      
