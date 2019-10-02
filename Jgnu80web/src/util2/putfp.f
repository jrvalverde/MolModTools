
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putfp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putfp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "putfp.web"
      subroutine putfp(X,N,BB,NBB)
      implicit none
      integer ieow,iord,m,N,NBB
      double precision X
      integer BB(*)
      integer Icbuf(34)
      character*1 Cbuf,cbuf1
      character*1 char1
      integer zero
      common/cbuf/Cbuf(136)
      equivalence(Icbuf,Cbuf)
      equivalence(cbuf1,char1)
      data zero/'0'/
      data ieow/1/
      
      
      call encode(30,N,Icbuf,X)
      
      if(N.LE.12)then
      m=30
50    cbuf1=Cbuf(m)
      if(iord(char1).NE.zero)then
      
      call pakstr(Icbuf,m,BB,NBB)
      call putdel(ieow,BB,NBB)
      return
      else
      m=m-1
      goto 50
      endif
      endif
      
      write(6,99001)
      call lnk1e
      
99001 format('  MORE THAN 12 FIGURES REQUESTED IN PUTFP.')
      
      return
      
      end
C* :1 * 
      
