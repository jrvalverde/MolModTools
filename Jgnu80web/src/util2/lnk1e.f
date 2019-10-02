
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lnk1e"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lnk1e.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "lnk1e.web"
      subroutine lnk1e
      implicit none
      integer i,irwovg,Ll,Nchain
      real w
      integer error,fclose
      common/tmpnch/Nchain,Ll
      data error/999/,irwovg/999/,fclose/10/
      
      
      write(6,99001)
      
99001 format(' **** ERROR TERMINATION IN LNK1E.')
      
      
      
      if(Nchain.NE.error)then
      if(Nchain.EQ.fclose)goto 100
      Nchain=error
      Ll=0
      
      call twrite(irwovg,Nchain,1,1,1,1,0)
      endif
      
      Nchain=fclose
      call fileio(fclose,i,i,i,i)
100   w=-1.0D00
      write(6,99002)
      call g80end
      stop 13
      
99002 format(' ','LNK1E ***** gnu80 STOPS EXECUTING DUE TO ERROR')
      
      end
C* :1 * 
      
