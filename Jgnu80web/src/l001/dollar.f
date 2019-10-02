
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dollar"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dollar.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "dollar.web"
      subroutine dollar(NTIMES,IPRT)
      implicit none
      integer i,Icard,ij,In,iord,Iout,IPRT,Ipunch,Irest,irstrt,NTIMES
      integer iicard(80)
      logical intyn,rwfyn,guesyn,saveyn
      dimension Icard(80),irstrt(7)
      common/ertcrd/Icard
      common/io/In,Iout,Ipunch
      common/restar/Irest
      data irstrt/'R','E','S','T','A','R','T'/
      
99001 format(80A1)
      
      Irest=0
      NTIMES=0
      intyn=.FALSE.
      rwfyn=.FALSE.
      guesyn=.FALSE.
      saveyn=.FALSE.
100   read(In,99001,end=300)(iicard(i),i=1,80)
      call captlz(iicard,Icard,320)
      if(Icard(1).EQ.iord('%'))call l0cmnd(iicard,Iout,intyn,rwfyn,guesy
     &n,saveyn)
      if(Icard(1).EQ.iord('#'))then
      
      if(.NOT.rwfyn)open(unit=18,status='SCRATCH',access='DIRECT',recl=1
     &6380,form='UNFORMATTED')
      if(.NOT.intyn)open(unit=3,status='SCRATCH',form='UNFORMATTED')
      rewind 3
      
      do 150 i=1,7
      ij=i+1
      if(Icard(ij).NE.irstrt(i))goto 200
150   continue
      Irest=1
      call rstart
      NTIMES=-1
      endif
      goto 100
      
200   NTIMES=-1
      IPRT=Icard(2)
      return
300   write(Iout,99002)
99002 format(' End of Input Stream; no more Command Records')
      write(Iout,99003)
99003 format(' **** End of gnu80 JOB ****')
      stop
      
      end
C* :1 * 
      
