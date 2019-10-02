
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fdump"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fdump.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "fdump.web"
      subroutine fdump
      implicit none
      integer base,Fstat,i,ifin,In,Iout,Ipunch,j,lim,Mpos,Nfiles,npl
      character*8 label
      dimension label(5)
      common/io/In,Iout,Ipunch
      common/fstat/Nfiles,Mpos,Fstat(5,200)
      data npl/11/
      data label/' NUMBER ',' BASE   ',' END    ',' WR PNTR',' RD PNTR'/
      
      
      
      
      
99001 format(4x)
99002 format(a8,13I9)
99003 format(' DUMPING /FSTAT/, NFILES =',i3,', MPOS= ',i5)
      
      write(Iout,99003)Nfiles,Mpos
      
      ifin=0
      base=1
      lim=npl
      
100   if(lim.GE.Nfiles)then
      ifin=1
      lim=Nfiles
      endif
      write(Iout,99001)
      
      do 200 j=1,5
      write(Iout,99002)label(j),(Fstat(j,i),i=base,lim)
200   continue
      
      if(ifin.NE.1)then
      base=lim+1
      lim=lim+npl
      goto 100
      endif
      
      return
      
      end
C* :1 * 
      
