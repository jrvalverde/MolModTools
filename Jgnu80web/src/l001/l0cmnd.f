
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 l0cmnd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "l0cmnd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "l0cmnd.web"
      subroutine l0cmnd(ICARD,IOUT,INTYN,RWFYN,GUESYN,SAVEYN)
      implicit none
      integer i,ii,itop,jtop,l,lenrec,lfname,m
      integer ICARD(*),IOUT
      logical INTYN,RWFYN,SAVEYN,GUESYN
      integer jcard(80),intrec(3),rwfrec(3),equals,blank
      integer savrec(4),gesrec(5)
      integer intlc(3),rwflc(3),savelc(4),gueslc(5)
      integer maxrec
      character getlcu
      character*1 fname1(20)
      character*20 fname
      equivalence(fname1,fname)
      data lenrec,lfname,maxrec/3,20,80/
      data intrec/'I','N','T'/
      data intlc/'i','n','t'/
      data rwfrec/'R','W','F'/
      data rwflc/'r','w','f'/
      data savrec/'S','A','V','E'/
      data savelc/'s','a','v','e'/
      data gesrec/'G','U','E','S','S'/
      data gueslc/'g','u','e','s','s'/
      data equals/'='/
      data blank/' '/
      do 100 i=1,lfname
      fname1(i)=' '
100   continue
      itop=maxrec
      do 200 ii=1,maxrec
      jcard(ii)=blank
      i=maxrec-ii+1
      if(ICARD(i).NE.blank)then
      itop=i
      goto 300
      endif
200   continue
300   m=0
      do 400 i=1,itop
      if(ICARD(i).NE.blank)then
      m=m+1
      jcard(m)=ICARD(i)
      jtop=m
      endif
400   continue
      if(jtop.GE.(lenrec+3))then
      do 450 i=1,lenrec
      if((jcard(i+1).NE.intrec(i)).AND.(jcard(i+1).NE.intlc(i)))goto 550
450   continue
      if(jcard(lenrec+2).NE.equals)goto 1000
      m=0
      do 500 i=6,jtop
      l=4*m+(lenrec+2)*4
      m=m+1
      fname1(m)=getlcu(jcard,l)
500   continue
      INTYN=.TRUE.
      open(unit=3,file=fname,status='UNKNOWN',form='UNFORMATTED')
      rewind 3
      return
550   do 600 i=1,lenrec
      if((jcard(i+1).NE.rwfrec(i)).AND.(jcard(i+1).NE.rwflc(i)))goto 700
600   continue
      if(jcard(5).NE.equals)goto 1000
      m=0
      do 650 i=6,jtop
      l=4*m+(lenrec+2)*4
      m=m+1
      fname1(m)=getlcu(jcard,l)
650   continue
      RWFYN=.TRUE.
      open(unit=18,file=fname,status='UNKNOWN',access='DIRECT',recl=1638
     &0,form='UNFORMATTED')
      return
700   do 750 i=1,5
      if((jcard(i+1).NE.gesrec(i)).AND.(jcard(i+1).NE.gueslc(i)))goto 85
     &0
750   continue
      if(jcard(7).NE.equals)goto 1000
      m=0
      do 800 i=8,jtop
      l=4*m+(7)*4
      m=m+1
      fname1(m)=getlcu(jcard,l)
800   continue
      GUESYN=.TRUE.
      open(unit=8,file=fname,status='UNKNOWN',form='FORMATTED')
      rewind 8
      return
850   do 900 i=1,4
      if((jcard(i+1).NE.savrec(i)).AND.(jcard(i+1).NE.savelc(i)))goto 10
     &00
900   continue
      if(jcard(6).EQ.equals)then
      m=0
      do 920 i=7,jtop
      l=4*m+(6)*4
      m=m+1
      fname1(m)=getlcu(jcard,l)
920   continue
      SAVEYN=.TRUE.
      open(unit=9,file=fname,status='UNKNOWN',form='FORMATTED')
      rewind 9
      return
      endif
      endif
1000  write(IOUT,99001)(ICARD(i),i=1,20)
99001 format(' Link 0 Control Record in ERROR:',/,20A1)
      call lnk1e
      return
      end
C* :1 * 
      
