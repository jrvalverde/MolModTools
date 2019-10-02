
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 binrd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "binrd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "binrd.web"
      subroutine binrd(IX,ITITLE,IDENT,NWRD,NB)
      implicit none
      integer i,idel,IDENT,iflag,ik,ilower,In,Inreg,Iout,Ipunch,isilly,I
     &TITLE,Iu1,Iudum,iupper,j,jdent,kount,mb,mwrd
      integer NB,numwpc,NWRD
      double precision IX
      dimension IX(*),ITITLE(20)
      dimension IDENT(4),jdent(4)
      common/io/Inreg,Iout,Ipunch
      common/munit/Iu1(5),In,Iudum(14)
      data numwpc/18/
      
      
      
      
      
      
      
      
      
      
99001 format(20A4)
99002 format(1x,18A4,i7)
99003 format(' BINARY INPUT CARDS ARE OUT OF SEQUENCE. IDENT= ',4A4,8H, 
     &KOUNT=,i5)
99004 format(1x,4A4,18x,i5,10x,i3)
      
      
      
      
      
      NWRD=0
      NB=0
      
      rewind In
      
      
100   read(In,99004,end=300)(jdent(i),i=1,4)
      do 200 i=1,4
      if(jdent(i).NE.IDENT(i))goto 100
200   continue
      
      backspace In
      read(In,99004)(ITITLE(i),i=1,4),mwrd,mb
      NWRD=mwrd
      NB=mb
      
      
      
      read(In,99001)(ITITLE(i),i=1,20)
      
      
      NWRD=NWRD/2
      read(In,99005)(IX(i),i=1,NWRD)
99005 format(4D20.12)
      
      isilly=0
      if(isilly.EQ.0)return
      
      kount=0
      iflag=1
      ilower=1
      goto 400
      
      
300   return
400   iupper=ilower+numwpc-1
      idel=iupper-NWRD
      if(idel.GT.0)then
      iupper=NWRD
      iflag=0
      endif
      kount=kount+1
      if(iflag.EQ.1)read(In,99002)(IX(i),i=ilower,iupper),ik
      if(iflag.EQ.0)read(In,99002)(IX(i),i=ilower,iupper),(j,i=1,idel),i
     &k
      if(ik.EQ.kount)then
      
      
      if(iflag.EQ.0.OR.iupper.EQ.NWRD)goto 100
      ilower=ilower+numwpc
      goto 400
      endif
      
      
      write(Iout,99003)(IDENT(i),i=1,4),kount
      call lnk1e
      stop 13
      
      end
C* :1 * 
      
