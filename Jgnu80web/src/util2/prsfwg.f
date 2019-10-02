
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prsfwg"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prsfwg.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "prsfwg.web"
      subroutine prsfwg(FWG,IPOS,ICHAR,JCHAR,NATSS)
      implicit none
      integer i,ICHAR,In,iord,Iout,IPOS,Ipunch,ival,JCHAR,mult,NATSS,npr
     &in,numer
      integer FWG(4)
      logical digit
      common/io/In,Iout,Ipunch
      
      
      
      
      
99001 format(1x,'PRSFWG-- UNABLE TO FIND LEFT SQUARE BRACKET')
99002 format(1x,'PRSFWG-- NON-DIGIT FOLLOWS "C" IN LINEAR SUBSPACE')
99003 format(1x,'PRSFWG-- UNRECOGINIZED SUBSPACE, ICHAR: "',a4,'"')
99004 format(1x,'PRSFWG-- UNRECOGNIZED POINT GROUP, ICHAR="X"')
99005 format(1x,'PRSFWG-- LEFT PARENTHESIS NOT FOUND')
99006 format(1x,'PRSFWG-- RIGHT PARENTHESIS NOT FOUND')
      
      
      NATSS=0
      if(IPOS.EQ.0)then
      do 50 i=1,100
      if(FWG(i).EQ.iord('<'))then
      IPOS=i+1
      goto 100
      endif
      
50    continue
      write(Iout,99001)
      ICHAR=iord('E')
      return
      endif
100   mult=1
      call digtst(FWG,IPOS,digit,ival)
      if(digit)mult=ival
      ICHAR=FWG(IPOS)
      IPOS=IPOS+1
      if(ICHAR.NE.iord('>'))then
      if(ICHAR.NE.iord('O'))then
      if(ICHAR.EQ.iord('C'))then
      call digtst(FWG,IPOS,digit,ival)
      if(.NOT.digit.AND.FWG(IPOS).NE.iord('*'))then
      
      ICHAR=iord('E')
      write(Iout,99002)
      return
      else
      JCHAR=ival
      endif
      
      elseif(ICHAR.EQ.iord('S'))then
      JCHAR=FWG(IPOS+1)
      IPOS=IPOS+1
      if(JCHAR.EQ.iord('('))then
      JCHAR=iord(' ')
      IPOS=IPOS-1
      endif
      
      elseif(ICHAR.NE.iord('X'))then
      
      write(Iout,99003)ICHAR
      ICHAR=iord('E')
      return
      else
      nprin=numer(FWG)
      if(FWG(1).EQ.iord('C'))then
      JCHAR=2*nprin
      if(FWG(4).EQ.iord(' '))JCHAR=nprin
      if(nprin.EQ.0)JCHAR=2
      
      elseif(FWG(1).EQ.iord('D'))then
      JCHAR=4*nprin
      if(FWG(4).EQ.iord(' '))JCHAR=2*nprin
      
      elseif(FWG(1).EQ.iord('S'))then
      JCHAR=nprin
      
      elseif(FWG(1).EQ.iord('T'))then
      JCHAR=12
      if(FWG(2).EQ.iord('D'))JCHAR=24
      
      elseif(FWG(1).NE.iord('O'))then
      
      write(Iout,99004)
      ICHAR=iord('E')
      return
      else
      JCHAR=24
      if(FWG(2).EQ.iord('H'))JCHAR=48
      endif
      endif
      endif
150   if(IPOS.GT.100)then
      ICHAR=iord('E')
      write(Iout,99005)
      return
      
      elseif(FWG(IPOS).EQ.iord('('))then
160   IPOS=IPOS+1
      if(IPOS.GT.100)then
      ICHAR=iord('E')
      write(Iout,99006)
      return
      
      elseif(FWG(IPOS).EQ.iord(')'))then
      IPOS=IPOS+1
      if(FWG(IPOS).EQ.iord(','))IPOS=IPOS+1
      else
      call digtst(FWG,IPOS,digit,ival)
      if(digit)then
      IPOS=IPOS-1
      NATSS=NATSS+ival*mult
      endif
      goto 160
      endif
      else
      IPOS=IPOS+1
      goto 150
      endif
      endif
      return
      
      end
C* :1 * 
      
