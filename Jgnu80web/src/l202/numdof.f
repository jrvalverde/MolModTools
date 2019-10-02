
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 numdof"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "numdof.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "numdof.web"
      integer function numdof(FRAME,NATOMS)
      implicit none
      integer i,ichar,In,iord,Iout,ipos,Ipunch,jchar,nat,NATOMS,natss,nd
     &of,nprin,ns,numer
      integer FRAME(*),fwg(100)
      common/io/In,Iout,Ipunch
      
      
      
      
      
99001 format(1x,'NUMDOF-- NOT CODED TO HANDLE GROUPS TH, I, OR IH')
99002 format(1x,'NUMDOF-- UNRECOGNIZED SYMMETRIC SUBSPACE, ICHAR= "',a4,
     &'"')
99003 format(1x,'NUMDOF-- NAT= ',i5,' NATOMS= ',i5)
      
      
      do 100 i=1,100
      fwg(i)=FRAME(i)
100   continue
      
      
      numdof=0
      ipos=0
      nat=0
      nprin=numer(fwg)
      if(fwg(1).EQ.iord('K'))return
      if(fwg(1).EQ.iord('I').OR.fwg(2).EQ.iord('T'))then
      if(.NOT.((fwg(1).EQ.iord('T').AND.fwg(2).EQ.iord(' ')).OR.(fwg(1).
     &EQ.iord('T').AND.fwg(2).EQ.iord('D'))))then
      write(Iout,99001)
      numdof=-1
      goto 300
      endif
      endif
200   call prsfwg(fwg,ipos,ichar,jchar,natss)
      if(ichar.NE.iord('>'))then
      nat=nat+natss
      if(ichar.EQ.iord('O'))goto 200
      if(ichar.EQ.iord('C'))then
      ndof=natss/2
      if(jchar.EQ.2.AND.fwg(1).EQ.iord('D'))ndof=natss/nprin
      if(fwg(1).EQ.iord('C').AND.fwg(4).NE.iord('H'))ndof=natss
      if(fwg(1).EQ.iord('T').AND.jchar.EQ.3)ndof=natss/4
      if(fwg(1).EQ.iord('O').AND.jchar.EQ.4)ndof=natss/6
      if((fwg(1).EQ.iord('O').AND.(jchar.EQ.2.OR.jchar.EQ.3)).OR.(fwg(1)
     &.EQ.iord('T').AND.jchar.EQ.2))ndof=natss/8
      numdof=numdof+ndof
      goto 200
      elseif(ichar.EQ.iord('S'))then
      if(nprin.EQ.0)nprin=1
      ndof=natss/nprin
      if(fwg(1).EQ.iord('C'))ndof=2*natss/nprin
      if(fwg(1).EQ.iord('T').OR.fwg(1).EQ.iord('O'))ndof=natss/12
      numdof=numdof+ndof
      goto 200
      elseif(ichar.NE.iord('X'))then
      numdof=-1
      write(Iout,99002)ichar
      else
      numdof=numdof+3*(natss/jchar)
      goto 200
      endif
      elseif(nat.NE.NATOMS)then
      numdof=0
      write(Iout,99003)nat,NATOMS
      
      elseif(fwg(1).EQ.iord('C').OR.fwg(1).EQ.iord('S'))then
      ns=0
      if(fwg(1).EQ.iord('S'))ns=1
      if(fwg(4).EQ.iord('H'))ns=1
      if(fwg(4).EQ.iord('V').OR.fwg(3).EQ.iord('V'))ns=1
      if(fwg(2).EQ.iord('S'))ns=3
      if(fwg(2).EQ.iord('I'))ns=3
      if(ns.EQ.0.AND.nprin.EQ.1)ns=6
      if(ns.EQ.0)ns=2
      numdof=numdof-ns
      endif
      
300   return
      
      end
C* :1 * 
      
