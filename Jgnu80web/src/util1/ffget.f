
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ffget"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ffget.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "ffget.web"
      subroutine ffget(STR,LEN,INTGR,FP,IFOUND)
      implicit none
      integer comma,delims,equal,getchr,Idump,IFOUND,In,INTGR,Iold,iord,
     &Iout,Ipunch,itst,j,l,Lcursr,LEN,lenlin,Line,Ncom
      integer qpaint,qpdp,STR
      integer junk,qpword
      double precision FP
      dimension delims(2),STR(*)
      common/io/In,Iout,Ipunch
      common/fffcom/Idump,Lcursr,Iold,Ncom,Line(40)
      data delims/3,',= '/,comma/','/,equal/'='/
      data lenlin/80/
      
      
      
99001 format('  FFGET -- FLOATING POINT NUMBER:',d20.7)
99002 format('  FFGET -- INTEGER:',i10)
99003 format('  FFGET -- END-OF-LINE.')
99004 format('  FFGET --- NULL FIELD FOUND.')
      
      Iold=Lcursr
      
100   call qpskbl(Line,Lcursr,lenlin)
      if(Lcursr.GE.lenlin)then
      
      IFOUND=iord('END')
      if(Idump.NE.0)write(Iout,99003)
      return
      else
      j=getchr(Line,Lcursr)
      if(.NOT.((j.EQ.comma.OR.j.EQ.equal).AND.Ncom.NE.0))then
      if(j.EQ.comma.OR.j.EQ.equal)Ncom=1
      if(j.EQ.comma.OR.j.EQ.equal)goto 100
      Lcursr=Lcursr-1
      
      itst=qpdp(Line,lenlin,Lcursr,FP)
      if(itst.EQ.0)then
      Ncom=0
      IFOUND=iord('FP')
      if(Idump.NE.0)write(Iout,99001)FP
      return
      
      elseif(qpaint(10,Line,lenlin,Lcursr,INTGR,0).NE.0)then
      
      l=0
      junk=qpword(1,delims,l,Line,lenlin,Lcursr,STR,LEN)
      IFOUND=iord('STR')
      Ncom=0
      return
      else
      IFOUND=iord('INT')
      Ncom=0
      if(Idump.NE.0)write(Iout,99002)INTGR
      return
      endif
      endif
      endif
      
      IFOUND=iord('NUL')
      Ncom=1
      if(Idump.NE.0)write(Iout,99004)
      return
      
      end
C* :1 * 
      
