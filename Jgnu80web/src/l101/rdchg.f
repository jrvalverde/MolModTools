
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rdchg"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rdchg.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "rdchg.web"
      subroutine rdchg(NAMCNT,NZ,IANZ,IOP)
      implicit none
      double precision charge,chg,fp,str
      integer i,IANZ,ichg,ieof,if1,In,IOP,iord,Iout,Ipunch,irwchg,iz,len
     &,lsubst,NAMCNT,NZ
      dimension str(10),NAMCNT(*),charge(50),IANZ(*),IOP(50)
      common/io/In,Iout,Ipunch
      data irwchg/577/
99001 format(' NON DEFAULT ATOMIC CHARGES READ IN:')
99002 format('*** END OF FILE')
99003 format('CENTER SPEC. MUST BE EITHER INTEGER OR STRING.')
99004 format('CENTER SPEC. TOO LONG')
      
      write(Iout,99001)
      do 100 i=1,NZ
      charge(i)=IANZ(i)
100   continue
      call ffread(ieof)
      if(ieof.NE.0)then
      write(Iout,99002)
      call lnk1e
      endif
      
200   call ffget(str,len,i,fp,if1)
      
      if(if1.EQ.iord('END'))goto 400
      if(if1.NE.iord('STR'))then
      if(if1.EQ.iord('INT'))then
      
      iz=i
      call szprnt(1,iz,0,1)
      goto 300
      else
      write(Iout,99003)
      call fferr(0,if1)
      endif
      endif
      
      if(len.GT.4)then
      write(Iout,99004)
      call fferr(0,0)
      endif
      iz=lsubst(NAMCNT,NZ,str,len)
      call szprnt(1,str,len,4)
      
300   call ffget(str,len,ichg,chg,if1)
      if(if1.NE.iord('FP'))call fferr('FP',if1)
      charge(iz)=chg
      call szprnt(1,chg,0,2)
      call szprnt(2,0,0,0)
      call ffread(ieof)
      if(ieof.EQ.0)goto 200
400   call twrite(irwchg,charge,NZ,1,NZ,1,0)
      if(IOP(34).EQ.0)return
      do 500 i=1,NZ
      write(Iout,99005)i,charge(i)
500   continue
      
99005 format(1x,i10,g15.5)
      
      return
      
      end
C* :1 * 
      
