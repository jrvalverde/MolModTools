
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zcentr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zcentr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 38 "zcentr.web"
      subroutine zcentr(IZ,NAMCNT,NZ)
      implicit none
      integer i,if1,In,iord,Iout,Ipunch,IZ,len,lsubst,NAMCNT,NZ,str
      double precision fp
      dimension str(10),NAMCNT(*)
      common/io/In,Iout,Ipunch
      
      call ffget(str,len,i,fp,if1)
      
      if(if1.NE.iord('STR'))then
      if(if1.EQ.iord('INT'))then
      
      IZ=i
      call szprnt(1,IZ,0,1)
      return
      else
      write(Iout,99001)
      
99001 format('  CENTER SPEC. MUST BE EITHER INTEGER OR STRING.')
      
      call fferr(0,if1)
      endif
      endif
      
      if(len.LE.4)then
      IZ=lsubst(NAMCNT,NZ-1,str,len)
      call szprnt(1,str,len,4)
      return
      endif
      
      write(Iout,99002)
      
99002 format('  CENTER NAME IS TO LONG.')
      
      call strout(Iout,3H  ?,3,0)
      call strout(Iout,str,len,1)
      call strout(Iout,1H?,1,1)
      call fferr(0,0)
      return
      
      end
C* :1 * 
      
