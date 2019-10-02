
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 zparm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "zparm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 43 "zparm.web"
      subroutine zparm(X,LX,SYMBLS,NSB,ISYMB)
      implicit none
      double precision fp,str,SYMBLS,X,zero
      integer i,if1,iord,isgn,ISYMB,len,LX,NSB
      dimension str(5),SYMBLS(*)
      data zero/0.0D0/
      
      call ffget(str,len,i,fp,if1)
      if(if1.NE.iord('FP'))then
      if(if1.EQ.iord('STR'))goto 100
      call fferr('FP',if1)
      endif
      
      X=fp
      LX=2
      call szprnt(1,fp,0,2)
      call putchr('0',SYMBLS,NSB)
      call putdel(2,SYMBLS,NSB)
      return
      
100   call nosign(str,len,isgn)
      call putb(str,len,SYMBLS,NSB)
      call putdel(2,SYMBLS,NSB)
      X=zero
      LX=3*isgn
      ISYMB=ISYMB+1
      call szprnt(1,str,len,LX)
      return
      
      end
C* :1 * 
      
