
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prmout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prmout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 47 "prmout.web"
      subroutine prmout(OK,IV0,STR,LEN,VALUE,INT,FP)
      implicit none
      integer In,INT,Iout,Ipunch,LEN,line,ncur,STR
      double precision FP,VALUE,zero,ten,hundrd,gabs
      logical IV0,OK
      dimension STR(*)
      dimension line(20)
      common/io/In,Iout,Ipunch
      data hundrd/100.0D0/,ten/10.0D0/,zero/0.0D0/
      
      ncur=0
      call pad(line,ncur,80,1H )
      ncur=1
      
      call putb(STR,LEN,line,ncur)
      
      ncur=20
      if(VALUE.GE.zero)ncur=ncur+1
      if(gabs(VALUE).LT.hundrd)ncur=ncur+1
      if(gabs(VALUE).LT.ten)ncur=ncur+1
      call putfp(VALUE,5,line,ncur)
      ncur=ncur-1
      call putchr(' ',line,ncur)
      
      if(IV0)then
      ncur=30
      if(INT.LT.10)ncur=ncur+1
      call puti(INT,line,ncur)
      ncur=ncur-1
      call putchr(' ',line,ncur)
      
      ncur=35
      if(FP.GE.zero)ncur=ncur+1
      if(gabs(FP).LT.hundrd)ncur=ncur+1
      if(gabs(FP).LT.ten)ncur=ncur+1
      call putfp(FP,5,line,ncur)
      ncur=ncur-1
      call putchr(' ',line,ncur)
      endif
      
      ncur=45
      if(.NOT.OK)call putbc('NOT FOUND IN Z-MATRIX.',22,line,ncur)
      
      call strout(Iout,line,ncur,1)
      return
      
      end
C* :1 * 
      
