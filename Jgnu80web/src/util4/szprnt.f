
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 szprnt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "szprnt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "szprnt.web"
      subroutine szprnt(OP,VAL,LEN,TYPE)
      implicit none
      integer i,In,Iout,Ipunch,LEN,minus,nb,OP,string,tab,TYPE
      save
      double precision VAL
      dimension string(40)
      integer blank(1)
      common/io/In,Iout,Ipunch
      data blank(1)/'    '/
      data minus/'-'/
      
      
      
      
      
      
99001 format('  UNRECOGNIZED VALUE OF TYPE IN "SZPRNT": ',i5)
99002 format('  BAD OP-CODE IN "SZPRNT":',i5)
      
      
      if(OP.LT.0.OR.OP.GT.2)write(Iout,99002)TYPE
      if(OP.LT.0.OR.OP.GT.2)call lnk1e
      i=OP+1
      if(i.EQ.2)then
      
      nb=tab
      if(TYPE.EQ.1.OR.TYPE.EQ.0)then
      call decchr(VAL,string,nb)
      tab=tab+6
      
      elseif(TYPE.EQ.2)then
      call putfp(VAL,5,string,nb)
      nb=nb-1
      call puticr(blank,string,nb)
      tab=tab+10
      
      elseif(iabs(TYPE).EQ.3)then
      if(TYPE.EQ.-3)call puticr(minus,string,nb)
      call putb(VAL,LEN,string,nb)
      tab=tab+10
      
      elseif(TYPE.NE.4)then
      
      write(Iout,99001)TYPE
      call lnk1e
      else
      nb=tab
      call putb(VAL,LEN,string,nb)
      tab=tab+6
      endif
      
      return
      elseif(i.NE.3)then
      
      nb=0
      do 50 i=1,10
      call putb(blank,4,string,nb)
50    continue
      nb=0
      tab=0
      return
      endif
      
      if(nb.NE.0)call strout(Iout,string,nb,1)
      if(nb.EQ.0)write(Iout,99003)
      
99003 format(' ','  SZPRNT OUTPUTS NULL STRING.')
      
      tab=0
      nb=0
      do 100 i=1,40
      call putb(blank,4,string,nb)
100   continue
      nb=0
      return
      
      end
C* :1 * 
      
