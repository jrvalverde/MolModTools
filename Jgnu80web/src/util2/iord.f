
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 iord"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "iord.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "iord.web"
      integer function iord(ICHR)
      implicit none
      integer i,ia,ia1,ib,icode,ilen,ires
      character*(*)ICHR
      character*1 bytes(4),byte(4)
      equivalence(icode,byte(1)),(ires,bytes(1))
      
      
      data ia/'A'/,ib/'B'/
      
      ia1=1
      if((ib-ia).GT.1)ia1=4
      ilen=len(ICHR)
      if(ilen.GT.4)ilen=4
      
      
      do 100 i=1,ilen
      icode=ichar(ICHR(i:i))
      bytes(i)=byte(ia1)
100   continue
      
      
      do 200 i=ilen+1,4
      icode=ichar(' ')
      bytes(i)=byte(ia1)
200   continue
      iord=ires
      return
      
      end
C* :1 * 
      
