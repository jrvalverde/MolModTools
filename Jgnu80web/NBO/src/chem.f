
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 chem"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "chem.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "chem.web"
      subroutine chem(NAT,NATOMS,LISTA,NL,ISTR)
      implicit none
      integer iat,iblnk,ibyte,ic,ih,il,ileft,inum,iright,ISTR,itemp,jat,
     &LISTA,MAXD,nameat,NAT,NATOMS,nd,NL
      dimension LISTA(NATOMS,2),ISTR(80)
      
      parameter(MAXD=4)
      dimension inum(MAXD),ibyte(4)
      data ic,ih,iblnk,ileft,iright/'C','H',' ','(',')'/
      
      
      
      do 100 iat=1,NAT
      LISTA(iat,1)=nameat(LISTA(iat,1))
100   continue
      
      
      do 200 iat=1,NAT-1
      do 150 jat=1,NAT-iat
      if(LISTA(jat,1).GT.LISTA(jat+1,1))then
      itemp=LISTA(jat,1)
      LISTA(jat,1)=LISTA(jat+1,1)
      LISTA(jat+1,1)=itemp
      itemp=LISTA(jat,2)
      LISTA(jat,2)=LISTA(jat+1,2)
      LISTA(jat+1,2)=itemp
      endif
150   continue
200   continue
      
      
      
      NL=1
      ISTR(NL)=ileft
      do 300 iat=1,NAT
      call debyte(LISTA(iat,1),ibyte)
      if(ibyte(1).EQ.iblnk.AND.ibyte(2).EQ.ic)then
      NL=NL+1
      ISTR(NL)=ic
      if(LISTA(iat,2).NE.1)then
      call idigit(LISTA(iat,2),inum,nd,MAXD)
      do 210 il=1,nd
      NL=NL+1
      ISTR(NL)=inum(il)
210   continue
      endif
      LISTA(iat,2)=0
      endif
300   continue
      
      
      do 400 iat=1,NAT
      call debyte(LISTA(iat,1),ibyte)
      if(ibyte(1).EQ.iblnk.AND.ibyte(2).EQ.ih)then
      NL=NL+1
      ISTR(NL)=ih
      if(LISTA(iat,2).NE.1)then
      call idigit(LISTA(iat,2),inum,nd,MAXD)
      do 310 il=1,nd
      NL=NL+1
      ISTR(NL)=inum(il)
310   continue
      endif
      LISTA(iat,2)=0
      endif
400   continue
      
      
      do 500 iat=1,NAT
      if(LISTA(iat,2).NE.0)then
      call debyte(LISTA(iat,1),ibyte)
      if(ibyte(1).NE.iblnk)then
      NL=NL+1
      ISTR(NL)=ibyte(1)
      endif
      if(ibyte(2).NE.iblnk)then
      NL=NL+1
      ISTR(NL)=ibyte(2)
      endif
      if(LISTA(iat,2).NE.1)then
      call idigit(LISTA(iat,2),inum,nd,MAXD)
      do 410 il=1,nd
      NL=NL+1
      ISTR(NL)=inum(il)
410   continue
      endif
      LISTA(iat,2)=0
      endif
500   continue
      NL=NL+1
      ISTR(NL)=iright
      return
      end
C* :1 * 
      
