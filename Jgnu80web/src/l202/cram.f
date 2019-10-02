
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cram"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cram.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 54 "cram.web"
      subroutine cram(ICODE,IA,L)
      implicit none
      integer IA,iasymb,iblnk,ibsymb,ICODE,iord,jcode,L,num
      dimension IA(*)
      dimension iasymb(2,104),ibsymb(30)
      data iasymb/' ','H','H','e','L','i','B','e',' ','B',' ','C',' ','N
     &',' ','O',' ','F','N','e','N','a','M','g','A','l','S','i',' ','P',
     &' ','S','C','l','A','r',' ','K','C','a','S','c','T','i',' ','V','C
     &','r','M','n','F','e','C','o','N','i','C','u','Z','n','G','a','G',
     &'e','A','s','S','e','B','r','K','r','R','b','S','r',' ','Y','Z','r
     &','N','b','M','o','T','c','R','u','R','h','P','d','A','g','C','d',
     &'I','n','S','n','S','b','T','e',' ','I','X','e','C','s','B','a','L
     &','a','C','e','P','r','N','d','P','m','S','m','E','u','G','d','T',
     &'b','D','y','H','o','E','r','T','m','Y','b','L','u','H','f','T','a
     &',' ','W','R','e','O','s','I','r','P','t','A','u','H','g','T','l',
     &'P','b','B','i','P','o','A','t','R','n','F','r','R','a','A','c','T
     &','h','P','a',' ','U','N','p','P','u','A','m','C','m','B','k','C',
     &'f','E','s','F','m','M','d','N','o','L','r','K','y'/
      data ibsymb/'<','>','(',')','.',1H','"','*','O','X','C','S','H','V
     &','D',',',' ','+','-',' ',10*' '/
      data iblnk/' '/
      
      
      if(ICODE.LE.99)then
      if(ICODE.GE.10)then
      L=L+1
      IA(L)=num(ICODE,1)
      endif
      L=L+1
      IA(L)=num(ICODE,2)
      return
      
      elseif(ICODE.GT.199)then
      
      jcode=ICODE-200
      L=L+1
      IA(L)=ibsymb(jcode)
      if(jcode.EQ.12)then
      
      L=L+1
      IA(L)=iord('G')
      return
      elseif(jcode.NE.17)then
      return
      endif
      else
      jcode=ICODE-100
      if(iasymb(1,jcode).NE.iblnk)then
      L=L+1
      IA(L)=iasymb(1,jcode)
      endif
      L=L+1
      IA(L)=iasymb(2,jcode)
      return
      endif
      
      L=L-2
      return
      
      end
C* :1 * 
      
