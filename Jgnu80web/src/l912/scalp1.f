
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 scalp1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "scalp1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "scalp1.web"
      subroutine scalp1(IBUC1,IBUC2,A,LPAIR,NPAIRS)
      implicit none
      double precision A,a0,F42,Four,Half,One,Onept5,Ten,Three,Two,V,Zer
     &o
      integer iab,IBUC1,IBUC2,ij,leng,LPAIR,Mdv,mdv11,NPAIRS
      dimension A(*)
      common/v/V(20000),Mdv
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      
      
      call track('SCALP1')
      
      if(NPAIRS.NE.0.AND.LPAIR.NE.0)then
      
      call fileio(2,-IBUC1,0,0,0)
      leng=LPAIR
      if(IBUC1.EQ.IBUC2)then
      
      do 20 ij=1,NPAIRS
      a0=Zero
      call fileio(2,IBUC1,leng,V,0)
      do 10 iab=1,LPAIR
      a0=a0+V(iab)**2
10    continue
      A(ij)=a0
20    continue
      else
      
      mdv11=LPAIR+1
      call fileio(2,-IBUC2,0,0,0)
      do 40 ij=1,NPAIRS
      a0=Zero
      call fileio(2,IBUC1,leng,V,0)
      call fileio(2,IBUC2,leng,V(mdv11),0)
      do 30 iab=1,LPAIR
      a0=a0+V(iab)*V(LPAIR+iab)
30    continue
      A(ij)=a0
40    continue
      endif
      endif
      
      return
      
      end
C* :1 * 
      
