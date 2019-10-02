
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 linout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "linout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "linout.web"
      subroutine linout(X,N,KEY)
      implicit none
      integer i,Ia,Ifilla,ij,ilower,In,Iout,Ipunch,irange,iupper,j,k,KEY
     &,maxia,N
      double precision s,X
      dimension s(8),X(4900)
      common/ia/Ia(164),Ifilla(92)
      common/io/In,Iout,Ipunch
      data maxia/164/
      
      
      
      
99001 format(10x,4(i3,12x),i3)
99002 format(' ',i3,' ',5D15.7)
99003 format(1x,'*****LINOUT-- INDEXING ARRAY NOT INITIALIZED'/1x,'*****
     &         COMMON/IA/ FILLED BY LINOUT')
      
      
      if(Ia(2).NE.1)then
      do 50 i=1,maxia
      Ia(i)=i*(i-1)/2
50    continue
      endif
      ilower=1
100   iupper=min0(ilower+4,N)
      irange=min0(iupper-ilower+1,6)
      write(Iout,99001)(j,j=ilower,iupper)
      do 200 i=1,N
      k=1
      do 150 j=ilower,iupper
      if(KEY.NE.0)then
      ij=N*(j-1)+i
      else
      
      ij=Ia(i)+j
      if(i.LT.j)ij=Ia(j)+i
      endif
      s(k)=X(ij)
      k=k+1
150   continue
      write(Iout,99002)i,(s(j),j=1,irange)
200   continue
      ilower=ilower+5
      if(N.GT.iupper)goto 100
      return
      
      end
C* :1 * 
      
