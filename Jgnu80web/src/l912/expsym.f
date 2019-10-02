
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 expsym"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "expsym.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "expsym.web"
      subroutine expsym(N,IBUC1,IBUC2)
      implicit none
      integer i,IBUC1,IBUC2,if,il,ilp,ind1,ind2,ip,j,jndex,leng,lnnext,m
     &ax,max1,Mdv,N,n2,n3
      double precision V
      common/v/V(20000),Mdv
      
      
      
      
      
      call track('EXPSYM')
      
      if(N.LE.0)return
      n3=N*(N+1)/2
      n2=N*N
      if(n2.GT.Mdv)then
      
      max=Mdv-N
      max1=max+1
      call fileio(2,-IBUC1,0,0,0)
      il=0
      else
      leng=n3
      call fileio(2,-IBUC1,0,0,0)
      call fileio(2,IBUC1,leng,V,0)
      call lsexs(V,N)
      leng=n2
      call fileio(1,-IBUC2,0,0,0)
      call fileio(1,IBUC2,leng,V,0)
      goto 400
      endif
100   if=il+1
      i=if
      leng=N-i+1
200   lnnext=leng+N-i
      if(lnnext.GT.max.OR.i.EQ.N)then
      
      if(i.EQ.0)call lnk1e
      il=i
      call fileio(2,IBUC1,leng,V,0)
      leng=N-if+1
      do 250 i=if,il
      ind1=i-N
      ind2=max
      do 220 j=if,i
      ind1=ind1+N-j+1
      ind2=ind2+1
      V(ind2)=V(ind1)
220   continue
      if(i.NE.N)then
      ip=i+1
      do 230 j=ip,N
      ind1=ind1+1
      ind2=ind2+1
      V(ind2)=V(ind1)
230   continue
      endif
      jndex=N*(i-1)+if-1
      call fileio(1,-IBUC2,leng,V(max1),jndex)
      if(i.EQ.N)goto 400
250   continue
      ilp=il+1
      leng=il-if+1
      do 300 i=ilp,N
      ind1=i-N
      ind2=max
      do 260 j=if,il
      ind1=ind1+N-j+1
      ind2=ind2+1
      V(ind2)=V(ind1)
260   continue
      jndex=N*(i-1)+if-1
      call fileio(1,-IBUC2,leng,V(max1),jndex)
300   continue
      if(il.LT.N)goto 100
      else
      i=i+1
      leng=lnnext
      goto 200
      endif
      
400   return
      
      end
C* :1 * 
      
