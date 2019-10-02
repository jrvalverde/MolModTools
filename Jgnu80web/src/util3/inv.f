
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 inv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "inv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "inv.web"
      subroutine inv(A,N,IS,IAD1,IAD2,D,MDM)
      implicit none
      double precision A,b,D,e,gabs,gmax1,one,small,zero
      integer i,IAD1,IAD2,ima,In,Iout,Ipunch,IS,j,k,l,m,MDM,N
      dimension A(MDM,MDM),IS(2,MDM),IAD1(MDM),IAD2(MDM),D(MDM)
      common/io/In,Iout,Ipunch
      data zero/0.0D0/,one/1.0D0/,small/1.0D-20/
      
      
      
      
99001 format(' WARNING FROM INV: MATRIX IS SINGULAR')
      do 100 l=1,N
      IS(1,l)=0
      IS(2,l)=0
100   continue
      do 400 ima=1,N
      b=zero
      do 150 l=1,N
      do 120 m=1,N
      if(IS(1,l).NE.1.AND.IS(2,m).NE.1)then
      e=gabs(A(l,m))
      if(e.GE.b)then
      i=l
      k=m
      endif
      b=gmax1(b,e)
      endif
120   continue
150   continue
      IS(1,i)=1
      IS(2,k)=1
      IAD1(k)=i
      IAD2(i)=k
      b=A(i,k)
      if(gabs(b).LT.small)goto 900
      A(i,k)=one/b
      do 200 l=1,N
      if(l.NE.k)A(i,l)=-A(i,l)/b
200   continue
      do 250 l=1,N
      do 220 m=1,N
      if(l.NE.i.AND.m.NE.k)A(l,m)=A(l,m)+A(l,k)*A(i,m)
220   continue
250   continue
      do 300 l=1,N
      if(l.NE.i)A(l,k)=A(l,k)/b
300   continue
400   continue
      do 600 l=1,N
      do 450 j=1,N
      k=IAD1(j)
      D(j)=A(k,l)
450   continue
      do 500 j=1,N
      A(j,l)=D(j)
500   continue
600   continue
      do 800 l=1,N
      do 650 j=1,N
      k=IAD2(j)
      D(j)=A(l,k)
650   continue
      do 700 j=1,N
      A(l,j)=D(j)
700   continue
800   continue
      return
      
900   write(Iout,99001)
      call lnk1e
      return
      
      end
C* :1 * 
      
