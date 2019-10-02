
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 minv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "minv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "minv.web"
      subroutine minv(A,N,D,L,M)
      implicit none
      double precision A,biga,D,gabs,hold,one,zero
      integer i,ij,ik,iz,j,ji,jk,jp,jq,jr,k,ki,kj,kk,L,M,N,nk
      dimension A(*),L(*),M(*)
      data one,zero/1.0D0,0.0D0/
      
      
      
      
      D=1.0
      nk=-N
      do 200 k=1,N
      nk=nk+N
      L(k)=k
      M(k)=k
      kk=nk+k
      biga=A(kk)
      do 50 j=k,N
      iz=N*(j-1)
      do 20 i=k,N
      ij=iz+i
      if(gabs(biga).LT.gabs(A(ij)))then
      biga=A(ij)
      L(k)=i
      M(k)=j
      endif
20    continue
50    continue
      
      
      j=L(k)
      if(j.GT.k)then
      ki=k-N
      do 60 i=1,N
      ki=ki+N
      hold=-A(ki)
      ji=ki-k+j
      A(ki)=A(ji)
      A(ji)=hold
60    continue
      endif
      
      
      i=M(k)
      if(i.GT.k)then
      jp=N*(i-1)
      do 80 j=1,N
      jk=nk+j
      ji=jp+j
      hold=-A(jk)
      A(jk)=A(ji)
      A(ji)=hold
80    continue
      endif
      
      
      if(biga.NE.0)then
      
      do 100 i=1,N
      if(i.NE.k)then
      ik=nk+i
      A(ik)=A(ik)/(-biga)
      endif
100   continue
      
      
      do 120 i=1,N
      ik=nk+i
      hold=A(ik)
      ij=i-N
      do 110 j=1,N
      ij=ij+N
      if(i.NE.k)then
      if(j.NE.k)then
      kj=ij-i+k
      A(ij)=hold*A(kj)+A(ij)
      endif
      endif
110   continue
120   continue
      
      
      kj=k-N
      do 140 j=1,N
      kj=kj+N
      if(j.NE.k)A(kj)=A(kj)/biga
140   continue
      
      
      D=D*biga
      
      A(kk)=one/biga
      else
      D=zero
      return
      endif
200   continue
      
      
      k=N
300   k=(k-1)
      if(k.GT.0)then
      i=L(k)
      if(i.GT.k)then
      jq=N*(k-1)
      jr=N*(i-1)
      do 320 j=1,N
      jk=jq+j
      hold=A(jk)
      ji=jr+j
      A(jk)=-A(ji)
      A(ji)=hold
320   continue
      endif
      j=M(k)
      if(j.GT.k)then
      ki=k-N
      do 340 i=1,N
      ki=ki+N
      hold=A(ki)
      ji=ki-k+j
      A(ki)=-A(ji)
      A(ji)=hold
340   continue
      endif
      goto 300
      endif
      
      return
      
      end
C* :1 * 
      
