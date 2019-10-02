
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 jacobi"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "jacobi.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "jacobi.web"
      subroutine jacobi(N,A,EIVU,EIVR,NDIM,NVDIM,ICONTR)
      implicit none
      double precision A,absaij,abss,aii,aij,ajj,atop,avgf,c,d,differ,do
     &ne,dstop,EIVR,EIVU,eps,five,offtop,oldthr,one
      double precision pt99,s,t,test,thrsh,u,zero
      integer i,i2,ICONTR,iflag,ii,irow,j,jcol,jcol1,jj,jm1,N,NDIM,NVDIM
      
      
      
      
      
      
      
      
      
      
      logical fulmix
      dimension A(NDIM,1),EIVR(NVDIM,1),EIVU(1)
      data differ,done,eps,pt99/1.0D-5,1.0D-13,0.5D-13,0.99D0/
      data zero,one,five/0.0D0,1.0D0,5.0D0/
      
      fulmix=.TRUE.
      if(ICONTR.EQ.1)fulmix=.FALSE.
      if(N.GT.1)then
      do 50 j=1,N
      do 20 i=1,N
      EIVR(i,j)=zero
20    continue
      EIVR(j,j)=one
50    continue
      
      
      atop=zero
      do 100 j=2,N
      jm1=j-1
      do 60 i=1,jm1
      if(atop.LE.dabs(A(i,j)))atop=dabs(A(i,j))
60    continue
100   continue
      offtop=atop
      do 150 j=1,N
      if(atop.LE.dabs(A(j,j)))atop=dabs(A(j,j))
150   continue
      if(atop.GE.done)then
      if(offtop.GE.done)then
      
      
      avgf=dfloat(N*(N-1)/2)
      d=0.0D0
      do 160 jj=2,N
      do 155 ii=2,jj
      s=A(ii-1,jj)/atop
      d=s*s+d
155   continue
160   continue
      dstop=(1.D-7)*d
      
      
      thrsh=dsqrt(d/avgf)*atop
      thrsh=thrsh*pt99
      if(thrsh.LT.done)thrsh=done
      
      
170   iflag=0
      do 190 jcol=2,N
      jcol1=jcol-1
      do 185 irow=1,jcol1
      aij=A(irow,jcol)
      
      
      absaij=dabs(aij)
      if(absaij.GE.thrsh)then
      aii=A(irow,irow)
      ajj=A(jcol,jcol)
      s=ajj-aii
      abss=dabs(s)
      if(.NOT.(fulmix))then
      if((abss.LT.differ).AND.(absaij.LT.differ))goto 185
      endif
      
      
      test=eps*abss
      if(absaij.GE.test)then
      iflag=1
      
      
      test=eps*absaij
      if(abss.GT.test)then
      
      
      t=aij/s
      s=0.25D0/dsqrt(0.25D0+t*t)
      
      
      c=dsqrt(0.5D0+s)
      s=2.D0*t*s/c
      else
      s=.707106781D0
      c=s
      endif
      
      
      do 172 i=1,irow
      t=A(i,irow)
      u=A(i,jcol)
      A(i,irow)=c*t-s*u
      A(i,jcol)=s*t+c*u
172   continue
      i2=irow+2
      if(i2.LE.jcol)then
      do 174 i=i2,jcol
      t=A(i-1,jcol)
      u=A(irow,i-1)
      A(i-1,jcol)=s*u+c*t
      A(irow,i-1)=c*u-s*t
174   continue
      endif
      A(jcol,jcol)=s*aij+c*ajj
      A(irow,irow)=c*A(irow,irow)-s*(c*aij-s*ajj)
      do 176 j=jcol,N
      t=A(irow,j)
      u=A(jcol,j)
      A(irow,j)=c*t-s*u
      A(jcol,j)=s*t+c*u
176   continue
      
      
      do 178 i=1,N
      t=EIVR(i,irow)
      EIVR(i,irow)=c*t-EIVR(i,jcol)*s
      EIVR(i,jcol)=s*t+EIVR(i,jcol)*c
178   continue
      
      
      s=aij/atop
      d=d-s*s
      if(d.LE.dstop)then
      
      
      d=zero
      do 182 jj=2,N
      do 180 ii=2,jj
      s=A(ii-1,jj)/atop
      d=s*s+d
180   continue
182   continue
      dstop=(1.D-7)*d
      endif
      oldthr=thrsh
      thrsh=dsqrt(d/avgf)*atop*pt99
      if(thrsh.LT.done)thrsh=done*pt99
      if(thrsh.GT.oldthr)thrsh=oldthr
      endif
      endif
185   continue
190   continue
      if(thrsh.GE.done)then
      if(iflag.NE.1)thrsh=thrsh/five
      goto 170
      endif
      endif
      endif
      else
      EIVR(1,1)=one
      EIVU(1)=A(1,1)
      return
      endif
      
      
      do 200 j=1,N
      EIVU(j)=A(j,j)
200   continue
      return
      end
C* :1 * 
      
