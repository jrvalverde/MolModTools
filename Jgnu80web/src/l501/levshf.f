
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 levshf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "levshf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "levshf.web"
      subroutine levshf(FA,F,C,D,NBASIS,MAX,NOCC,VSHIFT,IRWLC,IRWS,IRWSC
     &R)
      implicit none
      double precision C,D,F,FA,VSHIFT
      integer i,ij,IRWLC,IRWS,IRWSCR,j,ji,jj,MAX,NBASIS,NOCC,ntt
      dimension F(*),FA(*),C(*),D(*)
      ntt=(NBASIS*(NBASIS+1))/2
      
      
      call tread(IRWLC,C,MAX,MAX,NBASIS,NBASIS,0)
      call fvmul(NBASIS,FA,C,F,D)
      call vdagt(NBASIS,C,F,F,D)
      do 100 j=1,NBASIS
      if(j.GT.NOCC)then
      jj=(j*(j+1))/2
      F(jj)=F(jj)+VSHIFT
      endif
100   continue
      
      
      call twrite(IRWSCR,F,ntt,1,ntt,1,0)
      call tread(IRWS,FA,ntt,1,ntt,1,0)
      call tread(IRWLC,C,MAX,MAX,NBASIS,NBASIS,0)
      call fvmul(NBASIS,FA,C,F,D)
      do 200 i=1,NBASIS
      do 150 j=1,NBASIS
      ij=(j-1)*NBASIS+i
      ji=(i-1)*NBASIS+j
      C(ji)=F(ij)
150   continue
200   continue
      call tread(IRWSCR,FA,ntt,1,ntt,1,0)
      call fvmul(NBASIS,FA,C,F,D)
      call vdagt(NBASIS,C,F,F,D)
      do 300 i=1,ntt
      FA(i)=F(i)
300   continue
      return
      end
C* :1 * 
      
