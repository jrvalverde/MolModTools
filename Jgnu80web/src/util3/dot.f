
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "dot.web"
      subroutine dot(NBASIS,A,B,AR,AI)
      implicit none
      double precision A,a0i,a0r,AI,AR,B,b0i,b0r,Big,Four,One,Onept5,Pt5
     &,Small,Three,Two,Zero
      integer i,Ksm,Kspin,Ksw,m1,Mdim,Mdsq,Mshifs,Mtt,NBASIS,Nesk,Nest,N
     &est1,Nse,Nsep,Ntt
      logical Cmp,Rhf
      dimension A(Mdim,*),B(Mdim,*)
      common/const/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      
      
      AR=Zero
      AI=Zero
      if(.NOT.(Cmp))then
      
      do 50 i=1,NBASIS
      AR=AR+A(i,1)*B(i,1)
50    continue
      return
      endif
      
      m1=Mdim+1
      do 100 i=1,NBASIS
      a0r=A(i,1)
      a0i=A(i,m1)
      b0r=B(i,1)
      b0i=B(i,m1)
      AR=AR+a0r*b0r+a0i*b0i
      AI=AI+a0r*b0i-a0i*b0r
100   continue
      return
      
      end
C* :1 * 
      
