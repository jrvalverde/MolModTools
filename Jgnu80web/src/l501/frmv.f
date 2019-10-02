
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmv"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmv.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "frmv.web"
      subroutine frmv(A,B,NBASIS,AA,BB,IRWTM)
      implicit none
      double precision A,AA,B,BB,gsqrt,one,temp
      integer i,In,Iout,Ipunch,irws,IRWTM,j,jx,NBASIS,ntt
      dimension A(*),B(NBASIS,NBASIS),AA(*),BB(*)
      common/io/In,Iout,Ipunch
      data irws/14/
      data one/1.0D0/
      
      
99001 format(39H1INTEGRITY OF OVERLAP CHALLENGED IN SCF,i4//12H0EIGENVAL
     &UES/(1x,i3,e20.13))
99002 format(/15H0OVERLAP MATRIX/)
      
      ntt=(NBASIS*(NBASIS+1))/2
      call tread(irws,A,ntt,1,ntt,1,0)
      
      
      call diagd(A,B,BB,NBASIS,AA,A(ntt+1),NBASIS,.FALSE.)
      
      do 100 j=1,NBASIS
      if(BB(j).LE.0)then
      
      write(Iout,99001)j,(jx,BB(jx),jx=1,NBASIS)
      write(Iout,99002)
      call tread(irws,A,ntt,1,ntt,1,0)
      call ltoutd(NBASIS,A,1)
      call lnk1e
      endif
      
      temp=one/gsqrt(BB(j))
      do 50 i=1,NBASIS
      B(i,j)=B(i,j)*temp
50    continue
100   continue
      call twrite(IRWTM,B,NBASIS,NBASIS,NBASIS,NBASIS,0)
      
      
      return
      
      end
C* :1 * 
      
