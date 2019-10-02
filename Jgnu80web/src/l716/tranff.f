
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tranff"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tranff.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 34 "tranff.web"
      subroutine tranff(MAXNZ,NZ,IANZ,IZ,BL,ALPHA,BETA,NPARM,FX,FFX,FTMP
     &1,FTMP2,FFTMP,MXCORE,CORE,IDUMP,TOANG)
      implicit none
      double precision ALPHA,BETA,BL,CORE,del,del1,FFTMP,FFX,FTMP1,FTMP2
     &,FX,pt5,TOANG
      integer i,i1,i2,i3,i4,i5,i6,i7,i8,IANZ,IDUMP,iend,igeig,ii,ij,In,I
     &out,Ipunch,ipz,IZ
      integer j,jpz,lind,MAXNZ,MXCORE,nnprm,NPARM,NZ,nz3
      dimension IANZ(*),IZ(MAXNZ,4),BL(*),ALPHA(*),BETA(*)
      dimension FX(*),FFX(*),FTMP1(*),FTMP2(*),FFTMP(*)
      dimension CORE(*)
      common/io/In,Iout,Ipunch
      data pt5/0.5D0/
      data del1/0.00001D0/
      
      
      
      
      
      
      
      
      
99001 format(' ALLOCATION FAILURE IN TRANFF.'/'  NEEDS:',i6,'  HAS:',i6)
      
      lind(i,j)=(i*(i-1))/2+j
      
      nz3=3*NZ
      nnprm=(NPARM*(NPARM+1))/2
      
      
      i1=1
      i2=i1+4*NPARM
      i3=i2+3*4*NPARM
      i4=i3+NPARM*NPARM
      i5=i4+NZ*5
      i6=i5+3*NZ
      i7=i6+3*NZ
      i8=i7+NPARM
      iend=i8+NPARM-1
      if(iend.GT.MXCORE)then
      write(Iout,99001)iend,MXCORE
      call lnk1e
      endif
      igeig=0
      
      call formbg(MAXNZ,NZ,IANZ,IZ,BL,ALPHA,BETA,NPARM,igeig,CORE(i2),CO
     &RE(i1),CORE(i3),CORE(i4),CORE(i5),CORE(i6),CORE(i7),CORE(i8),IDUMP
     &,TOANG)
      
      
      do 200 i=1,nz3
      do 50 j=1,i
      ij=lind(i,j)
      FTMP1(j)=FFX(ij)
50    continue
      do 100 j=i,nz3
      ij=lind(j,i)
      FTMP1(j)=FFX(ij)
100   continue
      call tranf(NPARM,NZ,IANZ,FTMP1,FTMP2,CORE(i1),CORE(i2),CORE(i3),CO
     &RE(i7))
      do 150 j=1,NPARM
      ij=i+nz3*(j-1)
      FFTMP(ij)=FTMP2(j)
150   continue
200   continue
      
      call aclear(nnprm,FFX)
      do 400 i=1,NPARM
      ii=nz3*(i-1)
      call tranf(NPARM,NZ,IANZ,FFTMP(ii+1),FTMP2,CORE(i1),CORE(i2),CORE(
     &i3),CORE(i7))
      do 250 j=1,i
      ij=lind(i,j)
      FFX(ij)=FFX(ij)+FTMP2(j)
250   continue
      do 300 j=i,NPARM
      ij=lind(j,i)
      FFX(ij)=FFX(ij)+FTMP2(j)
300   continue
400   continue
      
      
      do 600 i=1,NPARM
      if(i.LE.(NZ-1))then
      ipz=i+1
      jpz=1
      del=del1
      
      elseif(i.GT.(2*NZ-3))then
      
      ipz=i-2*NZ+6
      jpz=3
      del=del1
      else
      ipz=i-NZ+3
      jpz=2
      del=del1
      endif
      
      call zmmod(BL,ALPHA,BETA,ipz,jpz,+del)
      call formbg(MAXNZ,NZ,IANZ,IZ,BL,ALPHA,BETA,NPARM,igeig,CORE(i2),CO
     &RE(i1),CORE(i3),CORE(i4),CORE(i5),CORE(i6),CORE(i7),CORE(i8),IDUMP
     &,TOANG)
      call tranf(NPARM,NZ,IANZ,FX,FTMP1,CORE(i1),CORE(i2),CORE(i3),CORE(
     &i7))
      
      call zmmod(BL,ALPHA,BETA,ipz,jpz,-del-del)
      call formbg(MAXNZ,NZ,IANZ,IZ,BL,ALPHA,BETA,NPARM,igeig,CORE(i2),CO
     &RE(i1),CORE(i3),CORE(i4),CORE(i5),CORE(i6),CORE(i7),CORE(i8),IDUMP
     &,TOANG)
      call tranf(NPARM,NZ,IANZ,FX,FTMP2,CORE(i1),CORE(i2),CORE(i3),CORE(
     &i7))
      
      call zmmod(BL,ALPHA,BETA,ipz,jpz,+del)
      
      do 450 j=1,NPARM
      FTMP1(j)=(FTMP1(j)-FTMP2(j))/(del1+del1)
450   continue
      
      do 500 j=1,i
      ij=lind(i,j)
      FFX(ij)=FFX(ij)-FTMP1(j)
500   continue
      do 550 j=i,NPARM
      ij=lind(j,i)
      FFX(ij)=FFX(ij)-FTMP1(j)
550   continue
600   continue
      
      do 700 ij=1,nnprm
      FFX(ij)=FFX(ij)*pt5
700   continue
      
      return
      
      end
C* :1 * 
      
