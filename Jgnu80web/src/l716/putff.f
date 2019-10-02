
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 putff"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "putff.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "putff.web"
      subroutine putff(NZ,LBL,LALPHA,LBETA,NPARM,NVAR,FFX,FRCNST,FTMP1,F
     &TMP2,FFTMP,IPRINT)
      implicit none
      double precision FFTMP,FFX,FRCNST,FTMP1,FTMP2
      integer i,ii,ij,In,Iout,IPRINT,Ipunch,j,LALPHA,LBETA,LBL,lind,NPAR
     &M,NVAR,nvartt,NZ
      dimension LBL(*),LALPHA(*),LBETA(*),FFX(*),FRCNST(*)
      dimension FTMP1(*),FTMP2(*),FFTMP(*)
      common/io/In,Iout,Ipunch
      
      
      
      
      
      
99001 format(' FROM PUTFF, CONTENTS OF FRCNST:')
      
      lind(i,j)=(i*(i-1))/2+j
      
      nvartt=(NVAR*(NVAR+1))/2
      call aclear(nvartt,FRCNST)
      
      do 200 i=1,NPARM
      do 50 j=1,i
      ij=lind(i,j)
      FTMP1(j)=FFX(ij)
50    continue
      do 100 j=i,NPARM
      ij=lind(j,i)
      FTMP1(j)=FFX(ij)
100   continue
      call putf(NZ,LBL,LALPHA,LBETA,NPARM,NVAR,FTMP1,FTMP2,IPRINT-1)
      do 150 j=1,NVAR
      ij=i+NPARM*(j-1)
      FFTMP(ij)=FTMP2(j)
150   continue
200   continue
      
      do 300 i=1,NVAR
      ii=NPARM*(i-1)
      call putf(NZ,LBL,LALPHA,LBETA,NPARM,NVAR,FFTMP(ii+1),FTMP2,IPRINT-
     &1)
      do 250 j=1,i
      ij=lind(i,j)
      FRCNST(ij)=FRCNST(ij)+FTMP2(j)
250   continue
300   continue
      
      if(IPRINT.GT.0)then
      write(Iout,99001)
      call ltoutd(NVAR,FRCNST,1)
      endif
      
      return
      
      end
C* :1 * 
      
