
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 trnrow"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "trnrow.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "trnrow.web"
      subroutine trnrow(A,NB,NCOL,TRAN,NTR,LOWER,IUPPER)
      implicit none
      double precision A,aloc,TRAN,zero
      integer i,icol,ir,irm,irow,IUPPER,LOWER,n,NB,NCOL,NTR
      dimension A(NB,NCOL),TRAN(NTR,NTR)
      dimension aloc(7)
      data zero/0.0D0/
      
      
      n=IUPPER-LOWER+1
      do 200 icol=1,NCOL
      ir=0
      do 50 irow=LOWER,IUPPER
      ir=ir+1
      aloc(ir)=zero
      irm=LOWER-1
      do 20 i=1,n
      irm=irm+1
      aloc(ir)=aloc(ir)+TRAN(ir,i)*A(irm,icol)
20    continue
50    continue
      ir=0
      do 100 i=LOWER,IUPPER
      ir=ir+1
      A(i,icol)=aloc(ir)
100   continue
200   continue
      return
      
      end
C* :1 * 
      
