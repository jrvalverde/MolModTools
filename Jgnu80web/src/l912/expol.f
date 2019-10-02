
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 expol"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "expol.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "expol.web"
      subroutine expol
      implicit none
      double precision A00,A0old,A0s,Anorm,Cuts,d1,d2,d3,De1,Dehf,Delmax
     &,Den,Dep,Energy,F42,Four,Half,One,Onept5,Q1
      double precision qe,Qep,small,Ten,Three,threxp,Two,W0,Zero
      integer Idummy,Iflag,In,Inr,Iout,Ipcyc,Iprint,Ipunch,Isd,Maxit,Nit
     &er,Norm
      logical Davail,Savail
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/io/In,Iout,Ipunch
      common/wtoa/De1,Q1,A0s
      common/extrap/Dep(2),Qep,A0old,Inr,Idummy
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      common/print/Iprint
      data threxp/.10D0/,small/1.D-4/
      
      
      
      
      
      
      
99001 format(' 1/(1-Q)=',d17.8,8x,'DE(EXT)=',d22.8)
99002 format(' EXTRAPOLATION PERFORMED')
      
      call track('EXPOL ')
      
      Inr=Inr+1
      Q1=One
      d2=Dep(2)
      d3=De1
      if((Inr.GE.3).AND.(dabs(De1).GE.small))then
      d1=Dep(1)
      
      qe=(d3-d2)/(d2-d1)
      if(dabs(qe-Qep).LE.threxp)then
      Q1=One/(One-qe)
      De1=d2+Q1*(d3-d2)
      Inr=0
      write(Iout,99002)
      if(Iprint.GT.0)write(Iout,99001)Q1,De1
      
      A00=Q1*(A00-A0old)+A0old
      endif
      
      Qep=qe
      endif
      
      Dep(1)=d2
      Dep(2)=d3
      A0old=A0s
      
      return
      
      end
C* :1 * 
      
