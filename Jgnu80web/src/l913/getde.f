
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getde"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getde.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "getde.web"
      subroutine getde(METHOD,ISCHEM)
      implicit none
      double precision A00,A0old,A0s,Anorm,Cuts,De1,Dehf,Delmax,Den,Dep,
     &Energy,F42,Four,Half,One,Onept5,Q1,Qep,Ten,Three
      double precision Two,W0,Zero
      integer Idummy,Iflag,In,Inr,Iout,Ipcyc,Iprint,Ipunch,ISCHEM,Isd,Ma
     &xit,METHOD,Niter,Norm
      logical Davail,Savail
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/wtoa/De1,Q1,A0s
      common/io/In,Iout,Ipunch
      common/extrap/Dep(2),Qep,A0old,Inr,Idummy
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      common/print/Iprint
      
      
      
      
      
      
99001 format(' DE=',d20.8)
      
      call track('METHOD')
      
      A0s=A00
      if(ISCHEM.EQ.2)then
      
      De1=W0/A00
      elseif(ISCHEM.EQ.3)then
      
      De1=Zero
      else
      
      if(METHOD.LE.1)De1=W0/A00
      if(METHOD.GT.1)De1=Zero
      endif
      
      if(Iprint.GT.0)write(Iout,99001)De1
      
      return
      
      end
C* :1 * 
      
