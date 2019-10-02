
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0909"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0909.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "uu0909.web"
      blockdata uu0909
      implicit none
      double precision A00,Anorm,Cuts,Dehf,Delmax,Den,Energy,W0
      integer Iflag,Ipcyc,Isd,Maxit,Niter,Norm
      logical Davail,Savail
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      data Maxit,Delmax/30,2.D-6/
      data Cuts/1.D-6/
      end
C* :1 * 
      
