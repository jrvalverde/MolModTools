
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0502"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0502.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 17 "uu0502.web"
      blockdata uu0502
      implicit none
      double precision Dummy
      integer Irwc1,Irwc2,Irwc3,Irwca,Irwcb,Irweig,Irwfa,Irwfb,Irwgen,Ir
     &wh,Irwibf,Irwpa,Irwpb,Irwps,Irwpt,Irws,Irwt,Irwtm,Irwur,Lenibf
      integer Maxnbf,Nttmax
      character*6 Word
      common/bd0502/Dummy
      common/word/Word(4)
      common/max502/Maxnbf,Nttmax
      common/irw502/Irwgen,Irweig,Irwca,Irwcb,Irwpa,Irwpb,Irwpt,Irwps,Ir
     &wfa,Irwfb,Irwur,Irws,Irwh,Irwt,Irwtm,Irwc1,Irwc2,Irwc3,Irwibf,Leni
     &bf
      data Maxnbf/75/,Nttmax/2850/
      data Irwibf/508/,Lenibf/15/
      data Word/6HALPHA-,6H BETA-,6HTOTAL-,6H SPIN-/
      data Irwgen/501/,Irweig/522/,Irwca/524/,Irwcb/526/,Irwpa/528/,Irwp
     &b/530/,Irwpt/532/,Irwps/534/,Irwfa/536/,Irwfb/538/,Irwur/540/,Irws
     &/514/,Irwh/515/,Irwt/516/,Irwtm/2555/,Irwc1/2551/,Irwc2/2552/,Irwc
     &3/2553/
      end
C* :1 * 
      
