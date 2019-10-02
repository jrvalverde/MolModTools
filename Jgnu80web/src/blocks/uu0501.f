
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0501"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0501.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 16 "uu0501.web"
      blockdata uu0501
      
      
      
      implicit none
      double precision Dummy
      integer Irwc,Irwc1,Irwc2,Irwc3,Irweig,Irwf,Irwgen,Irwh,Irwibf,Irwl
     &c,Irwle,Irwlp,Irwp,Irwpt,Irws,Irwt,Irwtm,Irww,Isymm,Labdck
      integer Neq
      common/bd0501/Dummy
      common/irw501/Irwgen,Irws,Irwh,Irwt,Irweig,Irwc,Irwp,Irwpt,Irwf,Ir
     &wc1,Irwc2,Irwc3,Irwtm,Irwibf(2),Irwle,Irwlc,Irwlp,Irww
      common/punlab/Labdck(4,3)
      common/isyrwf/Isymm,Neq
      data Irwgen/501/,Irws/514/,Irwh/515/,Irwt/516/,Irweig/522/,Irwc/52
     &4/
      data Irwp/528/,Irwpt/532/,Irwf/536/
      data Irwc1/2551/,Irwc2/2552/,Irwc3/2553/,Irwtm/2555/
      data Irwle/2556/,Irwlc/2557/,Irwlp/2558/,Irww/571/
      data Irwibf/508,15/
      data Labdck/'ALPH','A MO',' COE','FS  ','ALPH','A DE','NSIT','Y   
     &','EIGE','NVAL','UES ','    '/
      data Isymm/551/,Neq/565/
      
      end
C* :1 * 
      
