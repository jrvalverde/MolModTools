
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0503"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0503.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "uu0503.web"
      blockdata uu0503
      
      
      implicit none
      double precision Big,Dummy,Four,One,Onept5,Pt5,Small,Three,Two,Zer
     &o
      integer Ieval,Ievals,Igeno,Ioc,Ioc0,Iocs,Iod,Iof1p,Iofa,Ione,Ioq,I
     &os,Iouab,Ious,Ipscr,Ipspin,Iptot,Irwibf,Isymm,Jmat
      integer Ksm,Kspin,Ksw,Lehf,Lenibf,Ligen,Lilsw,Lipcw,Locav,Lrep,Lrm
     &sd,Ls2,Ltau,Lten,Lvir,Neq,Nesk,Nest,Nest1,Nse
      integer Nsep
      logical Cmp,Rhf
      common/bd0503/Dummy
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/memlng/Locav
      common/con503/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/rwf503/Igeno,Ieval,Ios,Ione,Iofa(4),Iod(4),Ioc(4),Iocs(4),I
     &oc0(4),Ioq(4),Ious(4),Jmat(4),Iof1p(4),Ievals,Iouab(4),Iptot(2),Ip
     &spin(2)
      common/lge503/Lilsw,Lipcw,Ltau,Lehf,Lvir,Lrep,Lten,Ls2,Ligen,Lrmsd
      common/pexscr/Ipscr(4)
      common/locibf/Irwibf,Lenibf
      common/isy503/Isymm,Neq
      
      
      data Locav/4970/
      data Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small/0.D0,.5D0,1.D0,1
     &.5D0,2.D0,3.D0,4.D0,1.D36,1.D-36/
      data Ksw/'ALPH','BETA'/
      data Lilsw,Lipcw,Ltau,Lehf,Lvir,Lrep,Lten,Ls2/2,3,31,32,40,41,43,4
     &4/
      data Lrmsd/23/
      
      
      data Igeno,Ieval,Ios,Ione,Ievals/501,522,514,515,523/
      data Iptot/532,533/
      data Ipspin/534,535/
      data Iouab/540,541,540,541/
      data Iofa/536,537,538,539/
      data Iod/528,529,530,531/
      data Ioc/524,525,526,527/
      data Ipscr/532,533,534,535/
      data Iocs/554,555,556,557/
      data Ioc0/2556,2557,2558,2559/
      data Ioq/2560,2561,2562,2563/
      data Ious/2564,2565,2566,2567/
      data Jmat/2568,2569,2570,2571/
      data Iof1p/2572,2573,2574,2575/
      data Ligen/47/
      data Irwibf/508/,Lenibf/15/
      data Isymm,Neq/551,565/
      
      
      
      
      
      
      end
C* :1 * 
      
