
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uucids"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uucids.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "uucids.web"
      blockdata uucids
      implicit none
      double precision Dummy
      integer Icivar,Iextrp,Igeno,Ilincc,Inforb,Iw1sav,Iw2sav,La0,Lanorm
     &,Lccd,Lcisd4,Lcivar,Ldq4,Lecid,Lecids,Lehf,Lenrgy,Lextrp,Ligen,Lis
     &d
      integer Llccd,Llccd4,Llincc,Lmp2,Lmp3,Lnforb,Lscc1,Lsdq4,Lsdtq4,Lv
     &ar1,Mdim
      common/bdcids/Dummy
      common/mdmax/Mdim
      common/locgen/Lehf,Lmp2,Lmp3,Ldq4,Lsdq4,Lsdtq4,Lecid,Lecids,Lscc1,
     &Lccd,Llccd,Lvar1,Lcisd4,Llccd4,Lenrgy,Lanorm,La0,Lisd
      common/rwfp/Igeno,Ligen,Inforb,Lnforb,Icivar,Lcivar,Iextrp,Lextrp
      common/loclin/Ilincc,Llincc
      common/wsav/Iw1sav,Iw2sav
      data Mdim/80/
      data Igeno,Ligen/501,47/
      data Inforb,Lnforb/545,14/
      data Icivar,Lcivar/601,12/
      data Iextrp,Lextrp/602,5/
      data Ilincc,Llincc/603,52/
      data Lehf,Lmp2,Lmp3,Ldq4,Lsdq4,Lsdtq4/32,33,34,38,39,35/
      data Lecid,Lecids,Lscc1,Lccd,Llccd/36,37,13,40,12/
      data Lvar1,Lcisd4,Llccd4,Lenrgy/11,18,17,43/
      data Lanorm,La0,Lisd/42,47,6/
      data Iw1sav,Iw2sav/101,102/
      
      
      
      
      
      
      
      
      
      
      
      end
C* :1 * 
      
