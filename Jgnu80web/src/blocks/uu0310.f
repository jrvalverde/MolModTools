
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0310"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0310.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "uu0310.web"
      blockdata uu0310
      implicit none
      double precision Binom,Dummy,F15,Five,Four,One,Three,Two,Xign,Zero
      integer Indf,Lhold,Mhold,Nhold
      integer ubound,ulpure
      common/bd0310/Dummy
      common/con310/Zero,One,Two,Three,Four,Five,F15
      common/indf/Indf(16)
      common/binom/Binom(28)
      common/hold/Lhold(20),Mhold(20),Nhold(20)
      common/xign/Xign(13)
      data Zero/0.0D0/,One/1.0D0/,Two/2.0D0/,Three/3.0D0/,Four/4.0D0/
      data Five/5.0D0/,F15/15.0D0/
      data Indf/1,2,4,7,11,13,16,20,25,28,32,37,43,47,52,58/
      data Binom/4*1.0D0,2.0D0,2*1.0D0,2*3.0D0,2*1.0D0,4.0D0,6.0D0,4.0D0
     &,2*1.0D0,5.0D0,2*10.0D0,5.0D0,2*1.0D0,6.0D0,15.0D0,20.0D0,15.0D0,6
     &.0D0,1.0D0/
      data Lhold/0,1,0,0,2,0,0,1,1,0,3,0,0,1,2,2,1,0,0,1/
      data Mhold/0,0,1,0,0,2,0,1,0,1,0,3,0,2,1,0,0,1,2,1/
      data Nhold/0,0,0,1,0,0,2,0,1,1,0,0,3,0,0,1,2,2,1,1/
      data Xign/1.0D0,-1.0D0,1.0D0,-1.0D0,1.0D0,-1.0D0,1.0D0,-1.0D0,1.0D
     &0,-1.0D0,1.0D0,-1.0D0,1.0D0/
      
      end
C* :1 * 
      
