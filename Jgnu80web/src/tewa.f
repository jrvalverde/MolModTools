
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tewa"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tewa.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "tewa.web"
      double precision function tewa(IBUCK,EV,NO,NV,IOPT)
      implicit none
      double precision EV,F42,Four,Half,One,Onept5,Ten,Three,Two,vewa,Ze
     &ro
      integer IBUCK,IOPT,NO,NV
      dimension EV(*)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      tewa=-vewa(IBUCK,0,Zero,EV,NO,NV,IOPT)
      
      return
      
      end
C* :1 * 
      
