
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 tewb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "tewb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "tewb.web"
      double precision function tewb(IBUCK,EVA,EVB,IOPT)
      implicit none
      double precision EVA,EVB,F42,Four,Half,One,Onept5,Ten,Three,Two,ve
     &wb,Zero
      integer IBUCK,IOPT
      dimension EVA(*),EVB(*)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      tewb=-vewb(IBUCK,0,Zero,EVA,EVB,IOPT)
      
      return
      
      end
C* :1 * 
      
