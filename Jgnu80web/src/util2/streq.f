
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 streq"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "streq.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "streq.web"
      logical function streq(I,J,LEN)
      implicit none
      integer I,ii,ij,J,ll
      integer LEN,l
      logical k
      integer getchr
      dimension I(LEN),J(LEN)
      
      
      k=.FALSE.
      if(LEN.NE.0)then
      do 50 l=1,LEN
      ll=l-1
      ii=getchr(I,ll)
      ll=l-1
      ij=getchr(J,ll)
      if(ii.NE.ij)goto 100
50    continue
      k=.TRUE.
      endif
100   streq=k
      return
      
      end
C* :1 * 
      
