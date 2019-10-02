
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dorot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dorot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "dorot.web"
      subroutine dorot(ROTP,ROTD,ROTF,A,NB,NCOL,MAPROT,NROT)
      implicit none
      double precision A,ROTD,ROTF,ROTP
      integer irot,iupper,lower,MAPROT,NB,NCOL,NROT
      dimension MAPROT(*),A(*),ROTP(*),ROTD(*),ROTF(*)
      
      
      lower=1
      do 100 irot=1,NROT
      iupper=lower-1+MAPROT(irot)
      if(MAPROT(irot).NE.1)then
      if(MAPROT(irot).EQ.3)then
      call trnrow(A,NB,NCOL,ROTP,3,lower,iupper)
      
      elseif(MAPROT(irot).EQ.7)then
      call trnrow(A,NB,NCOL,ROTF,10,lower,iupper)
      else
      call trnrow(A,NB,NCOL,ROTD,6,lower,iupper)
      endif
      endif
      lower=iupper+1
100   continue
      return
      
      end
C* :1 * 
      
