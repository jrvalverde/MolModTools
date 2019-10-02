
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 carpol"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "carpol.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "carpol.web"
      subroutine carpol(AR,AI,R,PHI)
      implicit none
      double precision a0,a1,AI,AR,Constd,Four,gsqrt,One,PHI,Pt5,R,Three
     &,Two,Zero
      common/const/Zero,Pt5,One,Two,Three,Four,Constd(3)
      
      a0=AR
      a1=AI
      R=gsqrt(a0**2+a1**2)
      if(R.LT.1.D-14)then
      
      R=Zero
      PHI=Zero
      else
      PHI=datan2(a1,a0)
      endif
      return
      
      end
C* :1 * 
      
