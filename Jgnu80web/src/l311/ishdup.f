
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ishdup"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ishdup.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "ishdup.web"
      integer function ishdup(ISHELL,JSHELL,KSHELL,LSHELL)
      implicit none
      integer ij,ikjl,ISHELL,JSHELL,kl,KSHELL,LSHELL
      
      
      ij=0
      kl=0
      ikjl=0
      if(ISHELL.EQ.JSHELL)ij=1
      if(KSHELL.EQ.LSHELL)kl=1
      if(ISHELL.EQ.KSHELL.AND.JSHELL.EQ.LSHELL)ikjl=1
      ishdup=min0(4*ikjl+2*ij+kl,5)
      
      return
      
      end
C* :1 * 
      
