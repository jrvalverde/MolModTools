
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 hukges"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "hukges.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 59 "hukges.web"
      subroutine hukges(A,B,AA,BB,SCR,MD,NB,IOV,IOS,IAN,NATOMS,IBASIS,IF
     &PONH,IFDON1,IFDON2,I5OR6D)
      implicit none
      double precision A,AA,B,BB,SCR
      integer I5OR6D,IAN,IBASIS,IFDON1,IFDON2,IFPONH,IOS,IOV,MD,NATOMS,N
     &B
      dimension A(*),B(*),AA(*),BB(*),SCR(*),IAN(*)
      call huckel(A(1),B(1),MD,NB,IAN,NATOMS,SCR,IOS,IBASIS,IFDON1,IFDON
     &2,I5OR6D,IFPONH)
      
      call getmo(A(1),B(1),AA(1),BB(1),MD,NB,IOV)
      return
      
      end
C* :1 * 
      
