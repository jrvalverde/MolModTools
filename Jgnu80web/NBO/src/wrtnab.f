
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wrtnab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wrtnab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "wrtnab.web"
      subroutine wrtnab(T,IFLG)
      implicit none
      integer i,Iathy,Ibxm,IFLG,Ispin,j,Label,Larc,lfn,Lstocc,MAXATM,MAX
     &BAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nbas
      integer Ndim
      double precision T
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      
      dimension T(Ndim,Ndim)
      character*80 title
      
      
      title='NBOs in the NAO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,2,IFLG)
      
      
      lfn=abs(IFLG)
      do 100 i=1,Nbas
      write(lfn,99001)(Label(i,j),j=1,6),Ibxm(i)
100   continue
      return
      
99001 format(1x,a2,a1,4I3,3x,i3)
      end
C* :1 * 
      
