
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 wrppna"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "wrppna.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "wrppna.web"
      subroutine wrppna(T,OCC,IFLG)
      implicit none
      integer IFLG,Ispin,j,Label,Larc,Lbl,lfn,Lorb,Lorbc,Lstemt,Lstocc,M
     &AXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms
      integer Nbas,Ndim
      double precision OCC,T
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      
      dimension T(Ndim,Ndim),OCC(Ndim)
      character*80 title
      
      
      
      title='PNAOs in the PAO basis:'
      call aout(T,Ndim,Nbas,Nbas,title,-1,IFLG)
      
      
      lfn=abs(IFLG)
      write(lfn,99001)(Naoctr(j),j=1,Nbas)
      write(lfn,99001)(Naol(j),j=1,Nbas)
      write(lfn,99001)(Lstocc(j),j=1,Nbas)
      
      
      write(lfn,99002)(OCC(j),j=1,Nbas)
      return
      
99001 format(1x,20I4)
99002 format(1x,5F15.9)
      end
C* :1 * 
      
