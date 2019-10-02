
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dipele"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dipele.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "dipele.web"
      subroutine dipele(DXYZ,C,T,SCR,ETA,NOCC,INDEX)
      implicit none
      double precision C,DXYZ,ETA,SCR,T
      integer i,INDEX,Ispin,j,k,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,
     &NOCC
      
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      dimension DXYZ(Ndim,Ndim),C(Ndim,Ndim),T(Ndim,Ndim),SCR(Ndim,Ndim)
      
      
      
      
      call fedxyz(DXYZ,INDEX)
      if(INDEX.EQ.0)return
      call simtrs(DXYZ,T,SCR,Ndim,Nbas)
      
      
      do 100 i=1,NOCC
      SCR(i,i)=-ETA*DXYZ(i,i)
100   continue
      
      
      do 200 i=1,NOCC
      do 150 j=1,Nbas
      if(j.NE.i)then
      SCR(j,i)=C(j,i)*DXYZ(i,i)-C(i,i)*DXYZ(j,i)
      do 110 k=1,Nbas
      SCR(j,i)=SCR(j,i)-C(k,i)*DXYZ(k,j)
110   continue
      SCR(j,i)=ETA*C(j,i)*SCR(j,i)
      endif
150   continue
200   continue
      call copy(SCR,DXYZ,Ndim,Nbas,Nbas)
      return
      end
C* :1 * 
      
