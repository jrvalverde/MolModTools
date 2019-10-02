
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 findcn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "findcn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "findcn.web"
      subroutine findcn(MAXAP3,NATOMS,A,B,D,IAN,NPOP,NSET,IXYZ,NORDER)
      implicit none
      double precision A,B,D,eight,gatan,gfloat,one,t,theta,twopi
      integer i,IAN,itst,IXYZ,j,MAXAP3,maxmul,multst,NATOMS,NORDER,NPOP,
     &NSET,numset
      dimension NPOP(*),A(*),B(*),D(*),IAN(*),NSET(*)
      dimension t(3,3)
      data one,eight/1.0D0,8.0D0/
      
      
      
      
      
      
      
      twopi=eight*gatan(one)
      
      
      call cirset(MAXAP3,NATOMS,A,IAN,IXYZ,NSET,NPOP,D,numset)
      
      
      maxmul=1
      do 100 i=1,numset
      maxmul=max0(maxmul,NPOP(i))
100   continue
      do 200 i=1,maxmul
      multst=maxmul-i+1
      do 150 j=1,numset
      if(mod(NPOP(j),multst).NE.0)goto 200
150   continue
      theta=twopi/gfloat(multst)
      call rotate(MAXAP3,A,B,NATOMS,t,IXYZ,theta)
      call equiv(MAXAP3,A,B,IAN,NATOMS,itst)
      if(itst.NE.0)then
      NORDER=multst
      return
      endif
      
200   continue
      NORDER=1
      return
      
      end
C* :1 * 
      
