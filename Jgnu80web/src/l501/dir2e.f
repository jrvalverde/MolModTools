
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dir2e"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dir2e.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "dir2e.web"
      subroutine dir2e(NSET,MU,NU,LAMBDA,SIGMA,GINT,IOP,D,F)
      implicit none
      integer NSET,MU,NU,LAMBDA,SIGMA
      integer IOP(*)
      double precision GINT(3,3),D(*),F(*)
      integer Ii
      integer iset,i,j,k,l,n,iswich
      double precision value
      common/ia/Ii(257)
      do 100 iset=1,NSET
      i=MU
      if(iset.EQ.2)then
      
      j=SIGMA
      k=NU
      l=LAMBDA
      elseif(iset.EQ.3)then
      
      j=LAMBDA
      k=NU
      l=SIGMA
      else
      
      j=NU
      k=LAMBDA
      l=SIGMA
      endif
      
      
      iswich=0
      if(i.LT.j)then
      n=i
      i=j
      j=n
      iswich=iswich+1
      endif
      if(k.LT.l)then
      n=k
      k=l
      l=n
      iswich=iswich+1
      endif
      if(i.LT.k)then
      elseif(i.EQ.k)then
      
      if(j.GE.l)goto 50
      else
      goto 50
      endif
      n=i
      i=k
      k=n
      n=j
      j=l
      l=n
      
50    value=GINT(1,iset)
      call gofr(D,F,i,j,k,l,Ii,value)
100   continue
      return
      end
C* :1 * 
      
