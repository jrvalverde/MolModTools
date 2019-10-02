
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sphset"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sphset.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "sphset.web"
      subroutine sphset(MAXAP3,NATOMS,A,ATMCHG,NSET,NPOP,ASET,NUMSET)
      implicit none
      double precision A,ASET,ATMCHG,curd,curz,gabs,gsqrt,Tol2,Toler,zer
     &o
      integer i,iat,iattop,ic,ictop,idx,init,iset,j,j1,j2,jat,jc,jset,k,
     &k1,l,MAXAP3,mpop,mset
      integer NATOMS,NPOP,NSET,num,NUMSET
      dimension A(MAXAP3,3),ATMCHG(*),NPOP(*),NSET(*),ASET(MAXAP3,3)
      dimension mset(100),mpop(100),init(100),idx(100)
      common/tol/Toler,Tol2
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
      do 100 iat=1,NATOMS
      ASET(iat,1)=ATMCHG(iat)
      ASET(iat,2)=gsqrt(A(iat,1)**2+A(iat,2)**2+A(iat,3)**2)
100   continue
      
      
      do 200 iat=1,NATOMS
      if(gabs(ASET(iat,2)).LT.Toler)ASET(iat,1)=zero
200   continue
      
      
      iattop=NATOMS-1
      ic=0
      iset=0
      do 300 iat=1,iattop
      if(ASET(iat,1).NE.zero)then
      ic=ic+1
      iset=iset+1
      mpop(iset)=1
      mset(ic)=iat
      init(iset)=iat
      j1=iat+1
      do 220 jat=j1,NATOMS
      if(ASET(jat,1).NE.zero)then
      if(gabs(ASET(jat,2)-ASET(iat,2)).LE.Toler)then
      ic=ic+1
      mpop(iset)=mpop(iset)+1
      mset(ic)=jat
      ASET(jat,1)=zero
      endif
      endif
220   continue
      endif
300   continue
      NUMSET=iset
      ictop=ic
      
      
      
      do 400 i=1,NUMSET
      idx(i)=i
400   continue
      if(NUMSET.NE.1)then
      
      i=0
450   i=i+1
      j=idx(i)
      iat=init(j)
      curd=ASET(iat,2)
      curz=ASET(iat,1)
      k1=i+1
      do 500 k=k1,NUMSET
      l=idx(k)
      jat=init(l)
      if(gabs(curd-ASET(jat,2)).GT.Toler)then
      
      if(curd.LT.ASET(jat,2))goto 500
      elseif(curz.LT.ASET(jat,1))then
      goto 500
      endif
      idx(i)=l
      idx(k)=j
      iat=init(l)
      curd=ASET(iat,2)
      curz=ASET(iat,1)
500   continue
      if(i.LT.NUMSET-1)goto 450
      endif
      
      
      ic=0
      do 600 iset=1,NUMSET
      jset=idx(iset)
      NPOP(iset)=mpop(jset)
      num=NPOP(iset)
      jc=0
      if(jset.NE.1)then
      j2=jset-1
      do 520 j=1,j2
      jc=jc+mpop(j)
520   continue
      endif
      do 550 i=1,num
      ic=ic+1
      NSET(ic)=mset(jc+i)
550   continue
600   continue
      return
      
      end
C* :1 * 
      
