
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ffrcnn"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ffrcnn.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "ffrcnn.web"
      subroutine ffrcnn(NATOMS,IAN,C,FFXYZ)
      implicit none
      double precision ab,C,FFXYZ,fn,fnn,gfloat,gsqrt,rabsq,temp,vnnxx,z
     &ero
      integer i,i1,ia,IAN,ib,ijaa,ijab,ijbb,ik,j,jiab,jk,k,lind,nat3,nat
     &3tt,NATOMS
      dimension IAN(*),C(*),FFXYZ(*)
      dimension ab(3)
      data zero/0.0D0/
      
      
      
      
      
      
      
      
      
      
      lind(i,j)=(i*(i-1))/2+j
      
      nat3=3*NATOMS
      nat3tt=(nat3*(nat3+1))/2
      
      call aclear(nat3tt,FFXYZ)
      
      do 100 i=1,NATOMS
      i1=i-1
      if(i1.NE.0)then
      ia=3*(i-1)
      do 40 j=1,i1
      ib=3*(j-1)
      rabsq=zero
      do 10 k=1,3
      ab(k)=C(k+ia)-C(k+ib)
      rabsq=rabsq+ab(k)**2
10    continue
      fn=gfloat(IAN(i)*IAN(j))/gsqrt(rabsq)
      vnnxx=vnnxx+fn
      fn=fn/rabsq
      fnn=(fn+fn+fn)/rabsq
      
      do 20 ik=1,3
      do 15 jk=1,ik
      ijaa=lind(ia+ik,ia+jk)
      ijbb=lind(ib+ik,ib+jk)
      ijab=lind(ia+ik,ib+jk)
      jiab=lind(ia+jk,ib+ik)
      temp=ab(ik)*ab(jk)*fnn
      if(ik.EQ.jk)temp=temp-fn
      FFXYZ(ijaa)=FFXYZ(ijaa)+temp
      FFXYZ(ijbb)=FFXYZ(ijbb)+temp
      FFXYZ(ijab)=FFXYZ(ijab)-temp
      if(ik.NE.jk)FFXYZ(jiab)=FFXYZ(jiab)-temp
15    continue
20    continue
40    continue
      endif
100   continue
      
      return
      
      end
C* :1 * 
      
