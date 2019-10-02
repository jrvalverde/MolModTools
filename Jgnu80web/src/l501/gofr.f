
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gofr"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gofr.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "gofr.web"
      subroutine gofr(R,G,I,J,K,L,II,VA)
      implicit none
      double precision coul1,coul2,coul3,exch
      integer ij,ik,il,jk,jl,kl
      double precision R(*),G(*),em,en,VA
      integer I,J,K,L
      integer II(*)
      data em/1.0D00/,en/0.5D00/
      ij=II(I)+J
      kl=II(K)+L
      il=II(I)+L
      ik=II(I)+K
      jk=II(J)+K
      jl=II(J)+L
      if(J.LT.K)jk=II(K)+J
      if(J.LT.L)jl=II(L)+J
      coul1=em*R(ij)*VA
      coul2=em*R(kl)*VA
      exch=en*VA
      if(K.NE.L)then
      coul2=coul2+coul2
      G(ik)=G(ik)-R(jl)*exch
      if((I.NE.J).AND.(J.GE.K))G(jk)=G(jk)-R(il)*exch
      endif
      G(il)=G(il)-R(jk)*exch
      G(ij)=G(ij)+coul2
      if((I.NE.J).AND.(J.GE.L))G(jl)=G(jl)-R(ik)*exch
      if(ij.NE.kl)then
      coul3=coul1
      if(I.NE.J)coul3=coul3+coul1
      if(J.LE.K)then
      G(jk)=G(jk)-R(il)*exch
      if((I.NE.J).AND.(I.LE.K))G(ik)=G(ik)-R(jl)*exch
      if((K.NE.L).AND.(J.LE.L))G(jl)=G(jl)-R(ik)*exch
      endif
      G(kl)=G(kl)+coul3
      endif
      return
      end
C* :1 * 
      
