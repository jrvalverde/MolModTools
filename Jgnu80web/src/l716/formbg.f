
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 formbg"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "formbg.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 38 "formbg.web"
      subroutine formbg(MAXNZ,NZ,IANZ,IZ,BL,ALPHA,BETA,NPARM,IGEIG,B,IB,
     &G,XM,CZ,CC,LL,MM,IDUMP,TOANG)
      implicit none
      double precision ALPHA,B,BETA,BL,CC,cutoff,CZ,G,gabs,one,r,TOANG,w
     &ork,XM,zero
      integer i,i1,IANZ,IB,ibi,IDUMP,IGEIG,In,Iout,iparm,Ipunch,IZ,j,j1,
     &k,l,LL,MAXNZ,MM,nattmp
      integer NPARM,NZ
      logical error
      dimension IANZ(*),IZ(MAXNZ,4),BL(*),ALPHA(*),BETA(*)
      dimension B(3,4,NPARM),IB(4,NPARM),G(*)
      dimension XM(NZ,5),CZ(*),CC(*),LL(*),MM(*)
      dimension work(100)
      common/io/In,Iout,Ipunch
      data zero/0.D0/,one/1.D0/
      data cutoff/1.D-20/
      
      
      
      
      
      
99001 format('0 ***** ERROR *****'/'  G-MATRIX IS SINGULAR IN FORMBG,  D
     &ET=',d20.13/'                               CUTOFF=',d12.2)
99002 format(' FROM FORMBG:'/'  ZTOC COORDINATES')
99003 format('  IB MATRIX')
99004 format('  B MATRIX')
99005 format('  G MATRIX')
99006 format('  G-INVERSE')
      
      call rtrace(6HFORMBG,2)
      call ztoc(MAXNZ,NZ,IANZ,IZ,BL,ALPHA,BETA,.FALSE.,nattmp,LL,CC,CZ,X
     &M(1,1),XM(1,2),XM(1,3),XM(1,4),XM(1,5),Iout,error,work)
      
      if(IDUMP.NE.0)then
      write(Iout,99002)
      call corpr1(NZ,IANZ,CZ,TOANG)
      call corpr1(nattmp,LL,CC,TOANG)
      endif
      
      do 100 i=1,NZ
      do 50 j=1,3
      XM(i,j)=one
50    continue
100   continue
      XM(1,1)=zero
      XM(1,2)=zero
      XM(1,3)=zero
      XM(2,1)=zero
      XM(2,2)=zero
      XM(3,2)=zero
      
      
      do 200 i=1,NPARM
      do 150 k=1,4
      IB(k,i)=0
      do 120 j=1,3
      B(j,k,i)=zero
120   continue
150   continue
200   continue
      
      do 300 i=2,NZ
      
      call str(i-1,i,IZ(i,1),B,IB,CZ,NPARM)
      if(BL(i).LT.zero)then
      do 220 j=1,3
      do 210 k=1,2
      B(j,k,i-1)=-B(j,k,i-1)
210   continue
220   continue
      endif
      
      if(i.GT.2)then
      iparm=NZ-3+i
      
      call bend(iparm,i,IZ(i,1),IZ(i,2),B,IB,CZ,NPARM)
      if(ALPHA(i).LT.zero)then
      do 230 j=1,3
      do 225 k=1,3
      B(j,k,iparm)=-B(j,k,iparm)
225   continue
230   continue
      endif
      
      if(i.GT.3)then
      iparm=NZ+NZ-6+i
      if(IZ(i,4).EQ.0)then
      
      call tors(iparm,i,IZ(i,1),IZ(i,2),IZ(i,3),B,IB,CZ,NPARM)
      else
      
      call bend(iparm,i,IZ(i,1),IZ(i,3),B,IB,CZ,NPARM)
      if(BETA(i).LT.zero)then
      do 234 j=1,3
      do 232 k=1,3
      B(j,k,iparm)=-B(j,k,iparm)
232   continue
234   continue
      endif
      endif
      endif
      endif
      
300   continue
      
      do 400 i=1,NPARM
      do 350 i1=1,4
      ibi=IB(i1,i)
      if(ibi.NE.0)then
      do 310 l=1,3
      B(l,i1,i)=B(l,i1,i)*XM(ibi,l)
310   continue
      endif
350   continue
400   continue
      
      if(IDUMP.NE.0)then
      write(Iout,99003)
      call ibout(IB,4,NPARM,4,NPARM)
      write(Iout,99004)
      call matout(B,12,NPARM,12,NPARM)
      endif
      
      do 500 i=1,NPARM
      do 450 j=1,i
      r=zero
      do 420 i1=1,4
      ibi=IB(i1,i)
      if(ibi.NE.0)then
      do 405 j1=1,4
      if(ibi.EQ.IB(j1,j))then
      do 402 l=1,3
      r=r+B(l,i1,i)*B(l,j1,j)
402   continue
      endif
405   continue
      endif
420   continue
      In=j+NPARM*(i-1)
      G(In)=r
      In=i+NPARM*(j-1)
      G(In)=r
450   continue
500   continue
      
      if(IDUMP.NE.0)then
      write(Iout,99005)
      call matout(G,NPARM,NPARM,NPARM,NPARM)
      endif
      
      
      call minv(G,NPARM,r,LL,MM)
      
      if(IDUMP.NE.0)then
      write(Iout,99006)
      call matout(G,NPARM,NPARM,NPARM,NPARM)
      endif
      if(gabs(r).LT.cutoff)then
      write(Iout,99001)r,cutoff
      call lnk1e
      endif
      if(IDUMP.EQ.0)return
      write(Iout,99007)r
      
99007 format(' FORMBG, DETERMINANT G-MATRIX =',d20.13)
      
      return
      
      end
C* :1 * 
      
