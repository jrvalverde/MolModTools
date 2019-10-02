
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aprint"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aprint.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "aprint.web"
      subroutine aprint(A,MR,NR,NC,TITLE,INDEX,MCOL)
      implicit none
      double precision A,atom,basis,dashes,tmax
      integer i,Iatcr,Iatno,ilabel,INDEX,Ino,Iznuc,j,k,l,Lbl,Lfnao,Lfnar
     &c,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao
      integer Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lf
     &npr,Ll,Lu,MAXATM,MAXBAS,maxcol,MCOL,MR,n,nameat,NC,ncl
      integer ncol,ncu,nd,Nlew,nloops,nn,Norbs,NR,Nval
      dimension A(MR,1)
      character*80 TITLE
      dimension basis(5)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nblbl/Nlew,Nval,Lbl(10,MAXBAS,4)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      data basis/4H AO ,4H NAO,4H NHO,4H NBO,4HNLMO/
      data atom,dashes/4HAtom,8H--------/
      
      
      ncol=MCOL
      if(ncol.GT.abs(NC))ncol=abs(NC)
      
      nn=abs(NR)
      ilabel=INDEX
      if(ilabel.EQ.5)ilabel=4
      
      tmax=dabs(A(1,1))
      do 100 j=1,ncol
      do 50 i=1,nn
      if(dabs(A(i,j)).GT.tmax)tmax=dabs(A(i,j))
50    continue
100   continue
      nd=int(log10(tmax))+1
      
      
      write(Lfnpr,99010)TITLE
      
      
      if(ilabel.GE.1.AND.ilabel.LE.4)then
      maxcol=min(10-nd,8)
      if(maxcol.LT.6)then
      call output(A,MR,ncol,nn,ncol)
      else
      ncl=1
      ncu=maxcol
      nloops=(ncol-1)/maxcol+1
      do 120 l=1,nloops
      if(ncu.GT.ncol)ncu=ncol
      if(maxcol.EQ.8)then
      write(Lfnpr,99001)basis(INDEX),(j,j=ncl,ncu)
      write(Lfnpr,99004)(dashes,j=ncl,ncu)
      do 105 i=1,nn
      write(Lfnpr,99007)i,(Lbl(j,i,ilabel),j=1,10),(A(i,k),k=ncl,ncu)
105   continue
      elseif(maxcol.EQ.7)then
      write(Lfnpr,99002)basis(INDEX),(j,j=ncl,ncu)
      write(Lfnpr,99005)(dashes,j=ncl,ncu)
      do 110 i=1,nn
      write(Lfnpr,99008)i,(Lbl(j,i,ilabel),j=1,10),(A(i,k),k=ncl,ncu)
110   continue
      else
      write(Lfnpr,99003)basis(INDEX),(j,j=ncl,ncu)
      write(Lfnpr,99006)(dashes,dashes,j=ncl,ncu)
      do 115 i=1,nn
      write(Lfnpr,99009)i,(Lbl(j,i,ilabel),j=1,10),(A(i,k),k=ncl,ncu)
115   continue
      endif
      ncl=ncu+1
      ncu=ncu+maxcol
120   continue
      endif
      
      
      elseif(ilabel.EQ.0)then
      maxcol=min(10-nd,9)
      if(maxcol.LT.7)then
      call output(A,MR,ncol,n,ncol)
      else
      ncl=1
      ncu=maxcol
      nloops=(ncol-1)/maxcol+1
      do 140 l=1,nloops
      if(ncu.GT.ncol)ncu=ncol
      if(maxcol.EQ.9)then
      write(Lfnpr,99011)atom,(j,j=ncl,ncu)
      write(Lfnpr,99014)(dashes,j=ncl,ncu)
      do 125 i=1,nn
      write(Lfnpr,99017)i,nameat(Iatno(i)),(A(i,k),k=ncl,ncu)
125   continue
      elseif(maxcol.EQ.8)then
      write(Lfnpr,99012)atom,(j,j=ncl,ncu)
      write(Lfnpr,99015)(dashes,j=ncl,ncu)
      do 130 i=1,nn
      write(Lfnpr,99018)i,nameat(Iatno(i)),(A(i,k),k=ncl,ncu)
130   continue
      else
      write(Lfnpr,99013)atom,(j,j=ncl,ncu)
      write(Lfnpr,99016)(dashes,j=ncl,ncu)
      do 135 i=1,nn
      write(Lfnpr,99019)i,nameat(Iatno(i)),(A(i,k),k=ncl,ncu)
135   continue
      endif
      ncl=ncu+1
      ncu=ncu+maxcol
140   continue
      endif
      
      
      else
      call output(A,MR,ncol,nn,ncol)
      endif
      return
      
99001 format(/9x,a4,3x,8(3x,i3,2x))
99002 format(/9x,a4,3x,7(4x,i3,2x))
99003 format(/9x,a4,3x,6(4x,i3,3x))
99004 format(6x,'----------',8(1x,a7))
99005 format(6x,'----------',7(1x,a8))
99006 format(6x,'----------',6(1x,a8,a1))
99007 format(1x,i3,'. ',10A1,8F8.4)
99008 format(1x,i3,'. ',10A1,7F9.4)
99009 format(1x,i3,'. ',10A1,6F10.4)
99010 format(//1x,a80)
99011 format(/5x,a4,9(2x,i3,3x))
99012 format(/5x,a4,8(3x,i3,3x))
99013 format(/5x,a4,7(3x,i3,4x))
99014 format(5x,'----',1x,9(a6,2x))
99015 format(5x,'----',1x,8(a7,2x))
99016 format(5x,'----',1x,7(a8,2x))
99017 format(1x,i3,'. ',a2,9F8.4)
99018 format(1x,i3,'. ',a2,8F9.4)
99019 format(1x,i3,'. ',a2,7F10.4)
      end
C* :1 * 
      
