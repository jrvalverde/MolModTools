
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 newwts"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "newwts.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "newwts.web"
      subroutine newwts(S,T,WT)
      implicit none
      double precision av,S,sjk,sum,T,WT,zero
      integer i,iadd,il,ilbl,imax,inao,iorb,Ispin,j,jorb,jorbl,k,Label,L
     &arc,Lbl,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm
      integer Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnp
     &nb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Lorb,Lorbc,Lstemt,Lstocc,m,MAXATM,MA
     &XBAS
      integer Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Natoms,Nbas,nc,Ndim,nl,
     &nm,nocc,nstart
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension T(Ndim,Ndim),S(Ndim,Ndim),WT(Ndim)
      character*80 title
      
      data zero/0.0D0/
      
      nocc=0
      do 100 i=1,Nbas
      sum=zero
      do 50 j=1,Nbas
      do 20 k=1,Nbas
      sjk=S(j,k)
      if(j.GT.k)sjk=S(k,j)
      sum=sum+T(j,i)*sjk*T(k,i)
20    continue
50    continue
      WT(i)=sum
      if(Lstocc(i).NE.0)then
      nocc=nocc+1
      Lstocc(nocc)=i
      endif
100   continue
      nstart=nocc+1
      do 200 i=nstart,Ndim
      Lstocc(i)=0
200   continue
      nl=1
      iorb=0
300   iorb=iorb+nl
      if(iorb.LE.Nbas)then
      nl=1
      ilbl=Naoctr(iorb)
      il=Naol(iorb)/100
      nm=il*2+1
      imax=Nbas-iorb
      do 350 iadd=1,imax
      jorb=iorb+iadd
      jorbl=Naol(jorb)/100
      if(Naoctr(jorb).NE.ilbl.OR.jorbl.NE.il)goto 400
      nl=nl+1
350   continue
400   nc=nl/nm
      do 450 i=1,nc
      sum=zero
      do 420 m=1,nm
      inao=iorb+(i-1)+(m-1)*nc
      sum=sum+WT(inao)
420   continue
      av=sum/nm
      do 440 m=1,nm
      inao=iorb+(i-1)+(m-1)*nc
      WT(inao)=av
440   continue
450   continue
      goto 300
      endif
      
      title='New symmetry-averaged occupancy weights:'
      call aout(WT,Nbas,Nbas,1,title,-1,1)
      return
      
      end
C* :1 * 
      
