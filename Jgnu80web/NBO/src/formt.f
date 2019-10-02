
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 formt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "formt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "formt.web"
      subroutine formt(T,Q,POL)
      implicit none
      integer i,ia,iab,Iatcr,Iathy,Iatno,ib,ibd,ibo,Ibx,Ibxm,icol,icr,il
     &,ilp,in,Ino,Iprin,irow,Ispin
      integer Iznuc,j,jb,jj,jl,jmax,ju,k,kbd,Label,lbd,lcr,Lfnao,Lfnarc,
     &Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab
      integer Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lf
     &nppa,Lfnpr,Ll,llp,lry,lstar,Lstocc,Ltyp,MAXATM,MAXBAS,Munit,Mxao
      integer Mxaolm,Mxbo,Naoa,Naoc,Naoctr,Naol,Natoms,Nbas,nbds,NBO,ncr
     &,nctr,Ndim,nlp,Norbs,nscan
      double precision POL,Q,T,tmax,zero
      integer Ul
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoc(MAXBAS),Naol(MAXBAS),Lstocc(MAXB
     &AS),Ibxm(MAXBAS),Ibx(MAXBAS),Iathy(MAXBAS,3)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),U
     &l(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbnao/Naoctr(MAXBAS),Naoa(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS
     &)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension T(Ndim,Ndim),Q(Mxao,Ndim),POL(Ndim,3)
      
      data lcr,llp,lbd,lstar,lry/'CR','LP','BD','*','RY'/
      data zero/0.0D0/
      
      
      ncr=0
      nlp=0
      nbds=0
      do 100 nscan=1,Nbas
      if(Label(nscan,2).NE.lstar)then
      nbds=nbds+1
      if(Label(nscan,1).EQ.llp)nlp=nlp+1
      if(Label(nscan,1).EQ.lcr)ncr=ncr+1
      endif
100   continue
      icr=0
      ilp=0
      ibo=0
      iab=0
      do 200 ibd=1,Nbas
      if(Label(ibd,2).EQ.lstar)then
      
      
      iab=iab+1
      Ibx(ibd)=nbds+iab
      elseif(Label(ibd,1).EQ.lcr)then
      
      
      icr=icr+1
      Ibx(ibd)=icr+nbds-ncr-nlp
      elseif(Label(ibd,1).EQ.llp)then
      
      
      ilp=ilp+1
      Ibx(ibd)=ilp+nbds-nlp
      else
      
      
      ibo=ibo+1
      Ibx(ibd)=ibo
      endif
200   continue
      
      
      do 300 i=1,Nbas
      do 250 j=1,Nbas
      T(i,j)=zero
250   continue
300   continue
      
      
      NBO=0
      do 500 ibd=1,Nbas
      kbd=ibd
      if(Label(ibd,2).EQ.lstar)then
      if(Label(ibd,1).NE.lry)then
      if(Label(ibd,1).NE.llp)then
      
      
      do 305 k=1,nbo
      do 302 i=4,6
      if(Label(k,i).NE.Label(ibd,i))goto 305
      if((Label(k,3).LE.0).AND.(Label(k,1).EQ.lbd))goto 305
302   continue
      
      
      
      kbd=k
      Label(kbd,3)=-Label(kbd,3)
      goto 350
305   continue
      
      
      write(Lfnpr,99001)ibd,(Label(ibd,jj),jj=1,6)
      stop
      endif
      endif
      endif
      
      
350   do 400 i=1,3
      ia=Label(ibd,i+3)
      if(ia.NE.0)then
      jl=Ll(ia)
      ju=Ul(ia)
      irow=0
      icol=jl+Iathy(kbd,i)-1
      do 360 j=jl,ju
      irow=irow+1
      jb=Ibx(ibd)
      T(j,jb)=POL(ibd,i)*Q(irow,icol)
360   continue
      endif
400   continue
      if(ibd.EQ.kbd)NBO=ibd
500   continue
      
      
      do 600 i=1,Nbas
      if(Label(i,3).LT.0)Label(i,3)=-Label(i,3)
600   continue
      
      
      do 700 ib=1,Nbas
      i=Ibx(ib)
      Ibxm(i)=ib
700   continue
      
      
      do 800 ib=1,Nbas
      nctr=1
      do 750 il=5,6
      if(Label(Ibxm(ib),il).NE.0)nctr=nctr+1
750   continue
      if(nctr.EQ.1)then
      jmax=0
      tmax=-1.0D0
      do 760 in=1,Nbas
      if(Naoa(in).LT.100)then
      if(dabs(T(in,ib)).GT.tmax)then
      jmax=in
      tmax=dabs(T(in,ib))
      endif
      endif
760   continue
      if(jmax.NE.0)then
      if(T(jmax,ib).LT.-1.0D-4)then
      do 765 in=1,Nbas
      T(in,ib)=-T(in,ib)
765   continue
      endif
      endif
      endif
800   continue
      return
      
99001 format(/,1x,'Can''t find bond/antibond match for NBO ',i3,2x,a2,a1
     &,'(',i2,')',3I4)
      end
C* :1 * 
      
