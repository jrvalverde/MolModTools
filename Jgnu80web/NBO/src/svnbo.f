
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 svnbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "svnbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "svnbo.web"
      subroutine svnbo(T,OCC,ISCR)
      implicit none
      integer i,Iatcr,Iatno,Ibxm,ii,Ino,ISCR,Ispin,Iznuc,k,l1,l3,l4,Labe
     &l,Larc,Lbl,Ll,Lorb,Lorbc,Lstocc
      integer Lu,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Nbotyp
     &,Nbouni,Ndim,nfile,Norb
      double precision OCC,T
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norb(MAXATM),Ll(MAXATM),Lu
     &(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      
      dimension T(Ndim,Ndim),OCC(Ndim),ISCR(1)
      
      
      
      l1=Ndim
      l3=Ndim*Ndim
      l4=Ndim*10
      nfile=44
      if(Beta)nfile=45
      call nbwrit(T,l3,nfile)
      
      
      nfile=27
      if(Beta)nfile=28
      call nbwrit(OCC,l1,nfile)
      
      
      ii=0
      do 100 k=1,6
      do 50 i=1,Nbas
      ii=ii+1
      ISCR(ii)=Label(i,k)
50    continue
100   continue
      do 200 i=1,Nbas
      ii=ii+1
      ISCR(ii)=Ibxm(i)
200   continue
      do 300 i=1,Natoms
      ii=ii+1
      ISCR(ii)=Iatno(i)
300   continue
      do 400 i=1,Nbas
      ii=ii+1
      ISCR(ii)=Nbouni(i)
400   continue
      do 500 i=1,Nbas
      ii=ii+1
      ISCR(ii)=Nbotyp(i)
500   continue
      
      nfile=60
      if(Beta)nfile=61
      call nbwrit(ISCR,l4,nfile)
      
      return
      end
C* :1 * 
      
