
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fenbo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fenbo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "fenbo.web"
      subroutine fenbo(T,OCC,ISCR,NELEC)
      implicit none
      double precision ele,OCC,T,tenth,zero
      integer i,Iatno,Ibxm,ii,ISCR,Iscr1,Iscr2,Ispin,k,l1,l3,l4,Label,MA
     &XATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms
      integer Nbas,Nbotyp,Nbouni,Ndim,NELEC,nfile
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Iatno(M
     &AXBAS),Ibxm(MAXBAS),Iscr1(2*MAXBAS),Iscr2(2*MAXBAS)
      
      dimension T(Ndim,Ndim),OCC(Ndim),ISCR(1)
      
      data zero,tenth/0.0D0,1.0D-1/
      
      
      
      l1=Ndim
      l3=Ndim*Ndim
      l4=Nbas*10
      nfile=44
      if(Beta)nfile=45
      call nbread(T,l3,nfile)
      
      
      nfile=27
      if(Beta)nfile=28
      call nbread(OCC,l1,nfile)
      
      
      ele=zero
      do 100 i=1,Nbas
      ele=ele+OCC(i)
100   continue
      ele=ele+tenth
      NELEC=ele
      
      
      nfile=60
      if(Beta)nfile=61
      call nbread(ISCR,l4,nfile)
      
      ii=0
      do 200 k=1,6
      do 150 i=1,Nbas
      ii=ii+1
      Label(i,k)=ISCR(ii)
150   continue
200   continue
      do 300 i=1,Nbas
      ii=ii+1
      Ibxm(i)=ISCR(ii)
300   continue
      do 400 i=1,Natoms
      ii=ii+1
      Iatno(i)=ISCR(ii)
400   continue
      do 500 i=1,Nbas
      ii=ii+1
      Nbouni(i)=ISCR(ii)
500   continue
      do 600 i=1,Nbas
      ii=ii+1
      Nbotyp(i)=ISCR(ii)
600   continue
      
      return
      end
C* :1 * 
      
