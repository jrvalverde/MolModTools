
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 savscf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "savscf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "savscf.web"
      subroutine savscf(JUMP)
      
      
      
      implicit none
      real*8 aii,Atmchg,C,daytim,Dgen,Exx,scfprg,stars,Title,today,X,zer
     &o
      integer i,Ian,ib,ibl,Icharg,illim,indx,Iop,iout,Ipunch,ircurs,irow
     &l,Irtcrd,itcur,Ititle,itmp,iulim,Ix,Iy,j
      integer jrowl,JUMP,k,Label,len,LENB,lim,lim1,lim2,MAXBAS,MAXPRM,MA
     &XS21,MAXSH1,MAXSHL,Multip,Nae,Natoms,nb4,Nbasis,Nbe
      integer nbsq,ncent,Ne,nmax,nocc,nphe,ntt,nvirt
      parameter(MAXSHL=100,MAXBAS=150,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1
     &),MAXS21=(2*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      integer getchr
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),X(3,100)
      common/rpacom/C(MAXBAS,MAXBAS)
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      common/io/Ix,Iy,Ipunch
      common/b/Exx(LENB)
      common/gen/Dgen(47)
      dimension aii(MAXBAS),jrowl(508)
      dimension illim(50),iulim(50),lim(51),Title(10),itmp(16),irowl(16)
      equivalence(Title(1),Ititle(1))
      data scfprg/8Hgnu80   /,zero/0.0D0/,ibl/1H /,stars/8H**SCF***/
      call drum
      nmax=150
      ntt=Nbasis*(Nbasis+1)/2
      nbsq=Nbasis*Nbasis
      nb4=4*Nbasis
      iout=11
      open(unit=iout,file='RPAC.OUT',form='UNFORMATTED',status='UNKNOWN'
     &)
      nocc=Ne/2
      nvirt=Nbasis-nocc
      nphe=nocc*nvirt
      rewind iout
      
      
      call utida(today)
      call utiti(daytim)
      call tread(502,Label,600,1,600,1,0)
      write(iout)stars,Title,scfprg,today,daytim
      
      
      write(iout)Natoms,Icharg,Multip,Nbasis,nocc,nvirt,Nae,Nbe,Ne
      
      
      write(iout)((X(k,i),k=1,3),Ian(i),i=1,Natoms)
      
      
      call tread(524,C,nmax,nmax,Nbasis,Nbasis,0)
      nbsq=Nbasis*Nbasis
      write(iout)((C(i,j),i=1,Nbasis),j=1,Nbasis)
      
      
      call tread(522,aii,nmax,1,Nbasis,1,0)
      write(iout)aii
      
      
      ncent=1
      lim1=1
      lim(1)=1
      ircurs=0
      call getb(2,itmp,len,Label,ircurs)
      goto 200
100   lim1=lim2
      ncent=ncent+1
      lim(ncent)=lim1
200   lim1=lim1+1
      if(lim1.LE.Nbasis)then
      do 250 i=lim1,Nbasis
      lim2=i
      call getb(2,itmp,len,Label,ircurs)
      do 220 j=1,16
      irowl(j)=ibl
220   continue
      len=min0(len,16)
      itcur=0
      do 240 j=1,len
      irowl(j)=getchr(itmp,itcur)
240   continue
      if(irowl(1).NE.ibl.OR.irowl(2).NE.ibl)goto 100
250   continue
      endif
      lim(ncent+1)=Nbasis+1
      do 300 i=1,Natoms
      illim(i)=lim(i)
      iulim(i)=lim(i+1)-1
300   continue
      write(iout)(illim(i),iulim(i),i=1,Natoms)
      
      
      do 400 i=1,nb4
      jrowl(i)=ibl
400   continue
      ircurs=0
      indx=1
      do 500 ib=1,Nbasis
      do 450 i=1,16
      itmp(i)=ibl
450   continue
      call getb(2,itmp,len,Label,ircurs)
      itcur=0
      call putb(itmp,len,jrowl(indx),itcur)
      indx=indx+4
500   continue
      write(iout)(jrowl(i),i=1,nb4)
      
      
      call tread(506,Exx,LENB,1,LENB,1,0)
      write(iout)(Exx(i),i=1,LENB)
      
      
      call tread(501,Dgen,47,1,47,1,0)
      write(iout)Dgen(32)
      
      rewind iout
      
      
      write(Iy,99002)iout,Title
      
      JUMP=0
      return
      
99001 format(10A8)
99002 format(/26H SCF OUTPUT SAVED ON FILE ,i2,1H:,5x,10A8//20H   RECORD
     &   CONTENTS/3x,6(1H-),3x,7(1H-)/7x,1H1,5x,18HTITLE AND JOB DATA/7x
     &,1H2,5x,19HMOLECULE PARAMETERS/7x,1H3,5x,18HATOMIC COORDINATES/7x,
     &1H4,5x,15HMO COEFFICIENTS/7x,1H5,5x,16HORBITAL ENERGIES/7x,1H6,5x,
     &13HORBITALS/ATOM/7x,1H7,5x,21HBASIS FUNCTION LABELS/7x,1H8,5x,18HG
     &AUSSIAN BASIS SET/7x,1H9,5x,10HSCF ENERGY)
      end
C* :1 * 
      
