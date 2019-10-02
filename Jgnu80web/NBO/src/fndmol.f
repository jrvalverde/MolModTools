
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fndmol"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fndmol.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "fndmol.web"
      subroutine fndmol(IATOMS)
      implicit none
      integer i,iat,Iatcr,iatmol,Iatno,IATOMS,imol,imolat,Ino,Ispin,item
     &p,Iznuc,j,k,latoms,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm
      integer Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnp
     &nb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ll,Lu,MAXATM,MAXBAS,Molat,Molata,Mol
     &ec
      integer Moleca,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Ndim,Nmola,Nmole
     &c,Norbs
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbmol/Nmolec,Molat(MAXATM),Molec(MAXATM,MAXATM),Nmola,Molat
     &a(MAXATM),Moleca(MAXATM,MAXATM)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      dimension IATOMS(Natoms)
      logical bdfind
      
      
      Nmolec=0
      do 100 i=1,Natoms
      Molat(i)=0
      do 50 j=1,Natoms
      Molec(i,j)=0
50    continue
100   continue
      do 200 i=1,Natoms
      IATOMS(i)=i
200   continue
      latoms=Natoms
300   Nmolec=Nmolec+1
      Molat(Nmolec)=1
      Molec(Nmolec,1)=IATOMS(1)
      latoms=latoms-1
      if(latoms.NE.0)then
      do 350 i=1,latoms
      IATOMS(i)=IATOMS(i+1)
350   continue
      iat=1
400   i=1
450   if(bdfind(Molec(Nmolec,iat),IATOMS(i)))then
      Molat(Nmolec)=Molat(Nmolec)+1
      Molec(Nmolec,Molat(Nmolec))=IATOMS(i)
      latoms=latoms-1
      if(i.LE.latoms)then
      do 460 j=i,latoms
      IATOMS(j)=IATOMS(j+1)
460   continue
      endif
      else
      i=i+1
      endif
      if(i.LE.latoms)goto 450
      iat=iat+1
      if(iat.LE.Molat(Nmolec).AND.latoms.NE.0)goto 400
      endif
      if(latoms.GT.0)goto 300
      
      
      do 600 i=1,Nmolec
      do 500 j=1,Molat(i)-1
      do 480 k=1,Molat(i)-j
      if(Molec(i,k).GT.Molec(i,k+1))then
      itemp=Molec(i,k)
      Molec(i,k)=Molec(i,k+1)
      Molec(i,k+1)=itemp
      endif
480   continue
500   continue
600   continue
      
      
      if(Ispin.EQ.2)then
      Nmola=Nmolec
      do 650 imol=1,Nmolec
      Molata(imol)=Molat(imol)
      imolat=Molat(imol)
      do 620 iatmol=1,imolat
      Moleca(imol,iatmol)=Molec(imol,iatmol)
620   continue
650   continue
      
      
      elseif(Ispin.EQ.-2)then
      if(Nmola.NE.Nmolec)goto 800
      do 700 imol=1,Nmolec
      imolat=Molat(imol)
      if(imolat.NE.Molata(imol))goto 800
      do 660 iatmol=1,imolat
      if(Moleca(imol,iatmol).NE.Molec(imol,iatmol))goto 800
660   continue
700   continue
      endif
      return
      
800   write(Lfnpr,99001)
      Nmola=-Nmola
      return
      
99001 format(/1x,'The molecular units found in the alpha and beta ','man
     &ifolds are inequivalent.',/1x,'For labelling purposes, ','the mole
     &cular units of the beta system will be used.')
      end
C* :1 * 
      
