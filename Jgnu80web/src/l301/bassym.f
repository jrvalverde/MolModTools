
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bassym"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bassym.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "bassym.web"
      subroutine bassym(NATOMS,IOP,NBASIS,NSHELL,X,Y,Z)
      implicit none
      integer i,iat,ibasis,igr,In,IOP,ioper,Iout,Ipunch,ishl,isymm,ix,iy
     &,iz,j,jat,jbasis,jshl,jtrans,LENB
      integer limshl,lneq,loshl,Mapper,Maprot,MAXATM,MAXBAS,MAXPRM,MAXS2
     &1,MAXSH1,MAXSHL,mout,MOUTD,MPLUS1,msymm,MSYMMD,myfshl,mynshl,NATOM
     &S,NBASIS
      integer neq,neqatm,neqbas,neqshl,ngr,nosym,Nrot,NSHELL,nsymop,numg
     &r
      double precision X,Y,Z
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      parameter(MAXBAS=150,MAXATM=100,MOUTD=(8*MAXBAS+8*MAXSHL),MPLUS1=(
     &8*MAXSHL+1),MSYMMD=(8*MAXATM+44))
      
      dimension IOP(*),X(*),Y(*),Z(*)
      dimension myfshl(MAXSHL),mynshl(MAXSHL),ngr(20)
      dimension msymm(MSYMMD),jtrans(3,8),neqatm(MAXATM,8)
      dimension mout(MOUTD),neqshl(MAXSHL,8),neqbas(MAXBAS,8)
      common/io/In,Iout,Ipunch
      common/maps/Nrot,Maprot(MAXBAS),Mapper(MAXATM)
      equivalence(msymm(1),nsymop)
      equivalence(msymm(2),jtrans(1,1))
      equivalence(msymm(26),neqatm(1,1))
      equivalence(mout(1),neqshl(1,1))
      equivalence(mout(MPLUS1),neqbas(1,1))
      data isymm/551/,neq/565/,lneq/1000/
      
      
      
      
      
      
      
      
      
99001 format(1H0,'UNRECOGNIZED SHELL GROUP',2I7)
99002 format(1H0,'*** SYMMETRY TURNED OFF IN LINK 301 BY BASSYM ***')
99003 format(1x,'NEQSHL:'/)
99004 format(1x,30I4)
99005 format(1x,'NEQBAS:'/)
      
      
      
      nosym=0
      
      
      call tread(isymm,msymm,422,1,422,1,0)
      
      
      call shldat(myfshl,mynshl,NSHELL,X,Y,Z)
      
      
      do 100 ioper=1,nsymop
      ishl=0
      do 50 iat=1,NATOMS
      jat=neqatm(iat,ioper)
      loshl=myfshl(jat)
      limshl=loshl+mynshl(jat)-1
      do 20 jshl=loshl,limshl
      ishl=ishl+1
      neqshl(ishl,ioper)=jshl
20    continue
50    continue
100   continue
      
      
      
      do 200 ioper=1,nsymop
      ibasis=0
      ix=jtrans(1,ioper)
      iy=jtrans(2,ioper)
      iz=jtrans(3,ioper)
      do 150 iat=1,NATOMS
      jat=neqatm(iat,ioper)
      jbasis=Mapper(jat)-1
      
      
      call getngr(ngr,numgr,Nrot,Mapper,Maprot,jat,NATOMS,NBASIS,nosym)
      igr=1
      
      
120   if(ngr(igr).EQ.1)then
      
      
      ibasis=ibasis+1
      jbasis=jbasis+1
      neqbas(ibasis,ioper)=jbasis
      igr=igr+1
      
      elseif(ngr(igr).EQ.3)then
      
      
      neqbas(ibasis+1,ioper)=(jbasis+1)*ix
      neqbas(ibasis+2,ioper)=(jbasis+2)*iy
      neqbas(ibasis+3,ioper)=(jbasis+3)*iz
      ibasis=ibasis+3
      jbasis=jbasis+3
      igr=igr+1
      
      
      
      elseif(ngr(igr).EQ.6)then
      neqbas(ibasis+1,ioper)=jbasis+1
      neqbas(ibasis+2,ioper)=jbasis+2
      neqbas(ibasis+3,ioper)=jbasis+3
      neqbas(ibasis+4,ioper)=(jbasis+4)*ix*iy
      neqbas(ibasis+5,ioper)=(jbasis+5)*ix*iz
      neqbas(ibasis+6,ioper)=(jbasis+6)*iy*iz
      ibasis=ibasis+6
      jbasis=jbasis+6
      igr=igr+1
      
      
      
      elseif(ngr(igr).EQ.7)then
      neqbas(ibasis+1,ioper)=(jbasis+1)*ix
      neqbas(ibasis+2,ioper)=(jbasis+2)*iy
      neqbas(ibasis+3,ioper)=(jbasis+3)*iz
      neqbas(ibasis+4,ioper)=(jbasis+4)*ix
      neqbas(ibasis+5,ioper)=(jbasis+5)*iy
      neqbas(ibasis+6,ioper)=(jbasis+6)*iz
      neqbas(ibasis+7,ioper)=(jbasis+7)*ix
      neqbas(ibasis+8,ioper)=(jbasis+8)*iy
      neqbas(ibasis+9,ioper)=(jbasis+9)*iz
      neqbas(ibasis+10,ioper)=(jbasis+10)*ix*iy*iz
      ibasis=ibasis+10
      jbasis=jbasis+10
      igr=igr+1
      
      
      
      elseif(ngr(igr).NE.5)then
      
      
      
      nosym=1
      write(Iout,99001)igr,ngr(igr)
      igr=igr+1
      else
      neqbas(ibasis+1,ioper)=jbasis+1
      neqbas(ibasis+2,ioper)=(jbasis+2)*ix*iz
      neqbas(ibasis+3,ioper)=(jbasis+3)*iy*iz
      neqbas(ibasis+4,ioper)=jbasis+4
      neqbas(ibasis+5,ioper)=(jbasis+5)*ix*iy
      ibasis=ibasis+5
      jbasis=jbasis+5
      igr=igr+1
      endif
      
      
      if(igr.LE.numgr)goto 120
150   continue
200   continue
      
      
      if(IOP(33).NE.0)then
      write(Iout,99003)
      do 250 i=1,NSHELL
      write(Iout,99004)(neqshl(i,j),j=1,nsymop)
250   continue
      write(Iout,99005)
      do 300 i=1,NBASIS
      write(Iout,99004)(neqbas(i,j),j=1,nsymop)
300   continue
      endif
      
      
      if(nosym.NE.0)then
      call ilsw(1,26,2)
      write(Iout,99002)
      return
      endif
      
      call twrite(neq,mout,lneq,1,lneq,1,0)
      return
      
      end
C* :1 * 
      
