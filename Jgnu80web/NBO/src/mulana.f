
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 mulana"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "mulana.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "mulana.web"
      subroutine mulana(BS,VMAYER,BMAYER,IWMULP,IWCUBF)
      implicit none
      double precision angl,BMAYER,BS,cubicf,occ,sumat,sumt,VMAYER,zero
      integer i,iang,iat,Iatcr,Iatno,ii,il,ilm,Ino,Ispin,IWCUBF,IWMULP,i
     &z,Iznuc,j,jat,l,Label,lang,Larc
      integer Lbl,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lf
     &nnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr
     &,Ll
      integer lm,Lorb,Lorbc,Lstemt,Lstocc,Lu,MAXATM,MAXBAS,Munit,Mxao,Mx
     &aolm,Mxbo,nam,nameat,Naoctr,Naol,Natoms,Nbas,Ndim,Norbs
      
      
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      dimension BS(Ndim,Ndim),VMAYER(Natoms),BMAYER(Natoms,Natoms),iang(
     &5),angl(60),lang(60),cubicf(7)
      character*80 title
      data iang/'s','p','d','f','g'/
      data lang/51,151,152,153,251,252,253,254,255,351,352,353,354,355,3
     &56,357,451,452,453,454,455,456,457,458,459,1,101,102,103,201,202,2
     &03,204,205,206,301,302,303,304,305,306,307,308,309,310,401,402,403
     &,404,405,406,407,408,409,410,411,412,413,414,415/
      data angl/4H    ,4Hx   ,4Hy   ,4Hz   ,4Hxy  ,4Hxz  ,4Hyz  ,4Hx2y2,
     &4Hz2  ,4H(0) ,4H(c1),4H(s1),4H(c2),4H(s2),4H(c3),4H(s3),4H(0) ,4H(
     &c1),4H(s1),4H(c2),4H(s2),4H(c3),4H(s3),4H(c4),4H(s4),4H    ,4Hx   
     &,4Hy   ,4Hz   ,4Hxx  ,4Hxy  ,4Hxz  ,4Hyy  ,4Hyz  ,4Hzz  ,4Hxxx ,4H
     &xxy ,4Hxxz ,4Hxyy ,4Hxyz ,4Hxzz ,4Hyyy ,4Hyyz ,4Hyzz ,4Hzzz ,4Hxxx
     &x,4Hxxxy,4Hxxxz,4Hxxyy,4Hxxyz,4Hxxzz,4Hxyyy,4Hxyyz,4Hxyzz,4Hxzzz,4
     &Hyyyy,4Hyyyz,4Hyyzz,4Hyzzz,4Hzzzz/
      data cubicf/4H(d1),4H(d2),4H(d3),4H(b) ,4H(e1),4H(e2),4H(e3)/
      data zero/0.0D0/
      if(IWCUBF.NE.0)then
      do 50 i=1,7
      ii=i+9
      angl(ii)=cubicf(i)
50    continue
      endif
      if(IWMULP.EQ.1)write(Lfnpr,99001)
      if(IWMULP.EQ.2)write(Lfnpr,99002)
      if(IWMULP.EQ.2)write(Lfnpr,99003)
      sumt=zero
      do 200 i=1,Ndim
      VMAYER(i)=zero
      do 100 j=1,Ndim
      BMAYER(i,j)=zero
100   continue
200   continue
      do 400 iat=1,Natoms
      iz=Iatno(iat)
      nam=nameat(iz)
      sumat=zero
      do 300 i=1,Nbas
      if(Lbl(i).NE.iat)goto 300
      lm=Lorbc(i)
      l=lm/100
      il=iang(l+1)
      do 220 ilm=1,60
      if(lm.EQ.lang(ilm))goto 240
220   continue
      
      stop
240   occ=BS(i,i)
      sumat=sumat+occ
      if(IWMULP.EQ.2)write(Lfnpr,99004)i,nam,iat,il,angl(ilm),occ
      do 260 j=1,Nbas
      jat=Lbl(j)
      if(jat.NE.iat)BMAYER(iat,jat)=BMAYER(iat,jat)+BS(i,j)*BS(j,i)
260   continue
300   continue
      if(IWMULP.EQ.1)write(Lfnpr,99006)nam,iat,sumat
      if(IWMULP.EQ.2)write(Lfnpr,99007)nam,iat,sumat
      sumt=sumt+sumat
400   continue
      if(IWMULP.NE.0)write(Lfnpr,99005)sumt
      title='Mayer-Mulliken atom-atom bond order matrix:'
      call aout(BMAYER,Natoms,Natoms,Natoms,title,0,Natoms)
      do 500 i=1,Natoms
      do 450 j=1,Natoms
      VMAYER(i)=VMAYER(i)+BMAYER(i,j)
450   continue
500   continue
      title='Mayer-Mulliken valencies by atom:'
      call aout(VMAYER,Natoms,Natoms,1,title,0,1)
      return
99001 format(//1x,'Total gross Mulliken populations by atom:',//4x,'Atom
     &  #',7x,'Total')
99002 format(//1x,'Input atomic orbitals, gross Mulliken populations:',/
     &/1x,' AO',2x,'Atom  #',2x,'lang',2x,'Mulliken Population',4x,'Atom
     &  #',7x,'Total')
99003 format(1x,79('-'))
99004 format(1x,i3,3x,a2,i3,2x,a1,a4,f13.7)
99005 format(/1x,'Total number of electrons: ',f11.6)
99006 format(5x,a2,i3,f15.7)
99007 format(44x,a2,i3,f15.7)
      end
C* :1 * 
      
