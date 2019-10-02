
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 delete"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "delete.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "delete.web"
      subroutine delete(F,TRF,NDIM,IDEL,LEN,ITYPE,NDEL,NTRUNC,DONE,ISPIN
     &)
      implicit none
      double precision F,TRF,zero
      integer i,i1,i2,iat,Iatno,ib,ibas,Ibxm,id,IDEL,idst,iff,ihtyp,ii,i
     &ndx,iout,Iscr1,Iscr2,ISPIN,istar
      integer itmp,ITYPE,iunit,iunit1,iunit2,ivic,j,j1,j2,jat,jb,jbas,jd
     &,jj,jout,k,keywd,Label,lalpha,lalt
      integer latom,lbeta,lblo,ldel,ldeloc,ldestr,lele,LEN,lend,leng,Lfn
     &ao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo
      integer Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,lg,
     &lnogem,lnostr,lnovic,lorb,lsame,lv,lzero,MAXATM,MAXBAS,mset1,mset2
      integer mstart,mstrt2,mtot,nacc,Nbotyp,Nbouni,nchemu,NDEL,ndel2,nd
     &el4,NDIM,nl,nread,nset1,nset2,nstart,nstrt2,ntot,NTRUNC,nu
      integer nunit,nunits
      logical error,DONE,equal
      logical donor,accptr,list1,list2
      dimension keywd(6),F(1),TRF(NDIM,NDIM),IDEL(LEN)
      dimension lorb(3),lele(3),lblo(3),ldel(3),lzero(4),lsame(4),lend(3
     &),ldestr(6),ldeloc(5),lnostr(6),latom(4),lnogem(5),lnovic(5),lalt(
     &4)
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Iatno(M
     &AXBAS),Ibxm(MAXBAS),Iscr1(2*MAXBAS),Iscr2(2*MAXBAS)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      data zero/0.0D0/,istar/1H*/
      data ldel/1HD,1HE,1HL/,lzero/1HZ,1HE,1HR,1HO/,lend/1HE,1HN,1HD/
      data lalpha,lbeta/1HA,1HB/,lsame/1HS,1HA,1HM,1HE/
      data lorb,lele,lblo/1HO,1HR,1HB,1HE,1HL,1HE,1HB,1HL,1HO/
      data ldestr/1HD,1HE,1HS,1HT,1HA,1HR/
      data lnostr/1HN,1HO,1HS,1HT,1HA,1HR/
      data ldeloc/1HD,1HE,1HL,1HO,1HC/,latom/1HA,1HT,1HO,1HM/
      data lnovic/1HN,1HO,1HV,1HI,1HC/,lnogem/1HN,1HO,1HG,1HE,1HM/
      data lalt/1H$,1HE,1HN,1HD/
      data lg,lv/'g','v'/
      
      
      NTRUNC=NDIM
      write(Lfnpr,99019)
      nchemu=0
      do 100 i=1,NDIM
      nunit=Nbouni(i)
      if(nunit.GT.nchemu)nchemu=nunit
100   continue
      if(ISPIN.NE.0)then
      leng=3
      call hfld(keywd,leng,DONE)
      if(equal(keywd,lend,3))DONE=.TRUE.
      if(equal(keywd,lalt,3))DONE=.TRUE.
      if(DONE)return
      if((ISPIN.EQ.2).AND.(keywd(1).NE.lalpha))then
      write(Lfnpr,99001)
99001 format(1x,'Keyword ALPHA (or A) not found to start alpha NBO',' de
     &letion input.')
      stop
      elseif((ISPIN.EQ.-2).AND.(keywd(1).NE.lbeta))then
      write(Lfnpr,99002)
99002 format(1x,'Keyword BETA (or B) not found to start beta NBO',' dele
     &tion input.')
      goto 1100
      else
      if(ISPIN.EQ.2)write(Lfnpr,99003)
      if(ISPIN.EQ.-2)write(Lfnpr,99004)
      endif
      endif
      leng=3
      call hfld(keywd,leng,DONE)
      if(equal(keywd,lend,3))DONE=.TRUE.
      if(equal(keywd,lalt,3))DONE=.TRUE.
      if(DONE)return
      if(.NOT.((ISPIN.EQ.-2).AND.equal(keywd,lsame,3)))then
      if(equal(keywd,lzero,3))then
      ITYPE=3
      ii=0
      do 120 i=1,NDIM
      do 110 j=1,i
      ii=ii+1
      TRF(i,j)=F(ii)
      TRF(j,i)=F(ii)
110   continue
120   continue
      call ifld(NDEL,error)
      if(error)goto 1100
      leng=5
      call hfld(keywd,leng,DONE)
      if(equal(keywd,ldeloc,5))then
      
      
      nstart=0
      do 140 k=1,NDEL
      call hfld(keywd,leng,DONE)
      call ifld(iunit1,error)
      if(error)goto 1100
      call hfld(keywd,leng,DONE)
      call ifld(iunit2,error)
      if(error)goto 1100
      write(Lfnpr,99005)iunit1,iunit2
      nstart=nstart+2
      nset1=0
      do 125 ibas=1,NDIM
      if(Nbouni(ibas).EQ.iunit1)then
      if(Nbotyp(ibas).LE.20)then
      nset1=nset1+1
      IDEL(nstart+nset1)=ibas
      endif
      endif
125   continue
      IDEL(nstart-1)=nset1
      nset2=0
      nstrt2=nstart+nset1
      do 130 ibas=1,NDIM
      if(Nbouni(ibas).EQ.iunit2)then
      if(Nbotyp(ibas).GE.10)then
      nset2=nset2+1
      IDEL(nstrt2+nset2)=ibas
      endif
      endif
130   continue
      IDEL(nstart)=nset2
      ntot=nset1+nset2
      do 135 i=1,nset1
      id=IDEL(nstart+i)
      do 132 j=1,nset2
      jd=IDEL(nstrt2+j)
      if(id.NE.jd)then
      TRF(id,jd)=zero
      TRF(jd,id)=zero
      endif
132   continue
135   continue
      nstart=nstart+ntot
140   continue
      elseif(equal(keywd,latom,4))then
      
      
      mstart=0
      nstart=0
      call hfld(keywd,leng,DONE)
      do 180 k=1,NDEL
      call ifld(mset1,error)
      if(error)goto 1100
      call hfld(keywd,leng,DONE)
      call ifld(mset2,error)
      if(error)goto 1100
      mstart=mstart+2
      Iscr1(mstart-1)=mset1
      Iscr1(mstart)=mset2
      mtot=mset1+mset2
      do 145 i=1,mtot
      call ifld(Iscr1(mstart+i),error)
      if(error)goto 1100
145   continue
      mstrt2=mstart+mset1
      write(Lfnpr,99006)
      write(Lfnpr,99016)(Iscr1(mstart+i),i=1,mset1)
      write(Lfnpr,99007)
      write(Lfnpr,99016)(Iscr1(mstrt2+i),i=1,mset2)
      write(Lfnpr,99008)
      nstart=nstart+2
      nset1=0
      nset2=0
      do 160 jbas=1,NDIM
      donor=.FALSE.
      accptr=.FALSE.
      if(Nbotyp(jbas).LT.20)donor=.TRUE.
      if(Nbotyp(jbas).GE.10)accptr=.TRUE.
      list1=.FALSE.
      list2=.FALSE.
      jb=Ibxm(jbas)
      do 148 j=4,6
      jat=Label(jb,j)
      if(jat.NE.0)then
      do 146 i=1,mset1
      iat=Iscr1(mstart+i)
      if(iat.EQ.jat)goto 148
146   continue
      goto 150
      endif
148   continue
      list1=.TRUE.
150   do 154 j=4,6
      jat=Label(jb,j)
      if(jat.NE.0)then
      do 152 i=1,mset2
      iat=Iscr1(mstrt2+i)
      if(iat.EQ.jat)goto 154
152   continue
      goto 156
      endif
154   continue
      list2=.TRUE.
156   if(.NOT.(list1.AND.list2))then
      if(.NOT.(.NOT.list1.AND..NOT.list2))then
      if(.NOT.(list1.AND..NOT.donor))then
      if(.NOT.(list2.AND..NOT.accptr))then
      if(list2)then
      nset2=nset2+1
      Iscr2(nset2)=jbas
      else
      nset1=nset1+1
      IDEL(nstart+nset1)=jbas
      endif
      endif
      endif
      endif
      endif
160   continue
      
      IDEL(nstart-1)=nset1
      IDEL(nstart)=nset2
      ntot=nset1+nset2
      nstrt2=nstart+nset1
      do 165 i=1,nset2
      IDEL(nstrt2+i)=Iscr2(i)
165   continue
      do 170 i=1,nset1
      id=IDEL(nstart+i)
      do 166 j=1,nset2
      jd=IDEL(nstrt2+j)
      TRF(id,jd)=zero
      TRF(jd,id)=zero
166   continue
170   continue
      mstart=mstart+ntot
      nstart=nstart+ntot
180   continue
      else
      nstart=0
      do 200 k=1,NDEL
      call ifld(nset1,error)
      if(error)goto 1100
      call hfld(keywd,leng,DONE)
      call ifld(nset2,error)
      if(error)goto 1100
      nstart=nstart+2
      IDEL(nstart-1)=nset1
      IDEL(nstart)=nset2
      ntot=nset1+nset2
      do 185 i=1,ntot
      call ifld(IDEL(nstart+i),error)
      if(error)goto 1100
185   continue
      nstrt2=nstart+nset1
      do 190 i=1,nset1
      id=IDEL(nstart+i)
      do 186 j=1,nset2
      jd=IDEL(nstrt2+j)
      if(id.NE.jd)then
      TRF(id,jd)=zero
      TRF(jd,id)=zero
      endif
186   continue
190   continue
      nstart=nstart+ntot
200   continue
      endif
      goto 800
      elseif(equal(keywd,lnovic,3))then
      
      
      ivic=1
      
      write(Lfnpr,99011)
      goto 500
      elseif(equal(keywd,lnogem,3))then
      ivic=0
      
      write(Lfnpr,99012)
      goto 500
      elseif(equal(keywd,ldestr,3))then
      ITYPE=1
      call ifld(nunits,error)
      if(error)goto 1100
      leng=3
      call hfld(keywd,leng,DONE)
      NDEL=0
      do 220 i=1,nunits
      call ifld(iunit,error)
      if(error)goto 1100
      write(Lfnpr,99009)iunit
      do 210 ibas=1,NDIM
      if(Nbouni(ibas).EQ.iunit)then
      if(Label(ibas,2).EQ.istar)then
      NDEL=NDEL+1
      IDEL(NDEL)=ibas
      endif
      endif
210   continue
220   continue
      elseif(equal(keywd,lnostr,3))then
      
      ITYPE=1
      NDEL=0
      write(Lfnpr,99010)
      do 240 ibas=1,NDIM
      if(Label(ibas,2).EQ.istar)then
      NDEL=NDEL+1
      IDEL(NDEL)=ibas
      endif
240   continue
      
99003 format(1x,' ----------- Alpha spin NBO deletions ----------- '/)
99004 format(1x,' ----------- Beta  spin NBO deletions ----------- '/)
99005 format(1x,'Zero delocalization from unit ',i2,' to unit ',i2)
99006 format(1x,'Zero delocalization from NBOs localized on atoms:')
99007 format(1x,'to NBOs localized on atoms:')
99008 format(1x,'    (NBOs in common to the two groups of atoms ','left 
     &out)')
99009 format(1x,'DESTAR unit ',i2,': Delete all Rydberg/antibond',' NBOs
     & from this unit')
99010 format(1x,'NOSTAR: Delete all Rydberg/antibond NBOs')
99011 format(1x,'NOVIC: Delete all vicinal delocalizations')
99012 format(1x,'NOGEM: Delete all geminal delocalizations')
99013 format(1x,'Deletion of the following orbitals ','from the NBO Fock
     & matrix:',(/1x,20I4))
99014 format(1x,'Deletion of the following NBO Fock matrix ','elements:'
     &,/(7(2x,'(',i3,',',i3,')')))
99015 format(1x,'Deletion of the NBO Fock matrix elements ','between orb
     &itals:')
99016 format(1x,20I4)
99017 format(1x,'and orbitals:')
99018 format(1x,'Deletion of the following NBO Fock matrix ','blocks:',/
     &(2(2x,'(',i3,'-',i3,'/',i3,'-',i3,')')))
99019 format(/)
      elseif(.NOT.equal(keywd,ldel,3))then
      
      write(Lfnpr,99020)(keywd(i),i=1,3)
99020 format(1x,'First character string does not have the',' first three
     & letters of DELETE or ZERO:',/1x,3A1)
      stop
      else
      call ifld(NDEL,error)
      if(error)then
      write(Lfnpr,99021)
99021 format(1x,'Non-integer was input for number of items to delete.')
      stop
      else
      call hfld(keywd,leng,DONE)
      if(leng.LT.3)goto 1000
      if(equal(keywd,lorb,3))then
      ITYPE=1
      elseif(.NOT.equal(keywd,lele,3))then
      if(.NOT.equal(keywd,lblo,3))goto 1000
      ITYPE=4
      else
      ITYPE=2
      endif
      nread=NDEL*ITYPE
      do 250 i=1,nread
      call ifld(IDEL(i),error)
      if(error)goto 1100
250   continue
      endif
      endif
      endif
      
      if(ITYPE.NE.1)then
      ii=0
      do 300 i=1,NDIM
      do 260 j=1,i
      ii=ii+1
      TRF(i,j)=F(ii)
      TRF(j,i)=F(ii)
260   continue
300   continue
      if(ITYPE.NE.2)then
      if(ITYPE.NE.4)stop
      do 320 id=1,NDEL
      idst=(id-1)*4
      j1=IDEL(idst+1)
      j2=IDEL(idst+2)
      i1=IDEL(idst+3)
      i2=IDEL(idst+4)
      if(j1.GT.j2)then
      IDEL(idst+2)=j1
      IDEL(idst+1)=j2
      j1=IDEL(idst+1)
      j2=IDEL(idst+2)
      endif
      if(i1.GT.i2)then
      IDEL(idst+4)=i1
      IDEL(idst+3)=i2
      i1=IDEL(idst+3)
      i2=IDEL(idst+4)
      endif
      do 310 i=i1,i2
      do 305 j=j1,j2
      if(i.NE.j)then
      TRF(i,j)=zero
      TRF(j,i)=zero
      endif
305   continue
310   continue
320   continue
      ndel4=NDEL*4
      write(Lfnpr,99018)(IDEL(i),i=1,ndel4)
      return
      else
      ndel2=NDEL*2
      write(Lfnpr,99014)(IDEL(i),i=1,ndel2)
      do 340 i=1,NDEL
      i2=2*i
      id=IDEL(i2-1)
      jd=IDEL(i2)
      TRF(id,jd)=zero
      TRF(jd,id)=zero
340   continue
      return
      endif
      else
      NTRUNC=NDIM-NDEL
      call orderr(Iscr1,IDEL,NDEL,NDIM,Iscr2)
      write(Lfnpr,99013)(IDEL(i),i=1,NDEL)
      iff=0
      iout=1
      ii=0
      do 400 i=1,NDIM
      if(iout.LE.NDEL)then
      if(i.EQ.IDEL(iout))then
      iff=iff+i
      iout=iout+1
      goto 400
      endif
      endif
      ii=ii+1
      jout=1
      jj=0
      do 360 j=1,i
      if(jout.LE.NDEL)then
      if(j.EQ.IDEL(jout))then
      iff=iff+1
      jout=jout+1
      goto 360
      endif
      endif
      jj=jj+1
      iff=iff+1
      TRF(ii,jj)=F(iff)
      TRF(jj,ii)=F(iff)
360   continue
400   continue
      return
      endif
500   ITYPE=3
      
      
      ii=0
      do 600 i=1,NDIM
      do 550 j=1,i
      ii=ii+1
      TRF(i,j)=F(ii)
      TRF(j,i)=F(ii)
550   continue
600   continue
      
      
      NDEL=0
      nstart=0
      do 700 ibas=1,NDIM
      ib=Ibxm(ibas)
      if(Label(ib,2).NE.istar)then
      nacc=0
      do 620 jbas=1,NDIM
      jb=Ibxm(jbas)
      if(Label(jb,2).EQ.istar)then
      
      itmp=ihtyp(ibas,jbas)
      
      
      if(ivic.EQ.1.AND.itmp.EQ.lv)then
      nacc=nacc+1
      IDEL(nstart+nacc+3)=jbas
      
      
      elseif(ivic.EQ.0.AND.itmp.EQ.lg)then
      nacc=nacc+1
      IDEL(nstart+nacc+3)=jbas
      endif
      endif
620   continue
      if(nacc.GT.0)then
      NDEL=NDEL+1
      IDEL(nstart+1)=1
      IDEL(nstart+2)=nacc
      IDEL(nstart+3)=ibas
      do 630 jb=1,nacc
      jbas=IDEL(nstart+jb+3)
      if(jbas.NE.ibas)then
      TRF(ibas,jbas)=zero
      TRF(jbas,ibas)=zero
      endif
630   continue
      nstart=nstart+nacc+3
      if(nstart.GT.LEN)stop 'INCREASE DIMENSION OF ARRAY IDEL'
      endif
      endif
700   continue
      
800   indx=0
      do 900 k=1,NDEL
      nset1=IDEL(indx+1)
      nset2=IDEL(indx+2)
      indx=indx+2
      nl=indx+1
      nu=indx+nset1
      write(Lfnpr,99015)
      write(Lfnpr,99016)(IDEL(i),i=nl,nu)
      write(Lfnpr,99017)
      nl=indx+nset1+1
      nu=indx+nset1+nset2
      write(Lfnpr,99016)(IDEL(i),i=nl,nu)
      indx=nu
900   continue
      return
1000  write(Lfnpr,99022)(keywd(i),i=1,3)
99022 format(1x,'No match with first three letters of the keywords ','fo
     &r deletion type'/' (ORBITAL,ELEMENT,BLOCK) found:',3A1)
      stop
1100  write(Lfnpr,99023)
99023 format(' There is an error in the input of deletions.')
      stop
      end
C* :1 * 
      
