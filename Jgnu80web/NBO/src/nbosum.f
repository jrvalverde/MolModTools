
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbosum"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbosum.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "nbosum.web"
      subroutine nbosum(F,BNDOCC,LIST,LISTA,SCR)
      implicit none
      double precision Accthr,Athr,BNDOCC,charge,Crtset,Dthr,E2thr,enrg,
     &Ethr,F,hundrd,occ,occlew,occnon,occryd,Prjset,Pthr,SCR,ten,tenth
      double precision thr,thr1,thr2,Thrset,total,two,zero
      integer i,iat,Iatcr,Iathy,Iatno,ib,ibas,Ibxm,Ichoos,ictr,iflg,il,i
     &lab,imol,Ino,Iprint,Ipseud,iptr,Ispin,istr
      integer Iw3c,Iwapol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao
     &,Iwtnab,Iwtnao,Iwtnbo,Iznuc,j,jat,Jcore,Jprint,kat,Kopt,Label
      integer Larc,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,L
     &fnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnp
     &r,LIST
      integer LISTA,Ll,lry,lstar,Lstocc,Lu,MAXATM,MAXBAS,mecp,ml,Molat,M
     &olata,Molec,Moleca,Munit,Mxao,Mxaolm,Mxbo,nameat,nat
      integer Natoms,Nbas,Nbotyp,Nbouni,nctr,Ndim,nel,nl,Nmola,Nmolec,No
     &rbs,ntri
      logical first
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbmol/Nmolec,Molat(MAXATM),Molec(MAXATM,MAXATM),Nmola,Molat
     &a(MAXATM),Moleca(MAXATM,MAXATM)
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension F(Ndim,Ndim),BNDOCC(Ndim),LIST(Ndim),LISTA(Natoms,2),SCR
     &(1)
      dimension istr(80),ilab(9)
      
      data zero,two,ten,hundrd/0.0D0,2.0D0,1.0D1,1.0D2/
      data tenth/1.0D-1/
      data lstar,lry/'*','RY'/
      
      
      iflg=0
      
      
      thr1=dabs(E2thr)
      if(Ispin.NE.0)thr=thr/two
      thr2=thr1/ten
      
      
      if(Iwfock.NE.0)then
      ntri=Ndim*(Ndim+1)/2
      call fefnbo(F)
      call unpack(F,Ndim,Nbas,ntri)
      endif
      
      
      if(Iwfock.NE.0)then
      write(Lfnpr,99001)
      else
      write(Lfnpr,99002)
      endif
      do 200 imol=1,Nmolec
      
      
      nat=0
      mecp=0
      charge=zero
      do 50 iat=1,Molat(imol)
      kat=Iatno(Molec(imol,iat))
      mecp=mecp+dble(kat-Iznuc(Molec(imol,iat)))
      charge=charge+dble(kat)
      do 20 jat=1,nat
      if(LISTA(jat,1).EQ.kat)then
      LISTA(jat,2)=LISTA(jat,2)+1
      goto 50
      endif
20    continue
      nat=nat+1
      LISTA(nat,1)=kat
      LISTA(nat,2)=1
50    continue
      if(Ispin.NE.0)mecp=mecp/2
      if(Ispin.NE.0)charge=charge/two
      call chem(nat,Natoms,LISTA,nl,istr)
      write(Lfnpr,99003)imol,(istr(i),i=1,nl)
      
      
      occlew=dble(mecp)
      occnon=zero
      occryd=zero
      do 100 ibas=1,Nbas
      if(Nbouni(ibas).EQ.imol)then
      ib=Ibxm(ibas)
      ilab(1)=Label(ib,1)
      ilab(2)=Label(ib,2)
      ilab(3)=Label(ib,3)
      iptr=3
      nctr=mod(Nbotyp(ibas),10)
      do 60 ictr=1,nctr
      iptr=iptr+2
      ilab(iptr)=Label(ib,ictr+3)
      ilab(iptr-1)=nameat(Iatno(ilab(iptr)))
60    continue
      occ=BNDOCC(ibas)
      if(ilab(1).EQ.lry)then
      occryd=occryd+occ
      elseif(ilab(2).EQ.lstar)then
      occnon=occnon+occ
      else
      occlew=occlew+occ
      endif
      
      
      if(Iwfock.NE.0)then
      enrg=F(ibas,ibas)
      call getdel(ibas,occ,thr1,thr2,nl,LIST,SCR,F,iflg)
      first=.TRUE.
      il=0
65    call dlcstr(ibas,il,nl,LIST,ml,istr)
      if(first)then
      if(nctr.EQ.1)then
      write(Lfnpr,99004)ibas,(ilab(i),i=1,iptr),occ,enrg,(istr(j),j=1,ml
     &)
      elseif(nctr.EQ.2)then
      write(Lfnpr,99005)ibas,(ilab(i),i=1,iptr),occ,enrg,(istr(j),j=1,ml
     &)
      else
      write(Lfnpr,99006)ibas,(ilab(i),i=1,iptr),occ,enrg,(istr(j),j=1,ml
     &)
      endif
      first=.FALSE.
      else
      write(Lfnpr,99007)(istr(j),j=1,ml)
      endif
      if(il.LT.nl)goto 65
      
      
      elseif(nctr.EQ.1)then
      write(Lfnpr,99004)ibas,(ilab(i),i=1,iptr),occ
      elseif(nctr.EQ.2)then
      write(Lfnpr,99005)ibas,(ilab(i),i=1,iptr),occ
      else
      write(Lfnpr,99006)ibas,(ilab(i),i=1,iptr),occ
      endif
      endif
100   continue
      write(Lfnpr,99008)
      total=occlew+occnon+occryd
      
      
      if(Nmolec.EQ.1)then
      total=total+tenth
      nel=total
      total=nel
      occryd=total-occlew-occnon
      endif
      
      
      if(dabs(total-dble(nint(total))).LT.1.0D-5)total=dble(nint(total))
      charge=charge-total
      write(Lfnpr,99009)occlew,occlew/total*hundrd
      write(Lfnpr,99010)occnon,occnon/total*hundrd
      write(Lfnpr,99011)occryd,occryd/total*hundrd
      write(Lfnpr,99008)
      write(Lfnpr,99012)imol,total,hundrd
      write(Lfnpr,99013)imol,charge
200   continue
      return
      
99001 format(//1x,'Natural Bond Orbitals (Summary):',//53x,'Principal ',
     &'Delocalizations',/1x,'          NBO              Occupancy  ','  
     &Energy      (geminal,vicinal,remote)',/1x,79('='))
99002 format(//1x,'Natural Bond Orbitals (Summary):',//1x,'          ','
     &NBO              Occupancy  ',/1x,40('-'))
99003 format(1x,'Molecular unit ',i2,'  ',60A1)
99004 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,i2,10x,f9.5,f12.5,4x,28A1)
99005 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,i2,'-',a2,i2,5x,f9.5,f12.5,4
     &x,28A1)
99006 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,i2,'-',a2,i2,'-',a2,i2,f9.5,
     &f12.5,4x,28A1)
99007 format(52x,28A1)
99008 format(1x,'      -------------------------------')
99009 format(1x,'             Total Lewis',f11.5,'  (',f8.4,'%)')
99010 format(1x,'       Valence non-Lewis',f11.5,'  (',f8.4,'%)')
99011 format(1x,'       Rydberg non-Lewis',f11.5,'  (',f8.4,'%)')
99012 format(1x,'           Total unit ',i2,f11.5,'  (',f8.4,'%)')
99013 format(1x,'          Charge unit ',i2,f11.5,/)
      end
C* :1 * 
      
