
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 anlyze"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "anlyze.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "anlyze.web"
      subroutine anlyze(T,BNDOCC,HYB,HYCOEF,THYB)
      implicit none
      double precision Accthr,Athr,BNDOCC,coef,coefsq,Crtset,Dthr,E2thr,
     &Ethr,hundrd,HYB,HYCOEF,occcr,occevr,occlew,occnon,occvl,occvnl,pce
     &nt,pct
      double precision pow,Prjset,Pthr,std,T,t99,t99p,tenth,thresh,Thrse
     &t,THYB,totele,tthoth,zero
      integer i,ia,Iatcr,Iathy,Iatno,ib,Ibxm,ich,Ichoos,ictr,Ino,ipar3c,
     &Iprin,Iprint,Ipseud,isgn,isp,Ispin,Iw3c,Iwapol
      integer Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtn
     &ao,Iwtnbo,Iznuc,j,Jcore,Jprint,k,kh,kl,Kopt,ku,l
      integer l2blnk,l3c,Label,lbd,lbl,lblnk,lcr,Lfnao,Lfnarc,Lfndaf,Lfn
     &def,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna
      integer Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,lhyp,Ll,llp,lname,lry,ls
     &tar,lstd,Lstocc,Ltyp,Ltyp1,MAXATM,MAXBAS,mcr,mecp,mhyb
      integer mlew,Munit,mvl,Mxao,Mxaolm,Mxbo,nam,nameat,Naoa,Naoc,Naoct
     &r,Naol,Natoms,Nbas,nbond,nctr,Ndim,nel,nhyb,nl
      integer nl1,Norbs
      integer Ul
      
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),U
     &l(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbnao/Naoc(MAXBAS),Naoa(MAXBAS),Ltyp1(MAXBAS),Iprin(MAXBAS)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Ltyp(MAXBAS),Iathy(MAXBAS,3)
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      dimension T(Ndim,Ndim),HYB(Mxao),BNDOCC(Ndim),THYB(Ndim,Ndim),pct(
     &5),pow(5),lname(5),isp(3),nam(3),ich(3,2),HYCOEF(Ndim)
      data llp,lbd,l3c,lcr,lry/'LP','BD','3C','CR','RY'/
      data lname/'s','p','d','f','g'/
      data zero,thresh,t99,t99p/0.0D0,1.D-2,99.99D0,99.995D0/
      data tenth,hundrd,tthoth/0.1D0,100.0D0,0.0001D0/
      data lhyp,lblnk,lstar,l2blnk/'-',' ','*','  '/
      
      
      totele=zero
      do 100 i=1,Nbas
      totele=totele+BNDOCC(i)
100   continue
      totele=totele+tenth
      nel=totele
      totele=nel
      
      
      mcr=0
      occcr=zero
      occvl=zero
      occvnl=zero
      do 200 i=1,Nbas
      if(Label(Ibxm(i),2).NE.lstar)then
      if(Label(Ibxm(i),1).EQ.lcr)then
      mcr=mcr+1
      occcr=occcr+BNDOCC(i)
      else
      occvl=occvl+BNDOCC(i)
      endif
      elseif(Label(Ibxm(i),1).NE.lry)then
      occvnl=occvnl+BNDOCC(i)
      endif
200   continue
      occevr=totele-occcr-occvl-occvnl
      if(Ispin.EQ.0)mcr=2*mcr
      mvl=nel-mcr
      mecp=0
      if(Ipseud.NE.0)then
      do 250 i=1,Natoms
      mecp=mecp+Iatno(i)-Iznuc(i)
250   continue
      if(Ispin.NE.0)mecp=mecp/2
      endif
      mlew=mcr+mvl+mecp
      occlew=occcr+occvl+mecp
      occnon=occvnl+occevr
      
      
      if(Jprint(5).EQ.1.AND.nel.NE.0)then
      write(Lfnpr,99010)
      if(Ipseud.NE.0)write(Lfnpr,99011)dble(mecp)
      if(mcr.NE.0)then
      pcent=occcr/mcr*hundrd
      write(Lfnpr,99012)occcr,pcent,mcr
      endif
      if(mvl.NE.0)then
      pcent=occvl/mvl*hundrd
      write(Lfnpr,99013)occvl,pcent,mvl
      endif
      write(Lfnpr,99014)
      pcent=occlew/mlew*hundrd
      write(Lfnpr,99015)occlew,pcent,mlew
      write(Lfnpr,99016)
      pcent=occvnl/mlew*hundrd
      write(Lfnpr,99017)occvnl,pcent,mlew
      pcent=occevr/mlew*hundrd
      write(Lfnpr,99018)occevr,pcent,mlew
      write(Lfnpr,99014)
      pcent=occnon/mlew*hundrd
      write(Lfnpr,99019)occnon,pcent,mlew
      write(Lfnpr,99020)
      endif
      
      
      if(Jprint(5).EQ.1)then
      write(Lfnpr,99001)
      write(Lfnpr,99002)(lhyp,j=1,79)
      endif
      
      
      nhyb=0
      mhyb=0
      ipar3c=1
      do 500 nbond=1,Nbas
      ib=Ibxm(nbond)
      lbl=Label(ib,1)
      if(lbl.EQ.llp.OR.lbl.EQ.lcr.OR.lbl.EQ.lry)nctr=1
      if(lbl.EQ.lbd)nctr=2
      if(lbl.EQ.l3c)nctr=3
      do 300 i=1,3
      ia=Label(ib,i+3)
      call convrt(ia,ich(i,1),ich(i,2))
      nam(i)=l2blnk
      if(ia.GT.0)nam(i)=nameat(Iatno(ia))
      isp(i)=lhyp
      if(i.GE.nctr)isp(i)=lblnk
300   continue
      
      
      do 400 ictr=1,nctr
      i=Label(ib,ictr+3)
      nel=nameat(Iatno(i))
      kl=Ll(i)
      ku=Ul(i)
      do 320 k=1,Mxao
      Ltyp(k)=0
      HYB(k)=zero
320   continue
      
      
      isgn=1
      if(Label(ib,2).EQ.lstar)then
      if(ictr.GE.2)then
      if(ictr.EQ.3)ipar3c=-ipar3c
      if(ictr.NE.3.OR.ipar3c.LE.0)isgn=-isgn
      endif
      endif
      
      
      kh=0
      do 340 k=kl,ku
      kh=kh+1
      HYB(kh)=T(k,nbond)
      Ltyp(kh)=Naoa(k)/100
340   continue
      call htype(HYB,Ltyp,Mxao,kh,coef,pct,nl,isgn)
      
      
      lstd=0
      do 360 l=1,nl
      if(lstd.LE.0)then
      pow(l)=zero
      std=pct(l)
      if(std.LT.thresh)goto 360
      lstd=l
      endif
      pow(l)=pct(l)/std
      if(pow(l).GT.t99p)pow(l)=t99
360   continue
      
      
      coefsq=coef*coef*hundrd
      nl1=nl
      if(nl1.GT.3)nl1=3
      if(ictr.EQ.1.AND.nctr.EQ.1.AND.Jprint(5).EQ.1)write(Lfnpr,99003)nb
     &ond,BNDOCC(nbond),(Label(ib,k),k=1,3),nam(1),ich(1,1),ich(1,2),pct
     &(1),(lname(l),pow(l),pct(l),l=2,nl1)
      if(ictr.EQ.1.AND.nctr.GT.1.AND.Jprint(5).EQ.1)write(Lfnpr,99004)nb
     &ond,BNDOCC(nbond),(Label(ib,k),k=1,3),(nam(k),ich(k,1),ich(k,2),is
     &p(k),k=1,3)
      if(nctr.NE.1.AND.Jprint(5).EQ.1)write(Lfnpr,99005)coefsq,coef,nel,
     &i,pct(1),(lname(l),pow(l),pct(l),l=2,nl1)
      if(nl.GT.3.AND.Jprint(5).EQ.1)write(Lfnpr,99006)(lname(l),pow(l),p
     &ct(l),l=4,nl)
      if(Iwhybs.NE.0.AND.BNDOCC(nbond).GT.tthoth.AND.Jprint(5).EQ.1)writ
     &e(Lfnpr,99007)(HYB(k),k=1,kh)
      call frmhyb(HYB,THYB,coef,HYCOEF,kl,ku,nhyb)
      
      
      if(mhyb.NE.nhyb)then
      mhyb=nhyb
      call lblnho(nhyb,nbond,ictr,nctr)
      endif
400   continue
500   continue
      return
      
99001 format(//,1x,'    (Occupancy)   Bond orbital/ Coefficients/ ','Hyb
     &rids')
99002 format(1x,80A1)
99003 format(1x,i3,'. (',f7.5,') ',a2,a1,'(',i2,')',a2,2A1,12x,' s(',f6.
     &2,'%)',2(a1,f5.2,'(',f6.2,'%)'))
99004 format(1x,i3,'. (',f7.5,') ',a2,a1,'(',i2,')',3(a2,3A1))
99005 format(16x,'(',f6.2,'%)',2x,f7.4,'*',a2,i2,' s(',f6.2,'%)',2(a1,f5
     &.2,'(',f6.2,'%)'))
99006 format(50x,2(a1,f5.2,'(',f6.2,'%)'))
99007 format(39x,5F8.4)
99008 format(i3)
99009 format(5F14.9)
99010 format(/,1x,56('-'))
99011 format(1x,'  Effective Core          ',f9.5)
99012 format(1x,'  Core                    ',f9.5,' (',f7.3,'% of ',i3,'
     &)')
99013 format(1x,'  Valence Lewis           ',f9.5,' (',f7.3,'% of ',i3,'
     &)')
99014 format(2x,18('='),7x,28('='))
99015 format(1x,'  Total Lewis             ',f9.5,' (',f7.3,'% of ',i3,'
     &)')
99016 format(2x,53('-'))
99017 format(1x,'  Valence non-Lewis       ',f9.5,' (',f7.3,'% of ',i3,'
     &)')
99018 format(1x,'  Rydberg non-Lewis       ',f9.5,' (',f7.3,'% of ',i3,'
     &)')
99019 format(1x,'  Total non-Lewis         ',f9.5,' (',f7.3,'% of ',i3,'
     &)')
99020 format(1x,56('-'))
      end
C* :1 * 
      
