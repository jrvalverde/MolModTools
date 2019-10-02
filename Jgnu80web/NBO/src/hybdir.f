
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 hybdir"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "hybdir.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "hybdir.web"
      subroutine hybdir(BNDOCC,ATCOOR,THYB,TBND,SCR)
      implicit none
      double precision Accthr,ATCOOR,Athr,azi,BNDOCC,conv,Crtset,cutoff,
     &dev,Dthr,E2thr,Ethr,one,phi,phyb,pol,Prjset,Pthr,r,SCR
      double precision TBND,theta,thresh,Thrset,THYB,x,xyz,y,z,zero
      integer i,Iatcr,Iathy,Iatno,ib,ibas,Ibxm,icnt,ictr,ihyb,Ino,Iprin,
     &iskip,Ispin,istr,Iznuc,jctr,khyb,l3c,Label
      integer lbd,lbl1,lbl2,lbl3,lcr,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lf
     &nin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh
      integer Lfnpnl,Lfnppa,Lfnpr,lhyp,Ll,llp,lry,Lstocc,Ltyp,Ltyp1,Lu,M
     &AXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,nameat,Naoa,Naoc
      integer Naoctr,Naol,Natoms,Nbas,nctr,Ndim,Norbs
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Ltyp(MAXBAS),Iathy(MAXBAS,3)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbnao/Naoc(MAXBAS),Naoa(MAXBAS),Ltyp1(MAXBAS),Iprin(MAXBAS)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension BNDOCC(Ndim),ATCOOR(Natoms*3),THYB(Ndim,Ndim),TBND(Ndim,
     &Ndim),SCR(Ndim)
      dimension istr(8),phyb(3),xyz(3,2),khyb(3),azi(2),pol(2),dev(2)
      dimension iskip(2)
      
      data lcr,llp,lry,lbd,l3c/'CR','LP','RY','BD','3C'/
      data lhyp/'-'/
      data zero,one,thresh,cutoff/0.0D0,1.0D0,1.0D-4,1.0D-8/
      
      
      
      conv=180.0/(4.0*datan(one))
      write(Lfnpr,99001)dabs(Athr),dabs(Pthr),dabs(Ethr)
      
      
      call fecoor(ATCOOR)
      call fetnho(THYB)
      call fetnab(TBND)
      call trnspo(TBND,Ndim,Nbas)
      call matmlt(TBND,THYB,SCR,Ndim,Nbas)
      
      
      icnt=0
      do 100 ibas=1,Nbas
      ib=Ibxm(ibas)
      lbl1=Label(ib,1)
      lbl2=Label(ib,2)
      lbl3=Label(ib,3)
      if(lbl1.EQ.llp.OR.lbl1.EQ.lry)nctr=1
      if(lbl1.EQ.lbd)nctr=2
      
      
      if(lbl1.NE.l3c)then
      if(lbl1.NE.lcr)then
      if(BNDOCC(ibas).GE.dabs(Ethr))then
      
      
      ictr=0
      do 5 ihyb=1,Nbas
      if(dabs(TBND(ibas,ihyb)).GT.thresh)then
      ictr=ictr+1
      khyb(ictr)=ihyb
      endif
5     continue
      if(ictr.NE.nctr)then
      write(Lfnpr,99002)nctr,ibas,ictr
      stop
      endif
      
      
      do 10 ictr=1,nctr
      ihyb=khyb(ictr)
      jctr=Label(ib,ictr+3)
      call hybcmp(xyz(1,ictr),phyb(ictr),ihyb,jctr,THYB(1,ihyb))
10    continue
      
      
      iskip(1)=0
      iskip(2)=0
      if(nctr.NE.1.OR.phyb(1).GE.dabs(Pthr))then
      if(nctr.EQ.2)then
      if(phyb(1).LT.dabs(Pthr))iskip(1)=1
      if(phyb(2).LT.dabs(Pthr))iskip(2)=1
      if(iskip(1).EQ.1.AND.iskip(2).EQ.1)goto 100
      endif
      
      
      do 12 ictr=1,nctr
      if(iskip(ictr).NE.1)call angles(xyz(1,ictr),xyz(2,ictr),xyz(3,ictr
     &),pol(ictr),azi(ictr))
12    continue
      
      
      if(nctr.EQ.2)then
      ictr=Label(ib,4)
      jctr=Label(ib,5)
      x=ATCOOR(jctr*3-2)-ATCOOR(ictr*3-2)
      y=ATCOOR(jctr*3-1)-ATCOOR(ictr*3-1)
      z=ATCOOR(jctr*3)-ATCOOR(ictr*3)
      if(dabs(x).LT.cutoff)x=zero
      if(dabs(y).LT.cutoff)y=zero
      if(dabs(z).LT.cutoff)z=zero
      r=dsqrt(x*x+y*y+z*z)
      x=x/r
      y=y/r
      z=z/r
      call angles(x,y,z,theta,phi)
      dev(1)=acos(xyz(1,1)*x+xyz(2,1)*y+xyz(3,1)*z)*conv
      dev(1)=dabs(dev(1))
      dev(2)=acos(xyz(1,2)*x+xyz(2,2)*y+xyz(3,2)*z)*conv
      dev(2)=dabs(dabs(dev(2))-180.0)
      if(dev(1).LT.dabs(Athr))iskip(1)=1
      if(dev(2).LT.dabs(Athr))iskip(2)=1
      if(iskip(1).EQ.1.AND.iskip(2).EQ.1)goto 100
      endif
      
      
      icnt=icnt+1
      istr(1)=lbl1
      istr(2)=lbl2
      istr(3)=lbl3
      istr(4)=nameat(Iatno(Label(ib,4)))
      istr(5)=Label(ib,4)
      if(nctr.EQ.2)then
      istr(6)=lhyp
      istr(7)=nameat(Iatno(Label(ib,5)))
      istr(8)=Label(ib,5)
      if(iskip(1).EQ.1)then
      write(Lfnpr,99003)ibas,(istr(i),i=1,8),theta,phi,pol(2),azi(2),dev
     &(2)
      elseif(iskip(2).EQ.1)then
      write(Lfnpr,99004)ibas,(istr(i),i=1,8),theta,phi,pol(1),azi(1),dev
     &(1)
      else
      write(Lfnpr,99005)ibas,(istr(i),i=1,8),theta,phi,pol(1),azi(1),dev
     &(1),pol(2),azi(2),dev(2)
      endif
      else
      write(Lfnpr,99006)ibas,(istr(i),i=1,5),pol(1),azi(1)
      endif
      endif
      endif
      endif
      endif
100   continue
      if(icnt.EQ.0)write(Lfnpr,99007)
      return
      
99001 format(//1x,'NHO Directionality and "Bond Bending" (deviations ','
     &from line of nuclear centers)',//1x,'        [Thresholds for ','pr
     &inting:  angular deviation  > ',f4.1,' degree]',/1x,'             
     &                      hybrid p-character > ',f4.1,'%',/1x,'       
     &                            orbital occupancy  ','>  ',f4.2,'e',//
     &1x,'                      Line of Centers     ','   Hybrid 1      
     &        Hybrid 2',/1x,'                      ','---------------  -
     &------------------   ------------------',/1x,'          NBO       
     &    Theta   Phi    Theta   Phi    Dev    ','Theta   Phi    Dev',/1
     &x,'=====================================','=======================
     &===================')
99002 format(/1x,'Error: the ',i1,'-center NBO ',i3,' has ','contributio
     &ns from ',i2,' atomic centers.')
99003 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,i2,a1,a2,i2,3x,f5.1,2x,f5.1,
     &'     --     --    --     ',f5.1,2x,f5.1,1x,f5.1)
99004 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,i2,a1,a2,i2,3x,f5.1,2x,f5.1,
     &3x,f5.1,2x,f5.1,1x,f5.1,'      --     --    --')
99005 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,i2,a1,a2,i2,3x,f5.1,2x,f5.1,
     &3x,f5.1,2x,f5.1,1x,f5.1,4x,f5.1,2x,f5.1,1x,f5.1)
99006 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,i2,'          --     --',4x,
     &f5.1,2x,f5.1,'   --       --     --    --')
99007 format(1x,'   None exceeding thresholds')
      end
C* :1 * 
      
