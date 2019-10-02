
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dipanl"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dipanl.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 30 "dipanl.web"
      subroutine dipanl(DM,T,C,TNBO,DX,DY,DZ,SCR,INDEX)
      implicit none
      double precision Accthr,Athr,C,Charge,couple,Crtset,debye,DM,Dthr,
     &DX,DY,DZ,E2thr,eta,Ethr,one,Prjset,Pthr,SCR,small
      double precision T,temp,tenten,tenth,Thrset,TNBO,toesu,tot,totnlm,
     &two,x,Xdip,xnbo,xnlmo,y,Ydip,ynbo,ynlmo,z,Zdip
      double precision zero,znbo,znlmo
      integer i,Iatcr,Iatno,ib,iblnk,Ibxm,icnt,ihyph,ii,INDEX,Ino,Ispin,
     &istr,itemp,Iznuc,j,j4,jj,k,Label
      integer Larc,Lbl,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnn
     &ab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,
     &Lfnpr
      integer Ll,Lorb,Lorbc,Lstocc,Lu,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mx
     &bo,nameat,Natoms,Nbas,Nbotyp,Nbouni,Ndim,nel,nocc,Norb
      logical test
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbdxyz/Xdip,Ydip,Zdip,Charge(MAXATM)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norb(MAXATM),Ll(MAXATM),Lu
     &(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      common/nbthr/Thrset,Prjset,Accthr,Crtset,E2thr,Athr,Pthr,Ethr,Dthr
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      dimension DM(Ndim,Ndim),T(Ndim,Ndim),C(Ndim,Ndim),TNBO(Ndim,Ndim),
     &DX(Ndim,Ndim),DY(Ndim,Ndim),DZ(Ndim,Ndim),SCR(Ndim*Ndim),INDEX(Ndi
     &m)
      dimension istr(14),couple(3)
      
      data tenten,small,zero,tenth,one,two/1.0D-10,1.0D-5,0.0D0,0.1D0,1.
     &0D0,2.0D0/
      data toesu/4.803242E-10/
      data ihyph,iblnk/1H-,1H /
      
      debye=toesu/tenten
      
      
      if(Alpha.OR..NOT.Open)then
      do 50 i=1,Natoms
      Charge(i)=Iznuc(i)
50    continue
      endif
      
      
      tot=zero
      do 100 i=1,Nbas
      tot=tot+DM(i,i)
      SCR(i)=DM(i,i)
100   continue
      nel=tot+tenth
      tot=nel
      nocc=nel
      if(.NOT.Open)nocc=nocc/2+mod(nocc,2)
      
      call rank(SCR,Nbas,Ndim,INDEX)
      do 200 i=1,nocc
      if(INDEX(i).GT.nocc)then
      write(Lfnpr,99001)
      return
      endif
200   continue
      
      
      eta=two
      if(Open)eta=one
      
      
      call fetlmo(C)
      call fetnbo(TNBO)
      ii=1
      call dipele(DX,C,TNBO,SCR,eta,nocc,ii)
      if(ii.EQ.0)return
      ii=2
      call dipele(DY,C,TNBO,SCR,eta,nocc,ii)
      if(ii.EQ.0)return
      ii=3
      call dipele(DZ,C,TNBO,SCR,eta,nocc,ii)
      if(ii.EQ.0)return
      
      
      call dipnuc(DX,DY,DZ,SCR,eta,nocc)
      
      
      do 300 i=1,nocc
      do 250 j=1,Nbas
      DX(j,i)=DX(j,i)*debye
      DY(j,i)=DY(j,i)*debye
      DZ(j,i)=DZ(j,i)*debye
250   continue
300   continue
      
      
      xnbo=zero
      ynbo=zero
      znbo=zero
      xnlmo=zero
      ynlmo=zero
      znlmo=zero
      do 600 i=1,nocc
      if(i.EQ.1)then
      if(Alpha)write(Lfnpr,99002)
      if(Beta)write(Lfnpr,99003)
      if(.NOT.Open)write(Lfnpr,99004)
      write(Lfnpr,99005)dabs(Dthr)
      else
      write(Lfnpr,99006)
      endif
      
      
      ib=Ibxm(i)
      istr(1)=Label(ib,1)
      istr(2)=Label(ib,2)
      istr(3)=Label(ib,3)
      do 350 j=1,3
      j4=4*j
      if(Label(ib,j+3).EQ.0)then
      do 310 k=j4-1,j4+2
      istr(k)=iblnk
310   continue
      else
      if(j.NE.1)istr(j4-1)=ihyph
      istr(j4)=nameat(Iatno(Label(ib,j+3)))
      call convrt(Label(ib,j+3),istr(j4+1),istr(j4+2))
      endif
350   continue
      
      
      x=zero
      y=zero
      z=zero
      do 400 j=1,Nbas
      x=x+DX(j,i)
      y=y+DY(j,i)
      z=z+DZ(j,i)
400   continue
      
      xnbo=xnbo+DX(i,i)
      ynbo=ynbo+DY(i,i)
      znbo=znbo+DZ(i,i)
      xnlmo=xnlmo+x
      ynlmo=ynlmo+y
      znlmo=znlmo+z
      
      
      tot=dsqrt(DX(i,i)*DX(i,i)+DY(i,i)*DY(i,i)+DZ(i,i)*DZ(i,i))
      totnlm=dsqrt(x*x+y*y+z*z)
      
      write(Lfnpr,99007)i,(istr(j),j=1,14),x,y,z,totnlm,DX(i,i),DY(i,i),
     &DZ(i,i),tot
      
      
      icnt=0
      do 450 j=1,Nbas
      if(j.NE.i)then
      tot=dsqrt(DX(j,i)*DX(j,i)+DY(j,i)*DY(j,i)+DZ(j,i)*DZ(j,i))
      if(tot.GT.dabs(Dthr))then
      icnt=icnt+1
      INDEX(icnt)=j
      SCR(icnt)=tot
      endif
      endif
450   continue
      
      do 500 j=1,icnt
      do 460 k=1,icnt-j
      if(SCR(k+1)-SCR(k).GT.small)then
      itemp=INDEX(k)
      INDEX(k)=INDEX(k+1)
      INDEX(k+1)=itemp
      temp=SCR(k)
      SCR(k)=SCR(k+1)
      SCR(k+1)=temp
      endif
460   continue
500   continue
      
      do 550 jj=1,icnt
      j=INDEX(jj)
      write(Lfnpr,99008)j,DX(j,i),DY(j,i),DZ(j,i),SCR(jj)
550   continue
600   continue
      
      
      if(.NOT.Alpha)then
      call fecoor(SCR)
      x=zero
      y=zero
      z=zero
      test=.FALSE.
      do 650 i=1,Natoms
      if(dabs(Charge(i)).GT.small)test=.TRUE.
      x=x+SCR(3*i-2)*Charge(i)*debye
      y=y+SCR(3*i-1)*Charge(i)*debye
      z=z+SCR(3*i)*Charge(i)*debye
650   continue
      if(test)then
      tot=dsqrt(x*x+y*y+z*z)
      write(Lfnpr,99009)x,y,z,tot,x,y,z,tot
      xnbo=xnbo+x
      ynbo=ynbo+y
      znbo=znbo+z
      xnlmo=xnlmo+x
      ynlmo=ynlmo+y
      znlmo=znlmo+z
      endif
      endif
      
      
      tot=dsqrt(xnbo*xnbo+ynbo*ynbo+znbo*znbo)
      totnlm=dsqrt(xnlmo*xnlmo+ynlmo*ynlmo+znlmo*znlmo)
      write(Lfnpr,99010)xnlmo,ynlmo,znlmo,totnlm,xnbo,ynbo,znbo,tot
      
      
      x=xnlmo-xnbo
      y=ynlmo-ynbo
      z=znlmo-znbo
      tot=dsqrt(x*x+y*y+z*z)
      write(Lfnpr,99011)x,y,z,tot
      
      
      test=.FALSE.
      do 800 i=1,Nbas
      if(i.GT.nocc.AND.dabs(DM(i,i)).GT.small)test=.TRUE.
      do 700 j=i+1,Nbas
      if(dabs(DM(j,i)).GT.small)test=.TRUE.
700   continue
800   continue
      if(test)then
      tot=zero
      do 850 k=1,3
      ii=k
      call fedxyz(DX,ii)
      call simtrs(DX,T,SCR,Ndim,Nbas)
      couple(k)=zero
      do 820 i=1,Nbas
      if(i.LE.nocc)then
      couple(k)=couple(k)+(eta-DM(i,i))*DX(i,i)
      else
      couple(k)=couple(k)-DM(i,i)*DX(i,i)
      endif
      do 810 j=i+1,Nbas
      couple(k)=couple(k)-two*DM(j,i)*DX(j,i)
810   continue
820   continue
      couple(k)=couple(k)*debye
      tot=tot+couple(k)*couple(k)
850   continue
      tot=dsqrt(tot)
      write(Lfnpr,99012)xnlmo,ynlmo,znlmo,totnlm,xnlmo,ynlmo,znlmo,totnl
     &m,(couple(k),k=1,3),tot
      xnlmo=xnlmo+couple(1)
      ynlmo=ynlmo+couple(2)
      znlmo=znlmo+couple(3)
      totnlm=dsqrt(xnlmo*xnlmo+ynlmo*ynlmo+znlmo*znlmo)
      if(Alpha)write(Lfnpr,99013)xnlmo,ynlmo,znlmo,totnlm
      if(Beta)write(Lfnpr,99014)xnlmo,ynlmo,znlmo,totnlm
      if(.NOT.Open)write(Lfnpr,99015)xnlmo,ynlmo,znlmo,totnlm
      else
      if(Alpha)write(Lfnpr,99013)xnlmo,ynlmo,znlmo,totnlm,xnlmo,ynlmo,zn
     &lmo,totnlm
      if(Beta)write(Lfnpr,99014)xnlmo,ynlmo,znlmo,totnlm,xnlmo,ynlmo,znl
     &mo,totnlm
      if(.NOT.Open)write(Lfnpr,99015)xnlmo,ynlmo,znlmo,totnlm,xnlmo,ynlm
     &o,znlmo,totnlm
      endif
      
      
      if(Alpha)then
      Xdip=xnlmo
      Ydip=ynlmo
      Zdip=znlmo
      endif
      
      
      if(Beta)then
      xnlmo=xnlmo+Xdip
      ynlmo=ynlmo+Ydip
      znlmo=znlmo+Zdip
      totnlm=dsqrt(xnlmo*xnlmo+ynlmo*ynlmo+znlmo*znlmo)
      write(Lfnpr,99015)xnlmo,ynlmo,znlmo,totnlm
      endif
      return
      
99001 format(/1x,'The highest occupied NBOs are not at the beginning ','
     &of the list.',/1x,'The dipole moment analysis is currently not',' 
     &set up to handle this.')
99002 format(//1x,'Dipole moment analysis, alpha spin:')
99003 format(//1x,'Dipole moment analysis, beta spin:')
99004 format(//1x,'Dipole moment analysis:')
99005 format(/1x,'[Print threshold: Net dipole >',f5.2,' Debye]',//1x,' 
     &                               NLMO bond dipole            ','NBO 
     &bond dipole',/1x,'                            ----------','-------
     &--------  ------------------------',/1x,'         ','Orbital      
     &        x     y     z   Total      x     y     ','z   Total',/1x,7
     &9('='))
99006 format(1x)
99007 format(1x,i3,'. ',a2,a1,'(',i2,')',a2,3A1,a2,3A1,a2,2A1,1x,4F6.2,3
     &x,4F6.2)
99008 format(1x,44x,'deloc ',i3,':',4F6.2)
99009 format(/1x,'  Residual nuclear charge  ',4F6.2,'   ',4F6.2)
99010 format(1x,'                           -----------------------','--
     &---------------------------',/1x,'        Net dipole moment','  ',
     &4F6.2,'   ',4F6.2)
99011 format(1x,'Delocalization correction  ',24x,'   ',4F6.2,/1x,'     
     &                      -----------------------------','------------
     &-----------')
99012 format(1x,'        Net dipole moment  ',4F6.2,'   ',4F6.2,/1x,' NL
     &MO coupling correction  ',4F6.2,/1x,'                  ','        
     & -------------------------')
99013 format(1x,'        Alpha spin dipole  ',4F6.2,'   ',4F6.2)
99014 format(1x,'         Beta spin dipole  ',4F6.2,'   ',4F6.2)
99015 format(1x,'      Total dipole moment  ',4F6.2,'   ',4F6.2)
      end
C* :1 * 
      
