
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 nbocla"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "nbocla.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "nbocla.web"
      subroutine nbocla(BNDOCC,ACCTHR)
      implicit none
      double precision ACCTHR,BNDOCC,donthr,one,thresh,two,zero
      integer iat,Iathy,iatmol,ib,ibas,Ibxm,imol,imolat,Ispin,l3c,lab,La
     &bel,lbd,lstar,Lstocc,MAXATM,MAXBAS,Molat,Molata,Molec
      integer Moleca,Mollst,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,Nbotyp,Nb
     &ouni,nctr,Ndim,Nmola,Nmolec
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Mollst(MAXBAS),Iathy(MAXBAS,3)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbmol/Nmolec,Molat(MAXATM),Molec(MAXATM,MAXATM),Nmola,Molat
     &a(MAXATM),Moleca(MAXATM,MAXATM)
      dimension BNDOCC(Nbas)
      data lbd,l3c,lstar/2HBD,2H3C,1H*/
      data thresh,one,zero,two/1.50D0,1.0D0,0.0D0,2.0D0/
      data donthr/1.0D-1/
      
      
      if(ACCTHR.LE.zero)then
      ACCTHR=thresh
      if(Ispin.NE.0)ACCTHR=ACCTHR-one
      endif
      if(Ispin.NE.0)donthr=donthr/two
      
      
      do 200 iat=1,Natoms
      do 50 imol=1,Nmolec
      imolat=Molat(imol)
      do 20 iatmol=1,imolat
      if(Molec(imol,iatmol).EQ.iat)goto 100
20    continue
50    continue
      stop 'ROUTINE NBOCLA'
100   Mollst(iat)=imol
200   continue
      
      do 300 ibas=1,Nbas
      ib=Ibxm(ibas)
      iat=Label(ib,4)
      imol=Mollst(iat)
      Nbouni(ibas)=imol
      lab=Label(ib,1)
      nctr=1
      if(lab.EQ.lbd)nctr=2
      if(lab.EQ.l3c)nctr=3
      Nbotyp(ibas)=nctr
      if(Label(ib,2).EQ.lstar)then
      
      
      Nbotyp(ibas)=nctr+20
      
      
      if(BNDOCC(ibas).GT.donthr)Nbotyp(ibas)=nctr+10
      elseif(BNDOCC(ibas).LE.ACCTHR)then
      
      
      Nbotyp(ibas)=nctr+10
      endif
300   continue
      return
      end
C* :1 * 
      
