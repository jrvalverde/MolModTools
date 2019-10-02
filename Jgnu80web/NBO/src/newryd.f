
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 newryd"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "newryd.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "newryd.web"
      subroutine newryd(T,S,TPNAO,DMBLK,SBLK,EVECT,OCC,EVAL,EVAL2,LIST,I
     &RPNAO)
      implicit none
      double precision DMBLK,EVAL,EVAL2,EVECT,OCC,one,S,SBLK,T,TPNAO
      integer i,iadd,il,ilbl,im1,imax,inao,iorb,IRPNAO,Ispin,j,jorb,jorb
     &l,Label,Larc,Lbl,LIST,Lorb,Lorbc,Lstemt
      integer Lstocc,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol,Na
     &toms,Nbas,nc,Ndim,nl,nm,nrydc,nskip,nstart
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(M
     &AXBAS)
      dimension T(Ndim,Ndim),S(Ndim,Ndim),TPNAO(Ndim,Ndim),OCC(Ndim),DMB
     &LK(Mxaolm,Mxaolm),SBLK(Mxaolm,Mxaolm),EVAL(Nbas),EVECT(Mxaolm,Mxao
     &lm),EVAL2(Nbas),LIST(Mxaolm)
      data one/1.0D0/
      
      
      
      if(IRPNAO.EQ.1)call fepnao(TPNAO)
      
      nl=1
      iorb=0
100   iorb=iorb+nl
      if(iorb.LE.Nbas)then
      nl=1
      ilbl=Naoctr(iorb)
      il=Naol(iorb)/100
      nm=il*2+1
      imax=Nbas-iorb
      do 150 iadd=1,imax
      jorb=iorb+iadd
      jorbl=Naol(jorb)/100
      if(Naoctr(jorb).NE.ilbl.OR.jorbl.NE.il)goto 200
      nl=nl+1
150   continue
200   nc=nl/nm
      nskip=0
      imax=iorb-1+nc
      do 250 i=1,Nbas
      inao=Lstocc(i)
      if(inao.GE.iorb.AND.inao.LE.imax)nskip=nskip+1
250   continue
      if(nskip.NE.nc)then
      nstart=nskip+1
      nrydc=nc-nskip
      call rydiag(T,S,TPNAO,DMBLK,SBLK,OCC,EVAL,EVECT,EVAL2,iorb,nc,nm,n
     &start,nrydc,Larc,LIST,IRPNAO)
      endif
      goto 100
      endif
      do 400 i=1,Nbas
      im1=i-1
      do 300 j=1,im1
      S(j,i)=S(i,j)
300   continue
      S(i,i)=one
400   continue
      
      
      if(IRPNAO.EQ.1)call svpnao(TPNAO)
      return
      end
C* :1 * 
      
