
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getdel"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getdel.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "getdel.web"
      subroutine getdel(IBO,OCC,THR1,THR2,NL,LIST,DEL,DELOC,IFLG)
      implicit none
      double precision aukcal,conv,cutoff,DEL,DELOC,div,evkcal,OCC,one,t
     &enth,THR1,THR2,zero
      integer i,Iathy,IBO,Ibxm,Ichoos,IFLG,Iprint,Ipseud,Ispin,Iw3c,Iwap
     &ol,Iwcubf,Iwdetl,Iwdm,Iwfock,Iwhybs,Iwmulp,Iwpnao,Iwtnab,Iwtnao
      integer Iwtnbo,j,jbo,Jcore,Jprint,kbo,Kopt,Label,Larc,lbo,LIST,Lst
     &occ,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas
      integer Nbotyp,Nbouni,Ndim,NL
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbopt/Iwdm,Iw3c,Iwapol,Iwhybs,Iwpnao,Iwtnao,Iwtnab,Iwtnbo,I
     &wfock,Iwcubf,Ipseud,Kopt,Iprint,Iwdetl,Iwmulp,Ichoos,Jcore,Jprint(
     &60)
      dimension LIST(Ndim),DEL(Ndim),DELOC(Ndim,Ndim)
      
      data zero,one,cutoff,tenth/0.0D0,1.0D0,1.0D-4,0.1D0/
      data aukcal,evkcal/627.51,23.060/
      
      
      if(Munit.EQ.0)then
      conv=aukcal
      elseif(Munit.EQ.1)then
      conv=evkcal
      else
      conv=one
      endif
      
      
      do 100 jbo=1,Nbas
      LIST(jbo)=0
      DEL(jbo)=zero
100   continue
      
      NL=0
      if(OCC.LT.tenth)return
      do 200 jbo=1,Nbas
      if(IBO.NE.jbo)then
      if(Nbotyp(jbo).GE.10)then
      DEL(jbo)=DELOC(IBO,jbo)*DELOC(IBO,jbo)
      if(IFLG.EQ.0)then
      div=abs(DELOC(IBO,IBO)-DELOC(jbo,jbo))
      if(div.NE.zero)then
      DEL(jbo)=OCC*DEL(jbo)/div*conv
      else
      DEL(jbo)=zero
      endif
      endif
      endif
      if(DEL(jbo).GT.THR2.AND.Nbouni(IBO).NE.Nbouni(jbo))then
      NL=NL+1
      LIST(NL)=jbo
      elseif(DEL(jbo).GT.THR1)then
      NL=NL+1
      LIST(NL)=jbo
      endif
      endif
200   continue
      
      
      do 300 i=1,NL
      do 250 j=1,NL-1
      kbo=LIST(j)
      lbo=LIST(j+1)
      if(DEL(lbo)-DEL(kbo).GT.cutoff)then
      LIST(j)=lbo
      LIST(j+1)=kbo
      endif
250   continue
300   continue
      return
      
99001 format(1x,8I10)
99002 format(1x,8F10.4)
      end
C* :1 * 
      
