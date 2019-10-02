
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 efill"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "efill.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "efill.web"
      subroutine efill(ISHELL,JSHELL,KSHELL,LSHELL,LA,LB,LC,LD,AX1,ISCF,
     &DM,DN,E,DMAX)
      implicit none
      double precision AX1,C1,C2,C3,C4,d12,d1234,d12b,d13,d13b,d14,d14b,
     &d23,d23b,d24,d24b,d34,d34b,DM,DMAX
      double precision DN,E,Exx,gabs,h,p25,Shladf,X,Y,Z,zero
      integer i,iaos,ias,id,ISCF,iscfp,ISHELL,j,Jan,jaos,jas,JSHELL,k,ka
     &os,kas,KSHELL,l,LA,laos,las
      integer LB,LC,LD,LENB,lii,lij,lik,lil,ljj,ljk,ljl,lkk,lkl,lll,LSHE
     &LL,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp
      integer Nshell
      integer Shella,Shelln,Shellt,Shellc,Aos,Aon
      logical open,complx
      dimension DM(*),DN(*),E(*)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      data zero/0.0D0/,p25/0.25D0/,h/0.5D0/
      
      
      
      
      
      
      
      iscfp=ISCF+1
      open=ISCF.EQ.1
      complx=ISCF.EQ.2
      
      iaos=Aos(ISHELL)-1
      jaos=Aos(JSHELL)-1
      kaos=Aos(KSHELL)-1
      laos=Aos(LSHELL)-1
      
      DMAX=zero
      do 100 l=1,LD
      las=laos+l
      lll=las*(las-1)/2
      
      do 50 k=1,LC
      kas=kaos+k
      lkk=kas*(kas-1)/2
      lkl=lkk+las
      if(kas.LT.las)lkl=lll+kas
      d34=DM(lkl)
      if(open)d34b=DN(lkl)
      
      do 20 j=1,LB
      id=16*j+4*k+l-84
      jas=jaos+j
      ljj=jas*(jas-1)/2
      ljk=ljj+kas
      if(jas.LT.kas)ljk=lkk+jas
      d23=DM(ljk)
      ljl=ljj+las
      if(jas.LT.las)ljl=lll+jas
      d24=DM(ljl)
      if(open.OR.complx)then
      d23b=DN(ljk)
      d24b=DN(ljl)
      if(complx)then
      if(kas.LT.jas)d23b=-d23b
      if(jas.LT.las)d24b=-d24b
      endif
      endif
      
      do 10 i=1,LA
      id=id+64
      ias=iaos+i
      lii=ias*(ias-1)/2
      lij=lii+jas
      if(ias.LT.jas)lij=ljj+ias
      d12=DM(lij)
      lik=lii+kas
      if(ias.LT.kas)lik=lkk+ias
      d13=DM(lik)
      lil=lii+las
      if(ias.LT.las)lil=lll+ias
      d14=DM(lil)
      
      if(iscfp.EQ.2)then
      
      d12b=DN(lij)
      d13b=DN(lik)
      d14b=DN(lil)
      d1234=((d12+d12b)*(d34+d34b)-h*(d13*d24+d23*d14+d13b*d24b+d23b*d14
     &b))*AX1
      elseif(iscfp.EQ.3)then
      
      d14b=DN(lil)
      d13b=DN(lik)
      if(ias.LT.las)d14b=-d14b
      if(kas.LT.ias)d13b=-d13b
      
      d1234=(d12*d34-p25*(d13*d24+d23*d14-d13b*d24b-d23b*d14b))*AX1
      elseif(iscfp.NE.4)then
      
      d1234=(d12*d34-p25*(d13*d24+d23*d14))*AX1
      endif
      
      E(id)=d1234
      d1234=gabs(d1234)
      if(d1234.GT.DMAX)DMAX=d1234
10    continue
20    continue
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
