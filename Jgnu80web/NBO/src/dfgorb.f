
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dfgorb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dfgorb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "dfgorb.web"
      subroutine dfgorb(RENORM,DM,T,ITRAN,IWCUBF,ITOPT,LFNPR)
      implicit none
      double precision a,b,DM,eight,four,one,renor,RENORM,rij,six,T,thre
     &e,two,x,zero
      integer i,ibas,id,idif,idtran,ifblk,iftran,igblk,igtran,Ispin,it,I
     &TOPT,ITRAN,IWCUBF,j,l,lang,Larc,Lbl,lf
      integer lf1,lf1t,lf2,lf3,lfcub,lfcubt,LFNPR,lft,lg,lg1,lg2,lg3,lgt
     &,List,Lorb,Lorbc,Lstemt,Lstocc,m,MAXATM
      integer MAXBAS,Munit,Mxao,Mxaolm,Mxbo,n1,n2,n3,n4,n5,n6,Naoctr,Nao
     &l,Natoms,Nbas,Ndim
      parameter(MAXATM=99,MAXBAS=500)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbbas/List(6,MAXBAS),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MAX
     &BAS),Lstemt(MAXBAS),Larc(MAXBAS),Lbl(MAXBAS),Lorbc(MAXBAS),Lorb(MA
     &XBAS)
      dimension T(Ndim,Ndim),DM(Ndim,Ndim),a(6,6),b(6),m(6),RENORM(Ndim)
     &,lf(3,3),lfcub(3,3),lft(3,3),lfcubt(3,3),lg(3,3),lgt(3,3)
      data lf/301,304,306,302,307,309,303,308,310/
      data lfcub/306,304,301,309,302,307,303,308,310/
      data lft/151,356,352,152,357,353,153,354,351/
      data lfcubt/151,355,351,152,356,352,153,357,353/
      data lg/402,407,409,403,408,410,405,412,414/
      data lgt/251,455,459,252,452,456,253,453,457/
      data zero,one,two,three,four,six,eight/0.0D0,1.0D0,2.0D0,3.0D0,4.0
     &D0,6.0D0,8.0D0/
      
      
      
      
      
      do 100 i=1,Nbas
      Lorb(i)=0
100   continue
      idtran=0
      n1=0
      n2=0
      n3=0
      n4=0
      n5=0
      n6=0
      do 200 ibas=1,Nbas
      if(Lorbc(ibas).EQ.201)then
      n1=n1+1
      List(1,n1)=ibas
      elseif(Lorbc(ibas).EQ.204)then
      n2=n2+1
      List(2,n2)=ibas
      elseif(Lorbc(ibas).EQ.206)then
      n3=n3+1
      List(3,n3)=ibas
      elseif(Lorbc(ibas).EQ.202)then
      n4=n4+1
      Lorb(ibas)=251
      elseif(Lorbc(ibas).EQ.203)then
      n5=n5+1
      Lorb(ibas)=252
      elseif(Lorbc(ibas).EQ.205)then
      n6=n6+1
      Lorb(ibas)=253
      endif
200   continue
      if(n1.EQ.n2.AND.n1.EQ.n3)then
      if(n1.EQ.n4.AND.n1.EQ.n5.AND.n1.EQ.n6)then
      idtran=n1
      if(idtran.NE.0)then
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=one
      a(2,2)=-one
      a(3,2)=zero
      a(1,3)=-one
      a(2,3)=-one
      a(3,3)=two
      if(ITOPT.NE.0)then
      do 205 j=1,3
      renor=RENORM(List(j,1))
      do 202 i=1,3
      a(i,j)=a(i,j)*renor
202   continue
205   continue
      call trnspo(a,6,3)
      endif
      do 210 id=1,idtran
      m(1)=List(1,id)
      m(2)=List(2,id)
      m(3)=List(3,id)
      if(ITOPT.NE.0)call limtrn(T,m,a,b,Ndim,Nbas,6,3,-1)
      if(ITOPT.EQ.0)then
      call limtrn(T,m,a,b,Ndim,Nbas,6,3,0)
      call limtrn(DM,m,a,b,Ndim,Nbas,6,3,0)
      Lorb(m(1))=51
      Lorb(m(2))=254
      Lorb(m(3))=255
      endif
210   continue
      endif
      iftran=0
      do 240 ifblk=1,3
      n1=0
      n2=0
      n3=0
      if(IWCUBF.NE.0)then
      lf1=lfcub(1,ifblk)
      lf2=lfcub(2,ifblk)
      lf3=lfcub(3,ifblk)
      else
      lf1=lf(1,ifblk)
      lf2=lf(2,ifblk)
      lf3=lf(3,ifblk)
      endif
      do 220 ibas=1,Nbas
      if(Lorbc(ibas).EQ.lf1)then
      n1=n1+1
      List(1,n1)=ibas
      elseif(Lorbc(ibas).EQ.lf2)then
      n2=n2+1
      List(2,n2)=ibas
      elseif(Lorbc(ibas).EQ.lf3)then
      n3=n3+1
      List(3,n3)=ibas
      endif
220   continue
      if(n1.NE.n2.OR.n1.NE.n3)goto 500
      if(ifblk.EQ.1)iftran=n1
      if((ifblk.NE.1).AND.(iftran.NE.n1))goto 500
      if(iftran.EQ.0)goto 280
      if(IWCUBF.NE.0)then
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=one
      a(2,2)=-one
      a(3,2)=zero
      a(1,3)=-three
      a(2,3)=-three
      a(3,3)=two
      elseif(ifblk.LE.1)then
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=one
      a(2,2)=-three
      a(3,2)=zero
      a(1,3)=-one
      a(2,3)=-one
      a(3,3)=four
      elseif(ifblk.EQ.3)then
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=one
      a(2,2)=-one
      a(3,2)=zero
      a(1,3)=-three
      a(2,3)=-three
      a(3,3)=two
      else
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=three
      a(2,2)=-one
      a(3,2)=zero
      a(1,3)=-one
      a(2,3)=-one
      a(3,3)=four
      endif
      if(ITOPT.NE.0)then
      do 225 j=1,3
      renor=RENORM(List(j,1))
      do 222 i=1,3
      a(i,j)=a(i,j)*renor
222   continue
225   continue
      call trnspo(a,6,3)
      endif
      do 230 it=1,iftran
      m(1)=List(1,it)
      m(2)=List(2,it)
      m(3)=List(3,it)
      if(ITOPT.NE.0)call limtrn(T,m,a,b,Ndim,Nbas,6,3,-1)
      if(ITOPT.EQ.0)then
      call limtrn(T,m,a,b,Ndim,Nbas,6,3,0)
      call limtrn(DM,m,a,b,Ndim,Nbas,6,3,0)
      endif
      if(IWCUBF.NE.0)then
      Lorb(m(1))=lfcubt(1,ifblk)
      Lorb(m(2))=lfcubt(2,ifblk)
      Lorb(m(3))=lfcubt(3,ifblk)
      else
      Lorb(m(1))=lft(1,ifblk)
      Lorb(m(2))=lft(2,ifblk)
      Lorb(m(3))=lft(3,ifblk)
      endif
230   continue
240   continue
      lf1=305
      lf1t=355
      if(IWCUBF.NE.0)lf1t=354
      n1=0
      do 260 ibas=1,Nbas
      if(Lorbc(ibas).EQ.lf1)then
      n1=n1+1
      Lorb(ibas)=lf1t
      endif
260   continue
      if(iftran.NE.n1)goto 500
280   igtran=0
      do 320 igblk=1,3
      n1=0
      n2=0
      n3=0
      lg1=lg(1,igblk)
      lg2=lg(2,igblk)
      lg3=lg(3,igblk)
      do 290 ibas=1,Nbas
      lang=Lorbc(ibas)
      if(lang.EQ.lg1)then
      n1=n1+1
      List(1,n1)=ibas
      elseif(lang.EQ.lg2)then
      n2=n2+1
      List(2,n2)=ibas
      elseif(lang.EQ.lg3)then
      n3=n3+1
      List(3,n3)=ibas
      endif
290   continue
      if(n1.NE.n2.OR.n1.NE.n3)goto 600
      if(igblk.EQ.1)igtran=n1
      if((igblk.NE.1).AND.(igtran.NE.n1))goto 600
      if(igtran.EQ.0)goto 380
      if(igblk.LE.1)then
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=one
      a(2,2)=-one
      a(3,2)=six
      a(1,3)=one
      a(2,3)=-one
      a(3,3)=zero
      elseif(igblk.EQ.3)then
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=-three
      a(2,2)=-three
      a(3,2)=four
      a(1,3)=three
      a(2,3)=-one
      a(3,3)=zero
      else
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(1,2)=-three
      a(2,2)=-three
      a(3,2)=four
      a(1,3)=one
      a(2,3)=-three
      a(3,3)=zero
      endif
      if(ITOPT.NE.0)then
      do 295 j=1,3
      renor=RENORM(List(j,1))
      do 292 i=1,3
      a(i,j)=a(i,j)*renor
292   continue
295   continue
      call trnspo(a,6,3)
      endif
      do 300 it=1,igtran
      m(1)=List(1,it)
      m(2)=List(2,it)
      m(3)=List(3,it)
      if(ITOPT.NE.0)call limtrn(T,m,a,b,Ndim,Nbas,6,3,-1)
      if(ITOPT.EQ.0)then
      call limtrn(T,m,a,b,Ndim,Nbas,6,3,0)
      call limtrn(DM,m,a,b,Ndim,Nbas,6,3,0)
      endif
      Lorb(m(1))=lgt(1,igblk)
      Lorb(m(2))=lgt(2,igblk)
      Lorb(m(3))=lgt(3,igblk)
300   continue
320   continue
      n1=0
      n2=0
      n3=0
      n4=0
      n5=0
      n6=0
      do 340 ibas=1,Nbas
      lang=Lorbc(ibas)
      if(lang.EQ.401)then
      n1=n1+1
      List(1,n1)=ibas
      elseif(lang.EQ.411)then
      n2=n2+1
      List(2,n2)=ibas
      elseif(lang.EQ.415)then
      n3=n3+1
      List(3,n3)=ibas
      elseif(lang.EQ.404)then
      n4=n4+1
      List(1,n4)=ibas
      elseif(lang.EQ.406)then
      n5=n5+1
      List(2,n5)=ibas
      elseif(lang.EQ.413)then
      n6=n6+1
      List(3,n6)=ibas
      endif
340   continue
      if(igtran.NE.n1.OR.n1.NE.n2.OR.n1.NE.n3)goto 600
      if(n1.NE.n4.OR.n1.NE.n5.OR.n1.NE.n6)goto 600
      a(1,1)=one
      a(2,1)=one
      a(3,1)=one
      a(4,1)=two
      a(5,1)=two
      a(6,1)=two
      a(1,2)=-one
      a(2,2)=-one
      a(3,2)=two
      a(4,2)=-two
      a(5,2)=one
      a(6,2)=one
      a(1,3)=one
      a(2,3)=-one
      a(3,3)=zero
      a(4,3)=zero
      a(5,3)=one
      a(6,3)=-one
      a(1,4)=three
      a(2,4)=three
      a(3,4)=eight
      a(4,4)=six
      a(5,4)=-six*four
      a(6,4)=-six*four
      a(1,5)=-one
      a(2,5)=-one
      a(3,5)=zero
      a(4,5)=six
      a(5,5)=-six
      a(6,5)=zero
      a(1,6)=one
      a(2,6)=one
      a(3,6)=zero
      a(4,6)=-six
      a(5,6)=zero
      a(6,6)=zero
      if(ITOPT.NE.0)then
      do 350 j=1,6
      renor=RENORM(List(j,1))
      do 345 i=1,6
      a(i,j)=a(i,j)*renor
345   continue
350   continue
      call trnspo(a,6,6)
      endif
      if(ITOPT.NE.0)call trnspo(a,6,6)
      do 360 it=1,igtran
      m(1)=List(1,it)
      m(2)=List(2,it)
      m(3)=List(3,it)
      m(4)=List(4,it)
      m(5)=List(5,it)
      m(6)=List(6,it)
      if(ITOPT.NE.0)call limtrn(T,m,a,b,Ndim,Nbas,6,6,-1)
      if(ITOPT.EQ.0)then
      call limtrn(T,m,a,b,Ndim,Nbas,6,6,0)
      call limtrn(DM,m,a,b,Ndim,Nbas,6,6,0)
      endif
      Lorb(m(1))=51
      Lorb(m(2))=254
      Lorb(m(3))=255
      Lorb(m(4))=451
      Lorb(m(5))=454
      Lorb(m(6))=458
360   continue
380   ITRAN=idtran+iftran+igtran
      if(ITOPT.NE.0)return
      if(ITRAN.NE.0)then
      do 390 i=1,Nbas
      x=T(i,i)
      RENORM(i)=one/sqrt(x)
390   continue
      do 400 i=1,Nbas
      do 395 j=1,Nbas
      rij=RENORM(i)*RENORM(j)
      T(i,j)=T(i,j)*rij
      DM(i,j)=DM(i,j)*rij
395   continue
400   continue
      endif
      do 420 i=1,Nbas
      if(Lorb(i).EQ.0)then
      lang=Lorbc(i)
      Lorb(i)=lang
      l=lang/100
      idif=lang-l*100
      if(idif.LE.50)Lorb(i)=Lorb(i)+50
      endif
420   continue
      return
      endif
      endif
      write(LFNPR,99001)
99001 format(' Unequal numbers of d function components were',' found in
     & the input.',/,' These cannot be properly transformed-','-perhaps 
     &they were improperly labelled.')
      stop
500   write(LFNPR,99002)
99002 format(' Unequal numbers of f function components were',' found in
     & the input.',/,' These cannot be properly transformed-','-perhaps 
     &they were improperly labelled.')
      stop
600   write(LFNPR,99003)
99003 format(' Unequal numbers of g function components were',' found in
     & the input.',/,' These cannot be properly transformed-','-perhaps 
     &they were improperly labelled.')
      stop
      end
C* :1 * 
      
