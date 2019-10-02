
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 efill1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "efill1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 37 "efill1.web"
      subroutine efill1(ISHELL,JSHELL,KSHELL,LSHELL,ISTART,JSTART,KSTART
     &,LSTART,IEND,JEND,KEND,LEND,IMJ,IMKJML,KML,ISCF,DA,DB,D1234,DNSMAX
     &)
      implicit none
      double precision C1,C2,C3,C4,cc2,cc4,d12,D1234,d12b,d13,d13b,d13q,
     &d14,d14b,d23,d23b,d23q,d24,d24b,d34
      double precision d34b,DA,DB,DNSMAX,dtemp,Exx,gabs,half,pt25,sigmas
     &,X,Y,Z,zero
      integer i,IEND,ijkl,IMJ,IMKJML,ISCF,iscfp,ISHELL,ist,ISTART,j,Jan,
     &JEND,JSHELL,jst,JSTART,k,KEND,KML,KSHELL
      integer kst,KSTART,l,Lamax,lambda,Lbmax,Lbound,Lcmax,Ldmax,LENB,LE
     &ND,lmbdas,lmbsgm,Lpmax,Lpqmax,Lqmax,LSHELL,lst,LSTART,MAXPRM
      integer MAXS21,MAXSH1,MAXSHL,Maxtyp,mu,mulmb,munu,mus,musgm,N10ord
     &,N5ord,N6ord,N7ord,Nordr,Nshell,nu,nulmb,nus,nusgm
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      integer Ubound,Ulpure,sigma
      logical open,complx
      dimension DA(*),DB(*),D1234(*)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/order/Nordr(20),N6ord(10),N5ord(9),N10ord(10),N7ord(7),Lbou
     &nd(4,3),Ubound(4),Ulpure(4)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      data half/0.5D0/,pt25/0.25D0/,zero/0.0D0/
      
      
      
      
      
      
      
      iscfp=ISCF+1
      open=ISCF.EQ.1
      complx=ISCF.EQ.2
      
      ist=Aos(ISHELL)-1
      jst=Aos(JSHELL)-1
      kst=Aos(KSHELL)-1
      lst=Aos(LSHELL)-1
      ijkl=0
      DNSMAX=zero
      do 100 i=ISTART,IEND
      if(IMJ.EQ.0)JEND=i
      if(IMKJML.EQ.0)KEND=i
      mu=ist+Nordr(i)
      mus=mu*(mu-1)/2
      
      do 50 j=JSTART,JEND
      nu=jst+Nordr(j)
      nus=nu*(nu-1)/2
      munu=mus+nu
      if(mu.LT.nu)munu=mu+nus
      cc2=half
      if(mu.NE.nu)cc2=cc2+cc2
      d12=DA(munu)
      if(open)d12b=DB(munu)
      
      do 20 k=KSTART,KEND
      LEND=Ubound(Ldmax)
      if(KML.EQ.0)LEND=k
      if(IMKJML.EQ.0.AND.i.EQ.k)LEND=j
      lambda=kst+Nordr(k)
      lmbdas=lambda*(lambda-1)/2
      mulmb=mus+lambda
      nulmb=nus+lambda
      if(mu.LT.lambda)mulmb=mu+lmbdas
      if(nu.LT.lambda)nulmb=nu+lmbdas
      d13=DA(mulmb)
      d13q=d13*pt25
      d23=DA(nulmb)
      d23q=d23*pt25
      if(open.OR.complx)then
      d23b=DB(nulmb)
      d13b=DB(mulmb)
      if(complx)then
      if(lambda.LT.nu)d23b=-d23b
      if(lambda.LT.mu)d13b=-d13b
      endif
      endif
      
      do 10 l=LSTART,LEND
      ijkl=ijkl+1
      sigma=lst+Nordr(l)
      sigmas=sigma*(sigma-1)/2
      cc4=cc2
      if(lambda.NE.sigma)cc4=cc4+cc4
      musgm=mus+sigma
      nusgm=nus+sigma
      lmbsgm=lmbdas+sigma
      if(mu.LT.sigma)musgm=mu+sigmas
      if(nu.LT.sigma)nusgm=nu+sigmas
      if(lambda.LT.sigma)lmbsgm=lambda+sigmas
      if(munu.NE.lmbsgm)cc4=cc4+cc4
      d14=DA(musgm)
      d24=DA(nusgm)
      d34=DA(lmbsgm)
      
      if(iscfp.EQ.2)then
      
      d14b=DB(musgm)
      d24b=DB(nusgm)
      d34b=DB(lmbsgm)
      dtemp=((d12+d12b)*(d34+d34b)-half*(d13*d24+d23*d14+d13b*d24b+d14b*
     &d23b))*cc4
      elseif(iscfp.EQ.3)then
      
      d14b=DB(musgm)
      d24b=DB(nusgm)
      if(mu.LT.sigma)d14b=-d14b
      if(nu.LT.sigma)d24b=-d24b
      dtemp=(d12*d34-pt25*(d13*d24+d23*d14-d13b*d24b-d23b*d14b))*cc4
      elseif(iscfp.EQ.4)then
      elseif(iscfp.NE.5)then
      
      dtemp=(d12*d34-(d13q*d24+d14*d23q))*cc4
      endif
      
      
      
      if(gabs(dtemp).GT.DNSMAX)DNSMAX=gabs(dtemp)
      D1234(ijkl)=dtemp
10    continue
20    continue
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
