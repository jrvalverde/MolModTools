
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ptprep"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ptprep.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "ptprep.web"
      subroutine ptprep(NP,NHI,LMALO,LMAHI,LMBLO,LMBHI,ALPHA,RKA,RKB,ARG
     &SUM)
      implicit none
      real*8 ALPHA,ARGSUM,ba,bb,bess,c,F,f10,f100,f1000,ft5,one,Pt,pta,p
     &tb,Ptpow,RKA,rkab,RKB,sqalp
      real*8 sqalpi,temp,two,w,wl,z
      integer i,i1,ihi,ilo,la,lb,LMAHI,LMALO,LMBHI,LMBLO,n,NHI,nhim1,NP,
     &Npts
      
      
      logical called
      common/ptwtdt/Ptpow(50,7),F(50,7,7),Pt(50),Npts
      dimension z(125),w(125),wl(125)
      dimension temp(50),ba(50,2),bb(50,2),pta(50),ptb(50)
      save f10,f100,f1000,ft5,z,w,one,two,wl,called
      data f10/1.0D01/,f100/1.0D02/,f1000/1.0D03/,ft5/1.0D05/,one/1.0D0/
     &,two/2.0D0/,called/.FALSE./
      data(z(i),i=1,15)/0.21686942698851D-01,0.11268419695264D+00,0.2704
     &9262038900D+00,0.48690228923427D+00,0.75304357423052D+00,0.1060930
     &8720058D+01,0.14042548093706D+01,0.17786462185204D+01,0.2181707962
     &8455D+01,0.26130606725136D+01,0.30746179394975D+01,0.3571407977467
     &3D+01,0.41137359184560D+01,0.47235128949706D+01,0.54604887738649D+
     &01/
      data(wl(i),i=1,15)/-0.28923934226892D+01,-0.20872502246784D+01,-0.
     &17413082798752D+01,-0.16529283986280D+01,-0.18111024880391D+01,-0.
     &22449044675152D+01,-0.29951916188624D+01,-0.41078567619256D+01,-0.
     &56342918250221D+01,-0.76357072966899D+01,-0.10191412182784D+02,-0.
     &13414263685260D+02,-0.17483525759750D+02,-0.22731556573814D+02,-0.
     &29978363911959D+02/
      data(z(i),i=16,40)/0.10300087285163D-01,0.53985341518803D-01,0.131
     &45451466029D+00,0.24094787552632D+00,0.38019082766422D+00,0.546640
     &41574787D+00,0.73769994842358D+00,0.95088776237837D+00,0.118395353
     &21852D+01,0.14349475698292D+01,0.17022546954938D+01,0.198460552566
     &13D+01,0.22810767024455D+01,0.25910895759857D+01,0.29144153490462D
     &+01,0.32511944348558D+01,0.36019794600646D+01,0.39678161994154D+01
     &,0.43503876059168D+01,0.47522701516031D+01,0.51774085039360D+01,0.
     &56320649416312D+01,0.61269698576629D+01,0.66832894012089D+01,0.735
     &69549248072D+01/
      data(wl(i),i=16,40)/-0.36344810809960D+01,-0.28028508807443D+01,-0
     &.23835918170425D+01,-0.21392799280915D+01,-0.20200270873718D+01,-0
     &.20181661664909D+01,-0.21411127653399D+01,-0.24026279639658D+01,-0
     &.28192105533838D+01,-0.34084929273225D+01,-0.41886129500119D+01,-0
     &.51781341056138D+01,-0.63963119271600D+01,-0.78636160911352D+01,-0
     &.96024897126096D+01,-0.11638389941733D+02,-0.14001227842670D+02,-0
     &.16727438637867D+02,-0.19863119631627D+02,-0.23469097464962D+02,-0
     &.27629759334103D+02,-0.32470010276268D+02,-0.38192471780396D+02,-0
     &.45177503750467D+02,-0.54374447721006D+02/
      data(z(i),i=41,90)/0.36994166894119D-02,0.19465578559300D-01,0.477
     &23257511078D-01,0.88299474142332D-01,0.14094424983151D+00,0.205343
     &82473297D+00,0.28113094473115D+00,0.36789570186710D+00,0.465196632
     &38257D+00,0.57257157583974D+00,0.68954786429340D+00,0.815651524129
     &03D+00,0.95041529376704D+00,0.10933853708522D+01,0.12441268939405D
     &+01,0.14022282325284D+01,0.15673042057959D+01,0.17389983773292D+01
     &,0.19169845842537D+01,0.21009678588666D+01,0.22906848929554D+01,0.
     &24859041828723D+01,0.26864259797693D+01,0.28920821561667D+01,0.310
     &27360886914D+01,0.33182826484263D+01,0.35386483857011D+01,0.376379
     &19961013D+01,0.39937051598745D+01,0.42284138590013D+01,0.446798029
     &67817D+01,0.47125055766333D+01,0.49621333441732D+01,0.521705466625
     &02D+01,0.54775145229933D+01,0.57438204412415D+01,0.60163540281218D
     &+01,0.62955865198298D+01,0.65821000263865D+01,0.68766170796566D+01
     &,0.71800426651669D+01,0.74935257051424D+01,0.78185521503957D+01,0.
     &81570921045017D+01,0.85118452647437D+01,0.88866800592441D+01,0.928
     &74967414165D+01,0.97241658658846D+01,0.10215886258578D+02,0.108129
     &86072945D+02/
      data(wl(i),i=41,90)/-0.46574383022338D+01,-0.38155859951346D+01,-0
     &.33704428205104D+01,-0.30727117604981D+01,-0.28572395301423D+01,-0
     &.26991646795109D+01,-0.25880627896317D+01,-0.25201353035082D+01,-0
     &.24951186304868D+01,-0.25148222836174D+01,-0.25823411511635D+01,-0
     &.27015876746387D+01,-0.28769941135486D+01,-0.31133146229898D+01,-0
     &.34154910850623D+01,-0.37885625744020D+01,-0.42376063028469D+01,-0
     &.47677021538748D+01,-0.53839153800117D+01,-0.60912935800822D+01,-0
     &.68948751268508D+01,-0.77997069948573D+01,-0.88108705560751D+01,-0
     &.99335144307359D+01,-0.11172893941886D+02,-0.12534417154165D+02,-0
     &.14023697905891D+02,-0.15646616696022D+02,-0.17409390796215D+02,-0
     &.19318655564202D+02,-0.21381559694059D+02,-0.23605878130490D+02,-0
     &.26000147713416D+02,-0.28573832479246D+02,-0.31337528194445D+02,-0
     &.34303219560723D+02,-0.37484609303812D+02,-0.40897547206294D+02,-0
     &.44560601095771D+02,-0.48495834503086D+02,-0.52729894016077D+02,-0
     &.57295576781520D+02,-0.62234173321411D+02,-0.67599125666685D+02,-0
     &.73462057911764D+02,-0.79923433718633D+02,-0.87133222490805D+02,-0
     &.95336594509397D+02,-0.10499775571391D+03,-0.11728925610295D+03/
      data(z(i),i=91,110)/-.53874808898628D+01,-.46036824578120D+01,-.39
     &447640525915D+01,-.33478545826881D+01,-.27888060754991D+01,-.22549
     &740197087D+01,-.17385377287517D+01,-.12340762292472D+01,-.73747373
     &780326D+00,-.24534071156534D+00,.24534071156534D+00,.7374737378032
     &7D+00,.12340762292472D+01,.17385377287517D+01,.22549740197087D+01,
     &.27888060754991D+01,.33478545826881D+01,.39447640525914D+01,.46036
     &824578119D+01,.53874808898629D+01/
      data(wl(i),i=91,110)/-0.29131876583529D+02,-0.21544396258299D+02,-
     &0.16035530658057D+02,-0.11761059230784D+02,-0.83846808826348D+01,-
     &0.57310180941632D+01,-0.36964875406417D+01,-0.22162495825681D+01,-
     &0.12494043489311D+01,-0.77166309205373D+00,-0.77166309205373D+00,-
     &0.12494043489312D+01,-0.22162495825681D+01,-0.36964875406416D+01,-
     &0.57310180941628D+01,-0.83846808826341D+01,-0.11761059230783D+02,-
     &0.16035530658057D+02,-0.21544396258298D+02,-0.29131876583528D+02/
      data(z(i),i=111,120)/-.34361591188396D+01,-.25327316742343D+01,-.1
     &7566836493012D+01,-.10366108297905D+01,-.34290132722408D+00,.34290
     &132722407D+00,.10366108297905D+01,.17566836493012D+01,.25327316742
     &342D+01,.34361591188395D+01/
      data(wl(i),i=111,120)/-0.11782056299959D+02,-0.66123686528960D+01,
     &-0.33850958757992D+01,-0.14265389761808D+01,-0.49288316712311D+00,
     &-0.49288316712309D+00,-0.14265389761808D+01,-0.33850958757990D+01,
     &-0.66123686528957D+01,-0.11782056299958D+02/
      data(z(i),i=121,125)/-.20201828704560D+01,-.95857246461381D+00,-.8
     &6713183502837D-16,.95857246461379D+00,.20201828704561D+01/
      data(wl(i),i=121,125)/-0.39143636396237D+01,-0.93237102163451D+00,
     &-0.56243716497676D-01,-0.93237102163446D+00,-0.39143636396238D+01/
      
      if(.NOT.called)then
      called=.TRUE.
      do 50 i=1,125
      w(i)=exp(wl(i))
50    continue
      endif
      if(ARGSUM.LE.one)then
      ilo=1
      ihi=15
      Npts=15
      elseif(ARGSUM.LE.f10)then
      ilo=16
      ihi=40
      Npts=25
      elseif(ARGSUM.LE.f100)then
      ilo=41
      ihi=90
      Npts=50
      elseif(ARGSUM.LE.f1000)then
      ilo=91
      ihi=110
      Npts=20
      elseif(ARGSUM.LE.ft5)then
      ilo=111
      ihi=120
      Npts=10
      else
      ilo=121
      ihi=125
      Npts=5
      endif
      
      sqalp=sqrt(ALPHA)
      sqalpi=one/sqalp
      nhim1=NHI-1
      rkab=RKA+RKB
      
      if(ARGSUM.GT.f100)then
      c=rkab/(two*ALPHA)
      i1=0
      do 100 i=ilo,ihi
      i1=i1+1
      Pt(i1)=c+z(i)*sqalpi
      temp(i1)=w(i)
      do 60 la=LMALO,LMAHI
      ba(i1,la-LMALO+1)=bess(RKA*Pt(i1),la-1)
60    continue
      do 80 lb=LMBLO,LMBHI
      bb(i1,lb-LMBLO+1)=bess(RKB*Pt(i1),lb-1)
80    continue
100   continue
      else
      
      i1=0
      do 150 i=ilo,ihi
      i1=i1+1
      Pt(i1)=z(i)*sqalpi
      temp(i1)=exp(rkab*Pt(i1)+wl(i))
      pta(i1)=Pt(i1)*RKA
      ptb(i1)=Pt(i1)*RKB
150   continue
      do 200 la=LMALO,LMAHI
      call bessv(ba(1,la-LMALO+1),pta,la-1,Npts)
200   continue
      do 250 lb=LMBLO,LMBHI
      call bessv(bb(1,lb-LMBLO+1),ptb,lb-1,Npts)
250   continue
      endif
      
      do 400 la=LMALO,LMAHI
      do 300 lb=LMBLO,LMBHI
      do 260 i=1,Npts
      F(i,la,lb)=ba(i,la-LMALO+1)*bb(i,lb-LMBLO+1)*temp(i)
260   continue
300   continue
400   continue
      
      do 500 i=1,Npts
      Ptpow(i,1)=Pt(i)**(NP)
500   continue
      if(NHI.NE.1)then
      do 550 n=1,nhim1
      do 520 i=1,Npts
      Ptpow(i,n+1)=Ptpow(i,n)*Pt(i)
520   continue
550   continue
      endif
      
      
      
      return
      end
C* :1 * 
      