
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dn21g"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dn21g.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 5 "dn21g.web"
      SUBROUTINE DN21G(E,CS,CP,IA,NGAUSS,IERR1)
      DOUBLE PRECISION E,CS,CP
      DIMENSION E(*),CS(*),CP(*)
      
      GO TO(10,20,30,100,170,240,310,380,450,520,590,660,730,800,870,940
     &,1010,1080),IA
      
      
10    CONTINUE
      E(1)=0.450180D+01
      E(2)=0.681444D+00
      E(3)=0.151398D+00
      CS(1)=0.156285D+00
      CS(2)=0.904691D+00
      CS(3)=0.100000D+01
      GO TO 1150
      
      
20    CONTINUE
      E(1)=0.136267D+02
      E(2)=0.199935D+01
      E(3)=0.382993D+00
      CS(1)=0.175230D+00
      CS(2)=0.893483D+00
      CS(3)=0.100000D+01
      GO TO 1150
      
      
30    GO TO(40,50,70,80,60,90),NGAUSS
40    CONTINUE
50    CONTINUE
60    CONTINUE
      CALL BERROR(IERR1)
70    CONTINUE
      E(1)=0.368382D+02
      E(2)=0.548172D+01
      E(3)=0.111327D+01
      E(4)=0.540205D+00
      E(5)=0.102255D+00
      E(6)=0.285645D-01
      CS(1)=0.696686D-01
      CS(2)=0.381346D+00
      CS(3)=0.681702D+00
      CS(4)=-0.263127D+00
      CS(5)=0.114339D+01
      CS(6)=0.100000D+01
      CP(1)=0.161546D+00
      CP(2)=0.915663D+00
      CP(3)=0.100000D+01
      GO TO 1150
80    CONTINUE
      E(1)=0.109353D+03
      E(2)=0.164228D+02
      E(3)=0.359415D+01
      E(4)=0.905297D+00
      E(5)=0.540205D+00
      E(6)=0.102255D+00
      E(7)=0.285645D-01
      CS(1)=0.190277D-01
      CS(2)=0.130276D+00
      CS(3)=0.439082D+00
      CS(4)=0.557314D+00
      CS(5)=-0.263127D+00
      CS(6)=0.114339D+01
      CS(7)=0.100000D+01
      CP(1)=0.161546D+00
      CP(2)=0.915663D+00
      CP(3)=0.100000D+01
      GO TO 1150
90    CONTINUE
      E(1)=0.642418D+03
      E(2)=0.965164D+02
      E(3)=0.220174D+02
      E(4)=0.617645D+01
      E(5)=0.193511D+01
      E(6)=0.639577D+00
      E(7)=0.540205D+00
      E(8)=0.102255D+00
      E(9)=0.285645D-01
      CS(1)=0.215096D-02
      CS(2)=0.162677D-01
      CS(3)=0.776383D-01
      CS(4)=0.246495D+00
      CS(5)=0.467506D+00
      CS(6)=0.346915D+00
      CS(7)=-0.263127D+00
      CS(8)=0.114339D+01
      CS(9)=0.100000D+01
      CP(1)=0.161546D+00
      CP(2)=0.915663D+00
      CP(3)=0.100000D+01
      GO TO 1150
      
      
100   GO TO(110,120,140,150,130,160),NGAUSS
110   CONTINUE
120   CONTINUE
130   CONTINUE
      CALL BERROR(IERR1)
140   CONTINUE
      E(1)=0.718876D+02
      E(2)=0.107289D+02
      E(3)=0.222205D+01
      E(4)=0.129548D+01
      E(5)=0.268881D+00
      E(6)=0.773501D-01
      CS(1)=0.644263D-01
      CS(2)=0.366096D+00
      CS(3)=0.695934D+00
      CS(4)=-0.421064D+00
      CS(5)=0.122407D+01
      CS(6)=0.100000D+01
      CP(1)=0.205132D+00
      CP(2)=0.882528D+00
      CP(3)=0.100000D+01
      GO TO 1150
150   CONTINUE
      E(1)=0.207980D+03
      E(2)=0.314316D+02
      E(3)=0.699419D+01
      E(4)=0.181295D+01
      E(5)=0.129548D+01
      E(6)=0.268881D+00
      E(7)=0.773501D-01
      CS(1)=0.180520D-01
      CS(2)=0.123804D+00
      CS(3)=0.427580D+00
      CS(4)=0.569901D+00
      CS(5)=-0.421064D+00
      CS(6)=0.122407D+01
      CS(7)=0.100000D+01
      CP(1)=0.205132D+00
      CP(2)=0.882528D+00
      CP(3)=0.100000D+01
      GO TO 1150
160   CONTINUE
      E(1)=0.126450D+04
      E(2)=0.189930D+03
      E(3)=0.431275D+02
      E(4)=0.120889D+02
      E(5)=0.380790D+01
      E(6)=0.128266D+01
      E(7)=0.129548D+01
      E(8)=0.268881D+00
      E(9)=0.773501D-01
      CS(1)=0.194336D-02
      CS(2)=0.148251D-01
      CS(3)=0.720662D-01
      CS(4)=0.237022D+00
      CS(5)=0.468789D+00
      CS(6)=0.356382D+00
      CS(7)=-0.421064D+00
      CS(8)=0.122407D+01
      CS(9)=0.100000D+01
      CP(1)=0.205132D+00
      CP(2)=0.882528D+00
      CP(3)=0.100000D+01
      GO TO 1150
      
      
170   GO TO(180,190,210,220,200,230),NGAUSS
180   CONTINUE
190   CONTINUE
200   CONTINUE
      CALL BERROR(IERR1)
210   CONTINUE
      E(1)=0.116434D+03
      E(2)=0.174314D+02
      E(3)=0.368016D+01
      E(4)=0.228187D+01
      E(5)=0.465248D+00
      E(6)=0.124328D+00
      CS(1)=0.629605D-01
      CS(2)=0.363304D+00
      CS(3)=0.697255D+00
      CS(4)=-0.368662D+00
      CS(5)=0.119944D+01
      CS(6)=0.100000D+01
      CP(1)=0.231152D+00
      CP(2)=0.866764D+00
      CP(3)=0.100000D+01
      GO TO 1150
220   CONTINUE
      E(1)=0.345445D+03
      E(2)=0.519156D+02
      E(3)=0.115337D+02
      E(4)=0.301981D+01
      E(5)=0.228187D+01
      E(6)=0.465248D+00
      E(7)=0.124328D+00
      CS(1)=0.170348D-01
      CS(2)=0.119266D+00
      CS(3)=0.424702D+00
      CS(4)=0.575037D+00
      CS(5)=-0.368662D+00
      CS(6)=0.119944D+01
      CS(7)=0.100000D+01
      CP(1)=0.231152D+00
      CP(2)=0.866764D+00
      CP(3)=0.100000D+01
      GO TO 1150
230   CONTINUE
      E(1)=0.208212D+04
      E(2)=0.312310D+03
      E(3)=0.708874D+02
      E(4)=0.198525D+02
      E(5)=0.629161D+01
      E(6)=0.212862D+01
      E(7)=0.228187D+01
      E(8)=0.465248D+00
      E(9)=0.124328D+00
      CS(1)=0.184986D-02
      CS(2)=0.141277D-01
      CS(3)=0.692697D-01
      CS(4)=0.232393D+00
      CS(5)=0.470154D+00
      CS(6)=0.360288D+00
      CS(7)=-0.368662D+00
      CS(8)=0.119944D+01
      CS(9)=0.100000D+01
      CP(1)=0.231152D+00
      CP(2)=0.866764D+00
      CP(3)=0.100000D+01
      GO TO 1150
      
      
240   GO TO(250,260,280,290,270,300),NGAUSS
250   CONTINUE
260   CONTINUE
270   CONTINUE
      CALL BERROR(IERR1)
280   CONTINUE
      E(1)=0.172256D+03
      E(2)=0.259109D+02
      E(3)=0.553335D+01
      E(4)=0.366498D+01
      E(5)=0.770545D+00
      E(6)=0.195857D+00
      CS(1)=0.617669D-01
      CS(2)=0.358794D+00
      CS(3)=0.700713D+00
      CS(4)=-0.395897D+00
      CS(5)=0.121584D+01
      CS(6)=0.100000D+01
      CP(1)=0.236460D+00
      CP(2)=0.860619D+00
      CP(3)=0.100000D+01
      GO TO 1150
290   CONTINUE
      E(1)=0.514836D+03
      E(2)=0.773470D+02
      E(3)=0.172534D+02
      E(4)=0.455754D+01
      E(5)=0.366498D+01
      E(6)=0.770545D+00
      E(7)=0.195857D+00
      CS(1)=0.165399D-01
      CS(2)=0.116447D+00
      CS(3)=0.419945D+00
      CS(4)=0.580709D+00
      CS(5)=-0.395897D+00
      CS(6)=0.121584D+01
      CS(7)=0.100000D+01
      CP(1)=0.236460D+00
      CP(2)=0.860619D+00
      CP(3)=0.100000D+01
      GO TO 1150
300   CONTINUE
      E(1)=0.304752D+04
      E(2)=0.456424D+03
      E(3)=0.103653D+03
      E(4)=0.292258D+02
      E(5)=0.934863D+01
      E(6)=0.318904D+01
      E(7)=0.366498D+01
      E(8)=0.770545D+00
      E(9)=0.195857D+00
      CS(1)=0.182588D-02
      CS(2)=0.140566D-01
      CS(3)=0.687570D-01
      CS(4)=0.230422D+00
      CS(5)=0.468463D+00
      CS(6)=0.362780D+00
      CS(7)=-0.395897D+00
      CS(8)=0.121584D+01
      CS(9)=0.100000D+01
      CP(1)=0.236460D+00
      CP(2)=0.860619D+00
      CP(3)=0.100000D+01
      GO TO 1150
      
      
310   GO TO(320,330,350,360,340,370),NGAUSS
320   CONTINUE
330   CONTINUE
340   CONTINUE
      CALL BERROR(IERR1)
350   CONTINUE
      E(1)=0.242766D+03
      E(2)=0.364851D+02
      E(3)=0.781449D+01
      E(4)=0.542522D+01
      E(5)=0.114915D+01
      E(6)=0.283205D+00
      CS(1)=0.598657D-01
      CS(2)=0.352955D+00
      CS(3)=0.706513D+00
      CS(4)=-0.413301D+00
      CS(5)=0.122442D+01
      CS(6)=0.100000D+01
      CP(1)=0.237972D+00
      CP(2)=0.858953D+00
      CP(3)=0.100000D+01
      GO TO 1150
360   CONTINUE
      E(1)=0.715345D+03
      E(2)=0.107490D+03
      E(3)=0.240414D+02
      E(4)=0.639437D+01
      E(5)=0.542522D+01
      E(6)=0.114915D+01
      E(7)=0.283205D+00
      CS(1)=0.162587D-01
      CS(2)=0.114910D+00
      CS(3)=0.417387D+00
      CS(4)=0.583539D+00
      CS(5)=-0.413301D+00
      CS(6)=0.122442D+01
      CS(7)=0.100000D+01
      CP(1)=0.237972D+00
      CP(2)=0.858953D+00
      CP(3)=0.100000D+01
      GO TO 1150
370   CONTINUE
      E(1)=0.415011D+04
      E(2)=0.620084D+03
      E(3)=0.141688D+03
      E(4)=0.403367D+02
      E(5)=0.130267D+02
      E(6)=0.447003D+01
      E(7)=0.542522D+01
      E(8)=0.114915D+01
      E(9)=0.283205D+00
      CS(1)=0.184541D-02
      CS(2)=0.141645D-01
      CS(3)=0.686325D-01
      CS(4)=0.228574D+00
      CS(5)=0.466162D+00
      CS(6)=0.365672D+00
      CS(7)=-0.413301D+00
      CS(8)=0.122442D+01
      CS(9)=0.100000D+01
      CP(1)=0.237972D+00
      CP(2)=0.858953D+00
      CP(3)=0.100000D+01
      GO TO 1150
      
      
380   GO TO(390,400,420,430,410,440),NGAUSS
390   CONTINUE
400   CONTINUE
410   CONTINUE
      CALL BERROR(IERR1)
420   CONTINUE
      E(1)=0.322037D+03
      E(2)=0.484308D+02
      E(3)=0.104206D+02
      E(4)=0.740294D+01
      E(5)=0.157620D+01
      E(6)=0.373684D+00
      CS(1)=0.592394D-01
      CS(2)=0.351500D+00
      CS(3)=0.707658D+00
      CS(4)=-0.404453D+00
      CS(5)=0.122156D+01
      CS(6)=0.100000D+01
      CP(1)=0.244586D+00
      CP(2)=0.853955D+00
      CP(3)=0.100000D+01
      GO TO 1150
430   CONTINUE
      E(1)=0.938318D+03
      E(2)=0.141662D+03
      E(3)=0.318308D+02
      E(4)=0.851101D+01
      E(5)=0.740294D+01
      E(6)=0.157620D+01
      E(7)=0.373684D+00
      CS(1)=0.162714D-01
      CS(2)=0.114340D+00
      CS(3)=0.416787D+00
      CS(4)=0.583808D+00
      CS(5)=-0.404453D+00
      CS(6)=0.122156D+01
      CS(7)=0.100000D+01
      CP(1)=0.244586D+00
      CP(2)=0.853955D+00
      CP(3)=0.100000D+01
      GO TO 1150
440   CONTINUE
      E(1)=0.547227D+04
      E(2)=0.817806D+03
      E(3)=0.186446D+03
      E(4)=0.530230D+02
      E(5)=0.171800D+02
      E(6)=0.591196D+01
      E(7)=0.740294D+01
      E(8)=0.157620D+01
      E(9)=0.373684D+00
      CS(1)=0.183217D-02
      CS(2)=0.141047D-01
      CS(3)=0.686262D-01
      CS(4)=0.229376D+00
      CS(5)=0.466399D+00
      CS(6)=0.364173D+00
      CS(7)=-0.404453D+00
      CS(8)=0.122156D+01
      CS(9)=0.100000D+01
      CP(1)=0.244586D+00
      CP(2)=0.853955D+00
      CP(3)=0.100000D+01
      GO TO 1150
      
      
450   GO TO(460,470,490,500,480,510),NGAUSS
460   CONTINUE
470   CONTINUE
480   CONTINUE
      CALL BERROR(IERR1)
490   CONTINUE
      E(1)=0.413801D+03
      E(2)=0.622446D+02
      E(3)=0.134340D+02
      E(4)=0.977759D+01
      E(5)=0.208617D+01
      E(6)=0.482383D+00
      CS(1)=0.585483D-01
      CS(2)=0.349308D+00
      CS(3)=0.709632D+00
      CS(4)=-0.407327D+00
      CS(5)=0.122314D+01
      CS(6)=0.100000D+01
      CP(1)=0.246680D+00
      CP(2)=0.852321D+00
      CP(3)=0.100000D+01
      GO TO 1150
500   CONTINUE
      E(1)=0.120491D+04
      E(2)=0.181792D+03
      E(3)=0.408879D+02
      E(4)=0.109630D+02
      E(5)=0.977759D+01
      E(6)=0.208617D+01
      E(7)=0.482383D+00
      CS(1)=0.160678D-01
      CS(2)=0.113306D+00
      CS(3)=0.415239D+00
      CS(4)=0.585754D+00
      CS(5)=-0.407327D+00
      CS(6)=0.122314D+01
      CS(7)=0.100000D+01
      CP(1)=0.246680D+00
      CP(2)=0.852321D+00
      CP(3)=0.100000D+01
      GO TO 1150
510   CONTINUE
      E(1)=0.678319D+04
      E(2)=0.104244D+04
      E(3)=0.242398D+03
      E(4)=0.696320D+02
      E(5)=0.226894D+02
      E(6)=0.779636D+01
      E(7)=0.977759D+01
      E(8)=0.208617D+01
      E(9)=0.482383D+00
      CS(1)=0.188463D-02
      CS(2)=0.138121D-01
      CS(3)=0.662493D-01
      CS(4)=0.221875D+00
      CS(5)=0.460842D+00
      CS(6)=0.378453D+00
      CS(7)=-0.407327D+00
      CS(8)=0.122314D+01
      CS(9)=0.100000D+01
      CP(1)=0.246680D+00
      CP(2)=0.852321D+00
      CP(3)=0.100000D+01
      GO TO 1150
      
      
520   GO TO(530,540,560,570,550,580),NGAUSS
530   CONTINUE
540   CONTINUE
550   CONTINUE
      CALL BERROR(IERR1)
560   CONTINUE
      E(1)=0.515724D+03
      E(2)=0.776538D+02
      E(3)=0.168136D+02
      E(4)=0.124830D+02
      E(5)=0.266451D+01
      E(6)=0.606250D+00
      CS(1)=0.581430D-01
      CS(2)=0.347951D+00
      CS(3)=0.710714D+00
      CS(4)=-0.409922D+00
      CS(5)=0.122431D+01
      CS(6)=0.100000D+01
      CP(1)=0.247460D+00
      CP(2)=0.851743D+00
      CP(3)=0.100000D+01
      GO TO 1150
570   CONTINUE
      E(1)=0.151275D+04
      E(2)=0.227368D+03
      E(3)=0.510739D+02
      E(4)=0.137213D+02
      E(5)=0.124830D+02
      E(6)=0.266451D+01
      E(7)=0.606250D+00
      CS(1)=0.158096D-01
      CS(2)=0.112430D+00
      CS(3)=0.414266D+00
      CS(4)=0.587193D+00
      CS(5)=-0.409922D+00
      CS(6)=0.122431D+01
      CS(7)=0.100000D+01
      CP(1)=0.247460D+00
      CP(2)=0.851743D+00
      CP(3)=0.100000D+01
      GO TO 1150
580   CONTINUE
      E(1)=0.878583D+04
      E(2)=0.132390D+04
      E(3)=0.300795D+03
      E(4)=0.851891D+02
      E(5)=0.276534D+02
      E(6)=0.953039D+01
      E(7)=0.124830D+02
      E(8)=0.266451D+01
      E(9)=0.606250D+00
      CS(1)=0.178077D-02
      CS(2)=0.135790D-01
      CS(3)=0.670847D-01
      CS(4)=0.226825D+00
      CS(5)=0.465053D+00
      CS(6)=0.368995D+00
      CS(7)=-0.409922D+00
      CS(8)=0.122431D+01
      CS(9)=0.100000D+01
      CP(1)=0.247460D+00
      CP(2)=0.851743D+00
      CP(3)=0.100000D+01
      
      
590   GO TO(600,610,640,620,630,650),NGAUSS
600   CONTINUE
610   CONTINUE
620   CONTINUE
630   CONTINUE
      CALL BERROR(IERR1)
640   E(1)=0.547613D+03
      E(2)=0.820678D+02
      E(3)=0.176917D+02
      E(4)=0.175407D+02
      E(5)=0.379398D+01
      E(6)=0.906441D+00
      E(7)=0.501824D+00
      E(8)=0.609458D-01
      E(9)=0.244349D-01
      CS(1)=0.674911D-01
      CS(2)=0.393505D+00
      CS(3)=0.665605D+00
      CS(4)=-0.111937D+00
      CS(5)=0.254654D+00
      CS(6)=0.844417D+00
      CS(7)=-0.219660D+00
      CS(8)=0.108912D+01
      CS(9)=0.100000D+01
      CP(1)=0.128233D+00
      CP(2)=0.471533D+00
      CP(3)=0.604273D+00
      CP(4)=0.906649D-02
      CP(5)=0.997202D+00
      CP(6)=0.100000D+01
      GO TO 1150
650   CONTINUE
      E(1)=0.999320D+04
      E(2)=0.149989D+04
      E(3)=0.341951D+03
      E(4)=0.946796D+02
      E(5)=0.297345D+02
      E(6)=0.100063D+02
      E(7)=0.150963D+03
      E(8)=0.355878D+02
      E(9)=0.111683D+02
      E(10)=0.390201D+01
      E(11)=0.138177D+01
      E(12)=0.466382D+00
      E(13)=0.501824D+00
      E(14)=0.609458D-01
      E(15)=0.244349D-01
      CS(1)=0.193766D-02
      CS(2)=0.148070D-01
      CS(3)=0.727055D-01
      CS(4)=0.252629D+00
      CS(5)=0.493242D+00
      CS(6)=0.313169D+00
      CS(7)=-0.354208D-02
      CS(8)=-0.439588D-01
      CS(9)=-0.109752D+00
      CS(10)=0.187398D+00
      CS(11)=0.646699D+00
      CS(12)=0.306058D+00
      CS(13)=-0.219660D+00
      CS(14)=0.108912D+01
      CS(15)=0.100000D+01
      CP(1)=0.500166D-02
      CP(2)=0.355109D-01
      CP(3)=0.142825D+00
      CP(4)=0.338620D+00
      CP(5)=0.451579D+00
      CP(6)=0.273271D+00
      CP(7)=0.906649D-02
      CP(8)=0.997202D+00
      CP(9)=0.100000D+01
      GO TO 1150
      
      
660   GO TO(670,680,710,690,700,720),NGAUSS
670   CONTINUE
680   CONTINUE
690   CONTINUE
700   CONTINUE
      CALL BERROR(IERR1)
710   CONTINUE
      E(1)=0.652841D+03
      E(2)=0.983805D+02
      E(3)=0.212996D+02
      E(4)=0.233727D+02
      E(5)=0.519953D+01
      E(6)=0.131508D+01
      E(7)=0.611349D+00
      E(8)=0.141841D+00
      E(9)=0.464011D-01
      CS(1)=0.675982D-01
      CS(2)=0.391778D+00
      CS(3)=0.666661D+00
      CS(4)=-0.110246D+00
      CS(5)=0.184119D+00
      CS(6)=0.896399D+00
      CP(1)=0.121014D+00
      CP(2)=0.462810D+00
      CP(3)=0.606907D+00
      CS(7)=-0.361101D+00
      CS(8)=0.121505D+01
      CS(9)=0.100000D+01
      CP(4)=0.242633D-01
      CP(5)=0.986673D+00
      CP(6)=0.100000D+01
      GO TO 1150
720   E(1)=0.117228D+05
      E(2)=0.175993D+04
      E(3)=0.400846D+03
      E(4)=0.112807D+03
      E(5)=0.359997D+02
      E(6)=0.121828D+02
      E(7)=0.189180D+03
      E(8)=0.452119D+02
      E(9)=0.143563D+02
      E(10)=0.513886D+01
      E(11)=0.190652D+01
      E(12)=0.705887D+00
      E(13)=0.611349D+00
      E(14)=0.141841D+00
      E(15)=0.464011D-01
      CS(1)=0.197783D-02
      CS(2)=0.151140D-01
      CS(3)=0.739108D-01
      CS(4)=0.249191D+00
      CS(5)=0.487928D+00
      CS(6)=0.319662D+00
      CS(7)=-0.323717D-02
      CS(8)=-0.410079D-01
      CS(9)=-0.112600D+00
      CS(10)=0.148633D+00
      CS(11)=0.616497D+00
      CS(12)=0.364829D+00
      CS(13)=-0.361101D+00
      CS(14)=0.121505D+01
      CS(15)=0.100000D+01
      CP(1)=0.492813D-02
      CP(2)=0.349888D-01
      CP(3)=0.140725D+00
      CP(4)=0.333642D+00
      CP(5)=0.444940D+00
      CP(6)=0.269254D+00
      CP(7)=0.242633D-01
      CP(8)=0.986673D+00
      CP(9)=0.100000D+01
      GO TO 1150
      
      
730   GO TO(740,750,780,760,770,790),NGAUSS
740   CONTINUE
750   CONTINUE
760   CONTINUE
770   CONTINUE
      CALL BERROR(IERR1)
780   E(1)=0.775737D+03
      E(2)=0.116952D+03
      E(3)=0.253326D+02
      E(4)=0.294796D+02
      E(5)=0.663314D+01
      E(6)=0.172675D+01
      E(7)=0.946160D+00
      E(8)=0.202506D+00
      E(9)=0.639088D-01
      CS(1)=0.668347D-01
      CS(2)=0.389061D+00
      CS(3)=0.669468D+00
      CS(4)=-0.107902D+00
      CS(5)=0.146245D+00
      CS(6)=0.923730D+00
      CP(1)=0.117574D+00
      CP(2)=0.461174D+00
      CP(3)=0.605535D+00
      CS(7)=-0.320327D+00
      CS(8)=0.118412D+01
      CS(9)=0.100000D+01
      CP(4)=0.519383D-01
      CP(5)=0.972660D+00
      CP(6)=0.100000D+01
      GO TO 1150
790   CONTINUE
      E(1)=0.139831D+05
      E(2)=0.209875D+04
      E(3)=0.477705D+03
      E(4)=0.134360D+03
      E(5)=0.428709D+02
      E(6)=0.145189D+02
      E(7)=0.239668D+03
      E(8)=0.574419D+02
      E(9)=0.182859D+02
      E(10)=0.659914D+01
      E(11)=0.249049D+01
      E(12)=0.944545D+00
      E(13)=0.946160D+00
      E(14)=0.202506D+00
      E(15)=0.639088D-01
      CS(1)=0.194267D-02
      CS(2)=0.148599D-01
      CS(3)=0.728494D-01
      CS(4)=0.246830D+00
      CS(5)=0.487258D+00
      CS(6)=0.323496D+00
      CS(7)=-0.292619D-02
      CS(8)=-0.374083D-01
      CS(9)=-0.114487D+00
      CS(10)=0.115635D+00
      CS(11)=0.612595D+00
      CS(12)=0.393799D+00
      CS(13)=-0.320327D+00
      CS(14)=0.118412D+01
      CS(15)=0.100000D+01
      CP(1)=0.460285D-02
      CP(2)=0.331990D-01
      CP(3)=0.136282D+00
      CP(4)=0.330476D+00
      CP(5)=0.449146D+00
      CP(6)=0.265704D+00
      CP(7)=0.519383D-01
      CP(8)=0.972660D+00
      CP(9)=0.100000D+01
      GO TO 1150
      
      
800   GO TO(810,820,850,830,840,860),NGAUSS
810   CONTINUE
820   CONTINUE
830   CONTINUE
840   CONTINUE
      CALL BERROR(IERR1)
850   E(1)=0.910655D+03
      E(2)=0.137336D+03
      E(3)=0.297601D+02
      E(4)=0.366716D+02
      E(5)=0.831729D+01
      E(6)=0.221645D+01
      E(7)=0.107913D+01
      E(8)=0.302422D+00
      E(9)=0.933392D-01
      CS(1)=0.660823D-01
      CS(2)=0.386229D+00
      CS(3)=0.672380D+00
      CS(4)=-0.104511D+00
      CS(5)=0.107410D+00
      CS(6)=0.951446D+00
      CP(1)=0.113355D+00
      CP(2)=0.457578D+00
      CP(3)=0.607427D+00
      CS(7)=-0.376108D+00
      CS(8)=0.125165D+01
      CS(9)=0.100000D+01
      CP(4)=0.671030D-01
      CP(5)=0.956883D+00
      CP(6)=0.100000D+01
      GO TO 1150
860   CONTINUE
      E(1)=0.161159D+05
      E(2)=0.242558D+04
      E(3)=0.553867D+03
      E(4)=0.156340D+03
      E(5)=0.500683D+02
      E(6)=0.170178D+02
      E(7)=0.292718D+03
      E(8)=0.698731D+02
      E(9)=0.223363D+02
      E(10)=0.815039D+01
      E(11)=0.313458D+01
      E(12)=0.122543D+01
      E(13)=0.107913D+01
      E(14)=0.302422D+00
      E(15)=0.933392D-01
      CS(1)=0.195948D-02
      CS(2)=0.149288D-01
      CS(3)=0.728478D-01
      CS(4)=0.246130D+00
      CS(5)=0.485914D+00
      CS(6)=0.325002D+00
      CS(7)=-0.278094D-02
      CS(8)=-0.357146D-01
      CS(9)=-0.114985D+00
      CS(10)=0.935634D-01
      CS(11)=0.603017D+00
      CS(12)=0.418959D+00
      CS(13)=-0.376108D+00
      CS(14)=0.125165D+01
      CS(15)=0.100000D+01
      CP(1)=0.443826D-02
      CP(2)=0.326679D-01
      CP(3)=0.134721D+00
      CP(4)=0.328678D+00
      CP(5)=0.449640D+00
      CP(6)=0.261372D+00
      CP(7)=0.671030D-01
      CP(8)=0.956883D+00
      CP(9)=0.100000D+01
      GO TO 1150
      
      
870   GO TO(880,890,920,900,910,930),NGAUSS
880   CONTINUE
890   CONTINUE
900   CONTINUE
910   CONTINUE
      CALL BERROR(IERR1)
920   CONTINUE
      E(1)=0.105490D+04
      E(2)=0.159195D+03
      E(3)=0.345304D+02
      E(4)=0.442866D+02
      E(5)=0.101019D+02
      E(6)=0.273997D+01
      E(7)=0.121865D+01
      E(8)=0.395546D+00
      E(9)=0.122811D+00
      CS(1)=0.655407D-01
      CS(2)=0.384036D+00
      CS(3)=0.674541D+00
      CS(4)=-0.102130D+00
      CS(5)=0.815922D-01
      CS(6)=0.969788D+00
      CS(7)=-0.371495D+00
      CS(8)=0.127099D+01
      CS(9)=0.100000D+01
      CP(1)=0.110851D+00
      CP(2)=0.456495D+00
      CP(3)=0.606936D+00
      CP(4)=0.915823D-01
      CP(5)=0.934924D+00
      CP(6)=0.100000D+01
      GO TO 1150
930   CONTINUE
      E(1)=0.194133D+05
      E(2)=0.290942D+04
      E(3)=0.661364D+03
      E(4)=0.185759D+03
      E(5)=0.591943D+02
      E(6)=0.200310D+02
      E(7)=0.339478D+03
      E(8)=0.810101D+02
      E(9)=0.258780D+02
      E(10)=0.945221D+01
      E(11)=0.366566D+01
      E(12)=0.146746D+01
      E(13)=0.121865D+01
      E(14)=0.395546D+00
      E(15)=0.122811D+00
      CS(1)=0.185160D-02
      CS(2)=0.142062D-01
      CS(3)=0.699995D-01
      CS(4)=0.240079D+00
      CS(5)=0.484762D+00
      CS(6)=0.335200D+00
      CS(7)=-0.278217D-02
      CS(8)=-0.360499D-01
      CS(9)=-0.116631D+00
      CS(10)=0.968328D-01
      CS(11)=0.614418D+00
      CS(12)=0.403798D+00
      CS(13)=-0.371495D+00
      CS(14)=0.127099D+01
      CS(15)=0.100000D+01
      CP(1)=0.456462D-02
      CP(2)=0.336936D-01
      CP(3)=0.139755D+00
      CP(4)=0.339362D+00
      CP(5)=0.450921D+00
      CP(6)=0.238586D+00
      CP(7)=0.915823D-01
      CP(8)=0.934924D+00
      CP(9)=0.100000D+01
      GO TO 1150
      
      
940   GO TO(950,960,990,970,980,1000),NGAUSS
950   CONTINUE
960   CONTINUE
970   CONTINUE
980   CONTINUE
      CALL BERROR(IERR1)
990   CONTINUE
      E(1)=0.121062D+04
      E(2)=0.182747D+03
      E(3)=0.396673D+02
      E(4)=0.522236D+02
      E(5)=0.119629D+02
      E(6)=0.328911D+01
      E(7)=0.122384D+01
      E(8)=0.457303D+00
      E(9)=0.142269D+00
      CS(1)=0.650071D-01
      CS(2)=0.382040D+00
      CS(3)=0.676545D+00
      CS(4)=-0.100310D+00
      CS(5)=0.650877D-01
      CS(6)=0.981455D+00
      CS(7)=-0.286089D+00
      CS(8)=0.122806D+01
      CS(9)=0.100000D+00
      CP(1)=0.109646D+00
      CP(2)=0.457649D+00
      CP(3)=0.604261D+00
      CP(4)=0.164777D+00
      CP(5)=0.870855D+00
      CP(6)=0.100000D+00
      GO TO 1150
1000  CONTINUE
      E(1)=0.219171D+05
      E(2)=0.330149D+04
      E(3)=0.754146D+03
      E(4)=0.212711D+03
      E(5)=0.679896D+02
      E(6)=0.230515D+02
      E(7)=0.423735D+03
      E(8)=0.100710D+03
      E(9)=0.321599D+02
      E(10)=0.118079D+02
      E(11)=0.463110D+01
      E(12)=0.187025D+01
      E(13)=0.122384D+01
      E(14)=0.457303D+00
      E(15)=0.142269D+00
      CS(1)=0.186924D-02
      CS(2)=0.142303D-01
      CS(3)=0.696962D-01
      CS(4)=0.238487D+00
      CS(5)=0.483307D+00
      CS(6)=0.338074D+00
      CS(7)=-0.237677D-02
      CS(8)=-0.316930D-01
      CS(9)=-0.113317D+00
      CS(10)=0.560900D-01
      CS(11)=0.592255D+00
      CS(12)=0.455006D+00
      CS(13)=-0.286089D+00
      CS(14)=0.122806D+01
      CS(15)=0.100000D+01
      CP(1)=0.406101D-02
      CP(2)=0.306813D-01
      CP(3)=0.130452D+00
      CP(4)=0.327205D+00
      CP(5)=0.452851D+00
      CP(6)=0.256042D+00
      CP(7)=0.164777D+00
      CP(8)=0.870855D+00
      CP(9)=0.100000D+00
      GO TO 1150
      
      
1010  GO TO(1020,1030,1060,1040,1050,1070),NGAUSS
1020  CONTINUE
1030  CONTINUE
1040  CONTINUE
1050  CONTINUE
      CALL BERROR(IERR1)
1060  CONTINUE
      E(1)=0.137640D+04
      E(2)=0.207857D+03
      E(3)=0.451554D+02
      E(4)=0.608014D+02
      E(5)=0.139765D+02
      E(6)=0.388710D+01
      E(7)=0.135299D+01
      E(8)=0.526955D+00
      E(9)=0.166714D+00
      CS(1)=0.645827D-01
      CS(2)=0.380363D+00
      CS(3)=0.678190D+00
      CS(4)=-0.987639D-01
      CS(5)=0.511338D-01
      CS(6)=0.991337D+00
      CS(7)=-0.222401D+00
      CS(8)=0.118252D+01
      CS(9)=0.100000D+01
      CP(1)=0.108598D+00
      CP(2)=0.458682D+00
      CP(3)=0.601962D+00
      CP(4)=0.219216D+00
      CP(5)=0.822321D+00
      CP(6)=0.100000D+00
      GO TO 1150
1070  CONTINUE
      E(1)=0.251801D+05
      E(2)=0.378035D+04
      E(3)=0.860474D+03
      E(4)=0.242145D+03
      E(5)=0.773349D+02
      E(6)=0.262470D+02
      E(7)=0.491765D+03
      E(8)=0.116984D+03
      E(9)=0.374153D+02
      E(10)=0.137834D+02
      E(11)=0.545215D+01
      E(12)=0.222588D+01
      E(13)=0.135299D+01
      E(14)=0.526955D+00
      E(15)=0.166714D+00
      CS(1)=0.183296D-02
      CS(2)=0.140342D-01
      CS(3)=0.690974D-01
      CS(4)=0.237452D+00
      CS(5)=0.483034D+00
      CS(6)=0.339856D+00
      CS(7)=-0.229739D-02
      CS(8)=-0.307137D-01
      CS(9)=-0.112528D+00
      CS(10)=0.450163D-01
      CS(11)=0.589353D+00
      CS(12)=0.465206D+00
      CS(13)=-0.222401D+00
      CS(14)=0.118252D+01
      CS(15)=0.100000D+01
      CP(1)=0.398940D-02
      CP(2)=0.303177D-01
      CP(3)=0.129880D+00
      CP(4)=0.327951D+00
      CP(5)=0.453527D+00
      CP(6)=0.252154D+00
      CP(7)=0.219216D+00
      CP(8)=0.822321D+00
      CP(9)=0.100000D+01
      GO TO 1150
      
      
1080  GO TO(1090,1100,1130,1110,1120,1140),NGAUSS
1090  CONTINUE
1100  CONTINUE
1110  CONTINUE
1120  CONTINUE
      CALL BERROR(IERR1)
1130  CONTINUE
      E(1)=0.155371D+04
      E(2)=0.234678D+03
      E(3)=0.510121D+02
      E(4)=0.700453D+02
      E(5)=0.161473D+02
      E(6)=0.453492D+01
      E(7)=0.154209D+01
      E(8)=0.607267D+00
      E(9)=0.195373D+00
      CS(1)=0.641707D-01
      CS(2)=0.378797D+00
      CS(3)=0.679752D+00
      CS(4)=-0.974661D-01
      CS(5)=0.390569D-01
      CS(6)=0.999916D+00
      CS(7)=-0.176866D+00
      CS(8)=0.114690D+01
      CS(9)=0.100000D+01
      CP(1)=0.107619D+00
      CP(2)=0.459576D+00
      CP(3)=0.600041D+00
      CP(4)=0.255687D+00
      CP(5)=0.789842D+00
      CP(6)=0.100000D+01
      GO TO 1150
1140  CONTINUE
      E(1)=0.283483D+05
      E(2)=0.425762D+04
      E(3)=0.969857D+03
      E(4)=0.273263D+03
      E(5)=0.873695D+02
      E(6)=0.296867D+02
      E(7)=0.575891D+03
      E(8)=0.136816D+03
      E(9)=0.438098D+02
      E(10)=0.162094D+02
      E(11)=0.646084D+01
      E(12)=0.265114D+01
      E(13)=0.154209D+01
      E(14)=0.607267D+00
      E(15)=0.195373D+00
      CS(1)=0.182526D-02
      CS(2)=0.139686D-01
      CS(3)=0.687073D-01
      CS(4)=0.236204D+00
      CS(5)=0.482214D+00
      CS(6)=0.342043D+00
      CS(7)=-0.215972D-02
      CS(8)=-0.290775D-01
      CS(9)=-0.110827D+00
      CS(10)=0.276999D-01
      CS(11)=0.577613D+00
      CS(12)=0.488688D+00
      CS(13)=-0.176866D+00
      CS(14)=0.114690D+01
      CS(15)=0.100000D+01
      CP(1)=0.380665D-02
      CP(2)=0.292305D-01
      CP(3)=0.126467D+00
      CP(4)=0.323510D+00
      CP(5)=0.454896D+00
      CP(6)=0.256630D+00
      CP(7)=0.255687D+00
      CP(8)=0.789842D+00
      CP(9)=0.100000D+00
1150  CONTINUE
      RETURN
      END
C* :1 * 
      
