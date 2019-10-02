
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 etwo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "etwo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "etwo.web"
      subroutine etwo(E1,E2,E3,CS1,CS2,CS3,CP2,CP3,NGAUSS,NSPLIT,IA)
      implicit none
      double precision CP2,CP3,CS1,CS2,CS3,E1,E2,E3
      integer IA,NGAUSS,NSPLIT
      dimension E1(6),E2(6),E3(6),CS1(6),CS2(6),CS3(6),CP2(6),CP3(6)
      
      if(IA.EQ.1.OR.IA.EQ.2.OR.IA.EQ.3.OR.IA.EQ.4.OR.IA.EQ.5.OR.IA.EQ.6.
     &OR.IA.EQ.7.OR.IA.EQ.8.OR.IA.EQ.9.OR.IA.EQ.10.OR.IA.EQ.18)goto 800
      if(IA.EQ.12)then
      elseif(IA.EQ.13)then
      goto 100
      elseif(IA.EQ.14)then
      call berror(2)
      goto 200
      elseif(IA.EQ.15)then
      goto 200
      elseif(IA.EQ.16)then
      goto 400
      elseif(IA.EQ.17)then
      goto 600
      else
      call berror(2)
      endif
      call berror(2)
100   call berror(2)
      call berror(2)
200   if(NGAUSS.EQ.1)then
      elseif(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.4)then
      goto 300
      elseif(NGAUSS.EQ.5)then
      
      call berror(2)
      goto 400
      elseif(NGAUSS.EQ.6)then
      call berror(2)
      goto 400
      endif
      
      call berror(2)
300   E1(1)=3.018671780D03
      CS1(1)=1.852131370D-02
      E1(2)=4.551271210D02
      CS1(2)=1.299048640D-01
      E1(3)=1.023147300D02
      CS1(3)=4.551002880D-01
      E1(4)=2.761784730D01
      CS1(4)=5.331318610D-01
      E2(1)=1.144294010D02
      CS2(1)=-2.475029610D-02
      CP2(1)=2.741400250D-02
      E2(2)=2.658229590D01
      CS2(2)=-1.350924600D-01
      CP2(2)=1.690791420D-01
      E2(3)=7.871888900D00
      CS2(3)=2.277360800D-01
      CP2(3)=4.691020890D-01
      E2(4)=2.487857250D00
      CS2(4)=8.755931160D-01
      CP2(4)=5.181530590D-01
      E3(1)=5.075061900D01
      CS3(1)=-4.511922300D-02
      CP3(1)=3.779071180D-03
      E3(2)=1.672862420D00
      CS3(2)=-8.504729900D-01
      CP3(2)=-4.634384050D-02
      E3(3)=6.210974120D-01
      CS3(3)=1.596285850D00
      CP3(3)=1.033944290D00
      E3(4)=1.670160070D-01
      CS3(4)=1.000000000D00
      CP3(4)=1.000000000D00
      return
400   if(NGAUSS.EQ.1)then
      elseif(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.4)then
      goto 500
      elseif(NGAUSS.EQ.5)then
      
      call berror(2)
      goto 600
      elseif(NGAUSS.EQ.6)then
      call berror(2)
      goto 600
      endif
      
      call berror(2)
500   E1(1)=3.442124410D03
      CS1(1)=1.849212360D-02
      E1(2)=5.189131000D02
      CS1(2)=1.298220220D-01
      E1(3)=1.166909030D02
      CS1(3)=4.550417870D-01
      E1(4)=3.157164720D01
      CS1(4)=5.330083560D-01
      E2(1)=1.274405760D02
      CS2(1)=-2.726461060D-02
      CP2(1)=2.915199950D-02
      E2(2)=2.974766730D01
      CS2(2)=-1.424834150D-01
      CP2(2)=1.779596760D-01
      E2(3)=8.834664280D00
      CS2(3)=2.597043520D-01
      CP2(3)=4.836237120D-01
      E2(4)=2.817389820D00
      CS2(4)=8.525472950D-01
      CP2(4)=4.942553020D-01
      E3(1)=3.729185370D00
      CS3(1)=-2.775315230D-01
      CP3(1)=-3.375092630D-02
      E3(2)=1.406770170D00
      CS3(2)=-4.576434550D-01
      CP3(2)=1.457110450D-01
      E3(3)=5.481099690D-01
      CS3(3)=1.431684270D00
      CP3(3)=8.982887430D-01
      E3(4)=1.703809050D-01
      CS3(4)=9.999999990D-01
      CP3(4)=1.000000000D00
      return
600   if(NGAUSS.EQ.1)then
      elseif(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.4)then
      goto 700
      elseif(NGAUSS.EQ.5)then
      
      call berror(2)
      goto 800
      elseif(NGAUSS.EQ.6)then
      call berror(2)
      goto 800
      endif
      
      call berror(2)
700   E1(1)=3.910302690D03
      CS1(1)=1.837943110D-02
      E1(2)=5.895518070D02
      CS1(2)=1.291401230D-01
      E1(3)=1.325939240D02
      CS1(3)=4.540448900D-01
      E1(4)=3.590354250D01
      CS1(4)=5.344394360D-01
      E2(1)=1.477653530D02
      CS2(1)=-2.674332300D-02
      CP2(1)=2.886446880D-02
      E2(2)=3.450607530D01
      CS2(2)=-1.446911820D-01
      CP2(2)=1.779646700D-01
      E2(3)=1.028647150D01
      CS2(3)=2.517035690D-01
      CP2(3)=4.869998070D-01
      E2(4)=3.311147380D00
      CS2(4)=8.598203810D-01
      CP2(4)=4.890184500D-01
      E3(1)=4.280284910D00
      CS3(1)=-2.703962750D-01
      CP3(1)=-3.670288510D-02
      E3(2)=1.641016670D00
      CS3(2)=-3.416297190D-01
      CP3(2)=1.918492420D-01
      E3(3)=6.144785030D-01
      CS3(3)=1.350024480D00
      CP3(3)=8.643376810D-01
      E3(4)=1.956594110D-01
      CS3(4)=9.999999990D-01
      CP3(4)=1.000000000D00
      return
800   return
      
      end
C* :1 * 
      
