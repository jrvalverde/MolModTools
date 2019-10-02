
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s4sp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s4sp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "s4sp.web"
      subroutine s4sp(EXX,CS,CP,NGAUSS)
      implicit none
      double precision CP,CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6),CP(6)
      
      if(NGAUSS.EQ.2)then
      EXX(1)=1.242349951D-01
      CS(1)=-6.615603018D-01
      CP(1)=1.309890515D-01
      EXX(2)=4.999746691D-02
      CS(2)=1.510754134D00
      CP(2)=8.946431268D-01
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=2.464581400D-01
      CS(1)=-3.088441215D-01
      CP(1)=-1.215468600D-01
      EXX(2)=9.095855374D-02
      CS(2)=1.960641166D-02
      CP(2)=5.715227604D-01
      EXX(3)=4.016825636D-02
      CS(3)=1.131034442D00
      CP(3)=5.498949471D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=4.389527791D-01
      CS(1)=-5.893559813D-02
      CP(1)=-7.180522657D-02
      EXX(2)=1.624449952D-01
      CS(2)=-4.289230261D-01
      CP(2)=3.726863425D-02
      EXX(3)=6.983976374D-02
      CS(3)=5.472998468D-01
      CP(3)=6.965842249D-01
      EXX(4)=3.469204610D-02
      CS(4)=7.853692030D-01
      CP(4)=3.310236850D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=7.857764224D-01
      CS(1)=-1.628249797D-03
      CP(1)=-2.399519595D-02
      EXX(2)=2.685947984D-01
      CS(2)=-2.138931336D-01
      CP(2)=-8.118321551D-02
      EXX(3)=1.155680976D-01
      CS(3)=-3.039583456D-01
      CP(3)=2.238628944D-01
      EXX(4)=5.839870334D-02
      CS(4)=8.491175450D-01
      CP(4)=6.726900408D-01
      EXX(5)=3.082651925D-02
      CS(5)=5.164147260D-01
      CP(5)=2.003802746D-01
      return
      elseif(NGAUSS.NE.6)then
      stop
      endif
      EXX(1)=3.080165240D00
      EXX(2)=8.248959202D-01
      EXX(3)=3.093447349D-01
      EXX(4)=1.384683897D-01
      EXX(5)=6.852094951D-02
      EXX(6)=3.531333690D-02
      CS(1)=-7.943126362D-03
      CS(2)=-7.100264172D-02
      CS(3)=-1.785026925D-01
      CS(4)=1.510635058D-01
      CS(5)=7.354914767D-01
      CS(6)=2.760593123D-01
      CP(1)=-7.139358907D-03
      CP(2)=-1.829277070D-02
      CP(3)=7.621621428D-02
      CP(4)=4.145098597D-01
      CP(5)=4.889621471D-01
      CP(6)=1.058816521D-01
      return
      
      end
C* :1 * 
      
