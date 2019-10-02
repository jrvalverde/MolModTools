
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s3sp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s3sp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "s3sp.web"
      subroutine s3sp(EXX,CS,CP,NGAUSS)
      implicit none
      double precision CP,CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6),CP(6)
      
      if(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.3)then
      EXX(1)=4.828540806D-01
      CS(1)=-2.196203690D-01
      CP(1)=1.058760429D-02
      EXX(2)=1.347150629D-01
      CS(2)=2.255954336D-01
      CP(2)=5.951670053D-01
      EXX(3)=5.272656258D-02
      CS(3)=9.003984260D-01
      CP(3)=4.620010120D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=9.217072439D-01
      CS(1)=-8.529019644D-02
      CP(1)=-2.504945181D-02
      EXX(2)=2.534730123D-01
      CS(2)=-2.132074034D-01
      CP(2)=1.686604461D-01
      EXX(3)=9.976476472D-02
      CS(3)=5.920843928D-01
      CP(3)=6.409553151D-01
      EXX(4)=4.439990833D-02
      CS(4)=6.115584746D-01
      CP(4)=2.779508957D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=1.709113496D00
      CS(1)=-2.662203391D-02
      CP(1)=-1.566883448D-02
      EXX(2)=4.654245585D-01
      CS(2)=-1.603484072D-01
      CP(2)=7.214920506D-03
      EXX(3)=1.785129144D-01
      CS(3)=-4.779473307D-02
      CP(3)=3.170854762D-01
      EXX(4)=8.067420191D-02
      CS(4)=7.275158441D-01
      CP(4)=5.818821382D-01
      EXX(5)=3.914669014D-02
      CS(5)=4.123846408D-01
      CP(5)=1.701799824D-01
      return
      elseif(NGAUSS.EQ.6)then
      goto 100
      else
      EXX(1)=7.163507065D-02
      CS(1)=1.000000000D00
      CP(1)=1.000000000D00
      endif
      EXX(1)=1.939790354D-01
      CS(1)=-2.983986044D-01
      CP(1)=3.480471912D-01
      EXX(2)=6.655227113D-02
      CS(2)=1.227982887D00
      CP(2)=7.222523221D-01
      return
100   EXX(1)=3.080165240D00
      CS(1)=-7.943126362D-03
      CP(1)=-7.139358907D-03
      EXX(2)=8.248959202D-01
      CS(2)=-7.100264172D-02
      CP(2)=-1.829277070D-02
      EXX(3)=3.093447349D-01
      CS(3)=-1.785026925D-01
      CP(3)=7.621621428D-02
      EXX(4)=1.384683897D-01
      CS(4)=1.510635058D-01
      CP(4)=4.145098597D-01
      EXX(5)=6.852094951D-02
      CS(5)=7.354914767D-01
      CP(5)=4.889621471D-01
      EXX(6)=3.531333690D-02
      CS(6)=2.760593123D-01
      CP(6)=1.058816521D-01
      return
      
      end
C* :1 * 
      
