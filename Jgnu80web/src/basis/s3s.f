
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s3s"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s3s.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "s3s.web"
      subroutine s3s(EXX,CS,NGAUSS)
      implicit none
      double precision CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6)
      
      if(NGAUSS.EQ.2)then
      EXX(1)=6.694095822D-01
      CS(1)=-1.529645716D-01
      EXX(2)=5.837135094D-02
      CS(2)=1.051370110D00
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=5.641487709D-01
      CS(1)=-1.782577972D-01
      EXX(2)=6.924421391D-02
      CS(2)=8.612761663D-01
      EXX(3)=3.269529097D-02
      CS(3)=2.261841969D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=1.513265591D00
      CS(1)=-3.295496352D-02
      EXX(2)=4.262497508D-01
      CS(2)=-1.724516959D-01
      EXX(3)=7.643320863D-02
      CS(3)=7.518511194D-01
      EXX(4)=3.760545063D-02
      CS(4)=3.589627317D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=4.275877914D00
      CS(1)=-3.920358850D-03
      EXX(2)=1.132409433D00
      CS(2)=-4.168430506D-02
      EXX(3)=4.016256968D-01
      CS(3)=-1.637440990D-01
      EXX(4)=7.732370620D-02
      CS(4)=7.419373723D-01
      EXX(5)=3.800708627D-02
      CS(5)=3.724364929D-01
      return
      elseif(NGAUSS.NE.6)then
      EXX(1)=5.296881757D-02
      CS(1)=1.000000000D00
      return
      endif
      EXX(1)=3.273031938D00
      CS(1)=-6.775596947D-03
      EXX(2)=9.200611311D-01
      CS(2)=-5.639325779D-02
      EXX(3)=3.593349765D-01
      CS(3)=-1.587856086D-01
      EXX(4)=8.636686991D-02
      CS(4)=5.534527651D-01
      EXX(5)=4.797373812D-02
      CS(5)=5.015351020D-01
      EXX(6)=2.724741144D-02
      CS(6)=7.223633674D-02
      return
      
      end
C* :1 * 
      
