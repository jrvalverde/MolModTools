
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s2sp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s2sp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "s2sp.web"
      subroutine s2sp(EXX,CS,CP,NGAUSS)
      implicit none
      double precision CP,CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6),CP(6)
      
      if(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.3)then
      EXX(1)=9.942027296D-01
      CS(1)=-9.996722919D-02
      CP(1)=1.559162750D-01
      EXX(2)=2.310313333D-01
      CS(2)=3.995128261D-01
      CP(2)=6.076837186D-01
      EXX(3)=7.513856000D-02
      CS(3)=7.001154689D-01
      CP(3)=3.919573931D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=2.323503675D00
      CS(1)=-6.220714565D-02
      CP(1)=4.368434884D-02
      EXX(2)=5.029886906D-01
      CS(2)=2.976804596D-05
      CP(2)=2.863793984D-01
      EXX(3)=1.635406719D-01
      CS(3)=5.588549221D-01
      CP(3)=5.835753141D-01
      EXX(4)=6.281044213D-02
      CS(4)=4.977673218D-01
      CP(4)=2.463134378D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=5.036294248D00
      CS(1)=-2.940855088D-02
      CP(1)=1.255609128D-02
      EXX(2)=1.032503477D00
      CS(2)=-6.532746883D-02
      CP(2)=1.075576962D-01
      EXX(3)=3.290598322D-01
      CS(3)=1.289973181D-01
      CP(3)=3.735975367D-01
      EXX(4)=1.279200125D-01
      CS(4)=6.122899938D-01
      CP(4)=5.102395637D-01
      EXX(5)=5.449486448D-02
      CS(5)=3.461205655D-01
      CP(5)=1.568281801D-01
      return
      elseif(NGAUSS.EQ.6)then
      goto 100
      else
      call berror(1)
      endif
      EXX(1)=3.842442531D-01
      CS(1)=4.947176920D-02
      CP(1)=5.115407076D-01
      EXX(2)=9.745448900D-02
      CS(2)=9.637824081D-01
      CP(2)=6.128198961D-01
      return
100   EXX(1)=1.030869372D01
      CS(1)=-1.325278809D-02
      CP(1)=3.759696623D-03
      EXX(2)=2.040359519D00
      CS(2)=-4.699171014D-02
      CP(2)=3.767936984D-02
      EXX(3)=6.341422177D-01
      CS(3)=-3.378537151D-02
      CP(3)=1.738967435D-01
      EXX(4)=2.439773685D-01
      CS(4)=2.502417861D-01
      CP(4)=4.180364347D-01
      EXX(5)=1.059595374D-01
      CS(5)=5.951172526D-01
      CP(5)=4.258595477D-01
      EXX(6)=4.856900860D-02
      CS(6)=2.407061763D-01
      CP(6)=1.017082955D-01
      return
      
      end
C* :1 * 
      
