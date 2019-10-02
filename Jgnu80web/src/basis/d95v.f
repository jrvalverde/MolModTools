
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 d95v"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "d95v.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "d95v.web"
      subroutine d95v(E,S,P,IA,NCONT,NGAUSS,ISHT,ISHC)
      implicit none
      real*8 E,P,S
      integer i,IA,ifirst,ilast,ISHC,ISHT,NCONT,NGAUSS,np,ns
      
      dimension E(20),S(20),P(20),NGAUSS(20),ISHT(20),ISHC(20)
      
      
      if(IA.GT.2)then
      if(IA.GT.10)call lnk1e
      NCONT=5
      NGAUSS(1)=7
      NGAUSS(2)=2
      NGAUSS(3)=1
      NGAUSS(4)=4
      NGAUSS(5)=1
      ns=3
      np=2
      else
      NCONT=2
      NGAUSS(1)=3
      NGAUSS(2)=1
      ns=2
      np=0
      endif
      call iclear(ns,ISHC)
      call iclear(ns,ISHT)
      if(np.GE.1)then
      ifirst=ns+1
      ilast=ns+np
      do 50 i=ifirst,ilast
      ISHT(i)=1
      ISHC(i)=1
50    continue
      endif
      if(IA.EQ.2)then
      
      
      write(6,*)' No D95V basis for He'
      call lnk1e
      elseif(IA.EQ.3)then
      
      
      NGAUSS(4)=3
      E(1)=9.213D+02
      S(1)=0.001367D+00
      E(2)=1.387D+02
      S(2)=0.010425D+00
      E(3)=3.194D+01
      S(3)=0.049859D+00
      E(4)=9.353D+00
      S(4)=0.160701D+00
      E(5)=3.158D+00
      S(5)=0.344604D+00
      E(6)=1.157D+00
      S(6)=0.425197D+00
      E(7)=4.446D-01
      S(7)=0.169468D+00
      E(8)=4.446D-01
      S(8)=-0.222311D+00
      E(9)=7.666D-02
      S(9)=1.116477D+00
      E(10)=2.864D-02
      S(10)=1.000000D+00
      E(11)=1.488D+00
      P(11)=0.038770D+00
      E(12)=2.667D-01
      P(12)=0.236257D+00
      E(13)=7.201D-02
      P(13)=0.830448D+00
      E(14)=2.370D-02
      P(14)=1.000000D+00
      return
      elseif(IA.EQ.4)then
      
      
      E(1)=1.741D+03
      S(1)=0.001305D+00
      E(2)=2.621D+02
      S(2)=0.009955D+00
      E(3)=6.033D+01
      S(3)=0.048031D+00
      E(4)=1.762D+01
      S(4)=0.158577D+00
      E(5)=5.933D+00
      S(5)=0.351325D+00
      E(6)=2.185D+00
      S(6)=0.427006D+00
      E(7)=8.590D-01
      S(7)=0.160490D+00
      E(8)=2.185D+00
      S(8)=-0.185294D+00
      E(9)=1.806D-01
      S(9)=1.057014D+00
      E(10)=5.835D-02
      S(10)=1.000000D+00
      E(11)=6.710D+00
      P(11)=0.016378D+00
      E(12)=1.442D+00
      P(12)=0.091553D+00
      E(13)=4.103D-01
      P(13)=0.341469D+00
      E(14)=1.397D-01
      P(14)=0.685428D+00
      E(15)=4.922D-02
      P(15)=1.000000D+00
      return
      elseif(IA.EQ.5)then
      
      
      E(1)=2.788D+03
      S(1)=0.001288D+00
      E(2)=4.190D+02
      S(2)=0.009835D+00
      E(3)=9.647D+01
      S(3)=0.047648D+00
      E(4)=2.807D+01
      S(4)=0.160069D+00
      E(5)=9.376D+00
      S(5)=0.362894D+00
      E(6)=3.406D+00
      S(6)=0.433582D+00
      E(7)=1.306D+00
      S(7)=0.140082D+00
      E(8)=3.406D+00
      S(8)=-0.179330D+00
      E(9)=3.245D-01
      S(9)=1.062594D+00
      E(10)=1.022D-01
      S(10)=1.000000D+00
      E(11)=1.134D+01
      P(11)=0.017988D+00
      E(12)=2.436D+00
      P(12)=0.110343D+00
      E(13)=6.836D-01
      P(13)=0.383072D+00
      E(14)=2.134D-01
      P(14)=0.647895D+00
      E(15)=7.011D-02
      P(15)=1.000000D+00
      return
      elseif(IA.EQ.6)then
      
      
      E(1)=4.233D+03
      S(1)=0.001220D+00
      E(2)=6.349D+02
      S(2)=0.009342D+00
      E(3)=1.461D+02
      S(3)=0.045452D+00
      E(4)=4.250D+01
      S(4)=0.154657D+00
      E(5)=1.419D+01
      S(5)=0.358866D+00
      E(6)=5.148D+00
      S(6)=0.438632D+00
      E(7)=1.967D+00
      S(7)=0.145918D+00
      E(8)=5.148D+00
      S(8)=-0.168367D+00
      E(9)=4.962D-01
      S(9)=1.060091D+00
      E(10)=1.533D-01
      S(10)=1.000000D+00
      E(11)=1.816D+01
      P(11)=0.018539D+00
      E(12)=3.986D+00
      P(12)=0.115436D+00
      E(13)=1.143D+00
      P(13)=0.386188D+00
      E(14)=3.594D-01
      P(14)=0.640114D+00
      E(15)=1.146D-01
      P(15)=1.000000D+00
      return
      elseif(IA.EQ.7)then
      
      
      E(1)=5.909D+03
      S(1)=0.001190D+00
      E(2)=8.875D+02
      S(2)=0.009099D+00
      E(3)=2.047D+02
      S(3)=0.044145D+00
      E(4)=5.984D+01
      S(4)=0.150464D+00
      E(5)=2.000D+01
      S(5)=0.356741D+00
      E(6)=7.193D+00
      S(6)=0.446533D+00
      E(7)=2.686D+00
      S(7)=0.145603D+00
      E(8)=7.193D+00
      S(8)=-0.160405D+00
      E(9)=7.000D-01
      S(9)=1.058215D+00
      E(10)=2.133D-01
      S(10)=1.000000D+00
      E(11)=2.679D+01
      P(11)=0.018254D+00
      E(12)=5.956D+00
      P(12)=0.116461D+00
      E(13)=1.707D+00
      P(13)=0.390178D+00
      E(14)=5.314D-01
      P(14)=0.637102D+00
      E(15)=1.654D-01
      P(15)=1.000000D+00
      return
      elseif(IA.EQ.8)then
      
      
      E(1)=7.817D+03
      S(1)=0.001176D+00
      E(2)=1.176D+03
      S(2)=0.008968D+00
      E(3)=2.732D+02
      S(3)=0.042868D+00
      E(4)=8.117D+01
      S(4)=0.143930D+00
      E(5)=2.718D+01
      S(5)=0.355630D+00
      E(6)=9.532D+00
      S(6)=0.461248D+00
      E(7)=3.414D+00
      S(7)=0.140206D+00
      E(8)=9.532D+00
      S(8)=-0.154153D+00
      E(9)=9.398D-01
      S(9)=1.056914D+00
      E(10)=2.846D-01
      S(10)=1.000000D+00
      E(11)=3.518D+01
      P(11)=0.019580D+00
      E(12)=7.904D+00
      P(12)=0.124200D+00
      E(13)=2.305D+00
      P(13)=0.394714D+00
      E(14)=7.171D-01
      P(14)=0.627376D+00
      E(15)=2.137D-01
      P(15)=1.000000D+00
      return
      elseif(IA.EQ.9)then
      
      
      E(1)=9.995D+03
      S(1)=0.001166D+00
      E(2)=1.506D+03
      S(2)=0.008876D+00
      E(3)=3.503D+02
      S(3)=0.042380D+00
      E(4)=1.041D+02
      S(4)=0.142929D+00
      E(5)=3.484D+01
      S(5)=0.355372D+00
      E(6)=1.222D+01
      S(6)=0.462085D+00
      E(7)=4.369D+00
      S(7)=0.140848D+00
      E(8)=1.222D+01
      S(8)=-0.148452D+00
      E(9)=1.208D+00
      S(9)=1.055270D+00
      E(10)=3.634D-01
      S(10)=1.000000D+00
      E(11)=4.436D+01
      P(11)=0.020876D+00
      E(12)=1.008D+01
      P(12)=0.130107D+00
      E(13)=2.996D+00
      P(13)=0.396166D+00
      E(14)=9.383D-01
      P(14)=0.620404D+00
      E(15)=2.733D-01
      P(15)=1.000000D+00
      return
      elseif(IA.EQ.10)then
      
      
      E(1)=1.210D+04
      S(1)=0.001200D+00
      E(2)=1.821D+03
      S(2)=0.009092D+00
      E(3)=4.328D+02
      S(3)=0.041305D+00
      E(4)=1.325D+02
      S(4)=0.137867D+00
      E(5)=4.377D+01
      S(5)=0.362433D+00
      E(6)=1.491D+01
      S(6)=0.472247D+00
      E(7)=5.127D+00
      S(7)=0.130035D+00
      E(8)=1.491D+01
      S(8)=-0.140810D+00
      E(9)=1.491D+00
      S(9)=1.053327D+00
      E(10)=4.468D-01
      S(10)=1.000000D+00
      E(11)=5.645D+01
      P(11)=0.020875D+00
      E(12)=1.292D+01
      P(12)=0.130032D+00
      E(13)=3.865D+00
      P(13)=0.395679D+00
      E(14)=1.203D+00
      P(14)=0.621450D+00
      E(15)=3.444D-01
      P(15)=1.000000D+00
      return
      else
      
      
      E(1)=19.2406D0
      S(1)=0.032828D0
      E(2)=2.8992D0
      S(2)=0.231208D0
      E(3)=0.6534D0
      S(3)=0.817238D0
      E(4)=0.1776D0
      S(4)=1.0D+00
      return
      endif
      end
C* :1 * 
      