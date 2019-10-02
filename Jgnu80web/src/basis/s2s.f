
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s2s"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s2s.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "s2s.web"
      subroutine s2s(EXX,CS,NGAUSS)
      implicit none
      double precision CS,EXX
      integer jdex,kdex,NGAUSS
      dimension EXX(6),CS(6)
      
      jdex=1
      kdex=36
      if(NGAUSS.EQ.2)then
      EXX(1)=1.292278611D-01
      CS(1)=7.470867124D-01
      EXX(2)=4.908584205D-02
      CS(2)=2.855980556D-01
      return
      elseif(NGAUSS.EQ.3)then
      EXX(1)=2.581578398D00
      CS(1)=-5.994474934D-02
      EXX(2)=1.567622104D-01
      CS(2)=5.960385398D-01
      EXX(3)=6.018332272D-02
      CS(3)=4.581786291D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=1.161525551D01
      CS(1)=-1.198411747D-02
      EXX(2)=2.000243111D00
      CS(2)=-5.472052539D-02
      EXX(3)=1.607280687D-01
      CS(3)=5.805587176D-01
      EXX(4)=6.125744532D-02
      CS(4)=4.770079976D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=8.984956862D00
      CS(1)=-1.596349096D-02
      EXX(2)=1.673710636D00
      CS(2)=-5.685884883D-02
      EXX(3)=1.944726668D-01
      CS(3)=3.698265599D-01
      EXX(4)=8.806345634D-02
      CS(4)=5.480512593D-01
      EXX(5)=4.249068522D-02
      CS(5)=1.472634893D-01
      return
      elseif(NGAUSS.NE.6)then
      EXX(1)=1.012151084D-01
      CS(1)=1.000000000D00
      return
      endif
      EXX(1)=2.768496241D01
      CS(1)=-4.151277819D-03
      EXX(2)=5.077140627D00
      CS(2)=-2.067024148D-02
      EXX(3)=1.426786050D00
      CS(3)=-5.150303337D-02
      EXX(4)=2.040335729D-01
      CS(4)=3.346271174D-01
      EXX(5)=9.260298399D-02
      CS(5)=5.621061301D-01
      EXX(6)=4.416183978D-02
      CS(6)=1.712994697D-01
      return
      
      end
C* :1 * 
      
