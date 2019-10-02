
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 s5sp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "s5sp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "s5sp.web"
      subroutine s5sp(EXX,CS,CP,NGAUSS)
      implicit none
      real*8 CP,CS,EXX
      integer NGAUSS
      dimension EXX(6),CS(6),CP(6)
      
      if(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.3)then
      EXX(1)=1.349013828D-01
      CS(1)=-3.842642607D-01
      CP(1)=-3.481691526D-01
      EXX(2)=7.263605443D-02
      CS(2)=-1.972567438D-01
      CP(2)=6.290323690D-01
      EXX(3)=3.208462257D-02
      CS(3)=1.375495512D+00
      CP(3)=6.662832743D-01
      return
      elseif(NGAUSS.EQ.4)then
      EXX(1)=2.577707269D-01
      CS(1)=4.045111204D-02
      CP(1)=-8.586149652D-02
      EXX(2)=1.189639508D-01
      CS(2)=-6.576691017D-01
      CP(2)=-1.090154130D-01
      EXX(3)=5.270776581D-02
      CS(3)=3.792524265D-01
      CP(3)=7.234042095D-01
      EXX(4)=2.870356027D-02
      CS(4)=1.038589734D+00
      CP(4)=4.117428940D-01
      return
      elseif(NGAUSS.EQ.5)then
      EXX(1)=4.822618266D-01
      CS(1)=3.037662332D-02
      CP(1)=-1.257672570D-02
      EXX(2)=1.813516259D-01
      CS(2)=-1.825060513D-01
      CP(2)=-1.563935424D-01
      EXX(3)=8.588912730D-02
      CS(3)=-6.591492319D-01
      CP(3)=6.500746946D-02
      EXX(4)=4.617413271D-02
      CS(4)=9.186932397D-01
      CP(4)=7.859820497D-01
      EXX(5)=2.582552665D-02
      CS(5)=6.952739594D-01
      CP(5)=2.582283808D-01
      return
      elseif(NGAUSS.EQ.6)then
      goto 100
      else
      call lnk1e
      endif
      EXX(1)=8.124323517D-02
      CS(1)=-1.093689792D+00
      CP(1)=-1.047403535D-01
      EXX(2)=3.930355482D-02
      CS(2)=1.881374656D+00
      CP(2)=1.087626793D+00
      return
100   EXX(1)=7.701420258D-01
      CS(1)=1.267447151D-02
      CP(1)=-1.105673292D-03
      EXX(2)=2.756268915D-01
      CS(2)=3.266734789D-03
      CP(2)=-6.243132446D-02
      EXX(3)=1.301847480D-01
      CS(3)=-4.307553999D-01
      CP(3)=-1.628476766D-01
      EXX(4)=6.953441940D-02
      CS(4)=-3.231998963D-01
      CP(4)=3.210328714D-01
      EXX(5)=4.002545502D-02
      CS(5)=1.104322879D+00
      CP(5)=6.964579592D-01
      EXX(6)=2.348388309D-02
      CS(6)=4.368498703D-01
      CP(6)=1.493146125D-01
      return
      end
C* :1 * 
      
