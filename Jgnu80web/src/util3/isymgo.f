
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 isymgo"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "isymgo.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "isymgo.web"
      subroutine isymgo(I,J,K,L,NSYMOP,NEQSHL,ISYM2E,REJECT,SYMFAC)
      implicit none
      integer I,iop,ip,iprio,ISYM2E,isymf,J,jp,K,kp,L,LENB,lp,MAXPRM,MAX
     &S21,MAXSH1,MAXSHL,mprio,NEQSHL,nprio
      integer NSYMOP
      double precision one,SYMFAC
      logical REJECT
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1),LENB=(15*MAXSHL+7*MAXSHL/2+1))
      dimension NEQSHL(MAXSHL,8)
      data one/1.0D0/
      
      
      
      
      
      
      if(ISYM2E.EQ.1)then
      mprio=nprio(I,J,K,L)
      isymf=1
      do 50 iop=2,NSYMOP
      ip=NEQSHL(I,iop)
      jp=NEQSHL(J,iop)
      kp=NEQSHL(K,iop)
      lp=NEQSHL(L,iop)
      iprio=nprio(ip,jp,kp,lp)
      if(iprio.GT.mprio)goto 100
      if(iprio.EQ.mprio)isymf=isymf+1
50    continue
      SYMFAC=dfloat(isymf)
      REJECT=.FALSE.
      else
      SYMFAC=one
      REJECT=.FALSE.
      endif
      goto 200
100   SYMFAC=one
      REJECT=.TRUE.
      
200   return
      
      end
C* :1 * 
      
