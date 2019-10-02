
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 bdfind"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "bdfind.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "bdfind.web"
      function bdfind(IAT,JAT)
      implicit none
      integer IAT,Iathy,ib,ibas,Ibxm,Ispin,JAT,k,Label,Larc,lstar,Lstocc
     &,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Naoctr,Naol
      integer Natoms,Nbas,Ndim
      logical bdfind,ifound,jfound
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Naoctr(MAXBAS),Naol(MAXBAS),Lstocc(MA
     &XBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      
      data lstar/1H*/
      
      
      do 100 ibas=1,Nbas
      ib=Ibxm(ibas)
      if(Label(ib,2).NE.lstar)then
      if(Label(ib,3).EQ.1)then
      ifound=.FALSE.
      jfound=.FALSE.
      do 10 k=4,6
      if(Label(ib,k).EQ.IAT)ifound=.TRUE.
      if(Label(ib,k).EQ.JAT)jfound=.TRUE.
10    continue
      if(ifound.AND.jfound)goto 200
      endif
      endif
100   continue
      bdfind=.FALSE.
      return
200   bdfind=.TRUE.
      return
      end
C* :1 * 
      
