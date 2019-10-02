
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aout"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aout.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "aout.web"
      subroutine aout(A,MR,NR,NC,TITLE,INDEX,IFLG)
      implicit none
      double precision A
      integer i,iat,Iatcr,Iatno,iecp,IFLG,INDEX,Ino,ishell,Ispin,Iznuc,j
     &flg,kfull,klew,kval,Lbl,Ll,Lu,MAXATM,MAXBAS
      integer MR,mult,Munit,Mxao,Mxaolm,Mxbo,Natoms,Nbas,NC,Ndim,Nlew,No
     &rbs,NR,Nval
      dimension A(MR,1)
      character*80 TITLE
      dimension ishell(4)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nblbl/Nlew,Nval,Lbl(10,MAXBAS,4)
      common/nbflag/Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      logical Rohf,Uhf,Ci,Open,Complex,Alpha,Beta,Mcscf,Auhf,Ortho
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      
      data kfull,kval,klew/4HFULL,3HVAL,3HLEW/
      
      
      
      
      
      
      
      
      
      jflg=IFLG
      if(jflg.EQ.0)return
      
      
      if(jflg.EQ.kfull)jflg=abs(NC)
      
      
      if(jflg.EQ.kval)then
      if(Nval.LT.0)then
      iecp=0
      jflg=0
      do 40 iat=1,Natoms
      call cortbl(iat,ishell,iecp)
      do 10 i=1,4
      mult=2*(i-1)+1
      jflg=jflg+ishell(i)*mult
10    continue
      call valtbl(iat,ishell)
      do 20 i=1,4
      mult=2*(i-1)+1
      jflg=jflg+ishell(i)*mult
20    continue
40    continue
      else
      jflg=Nval
      endif
      endif
      
      
      if(jflg.EQ.klew)jflg=Nlew
      
      
      if(jflg.GT.0)call aprint(A,MR,NR,NC,TITLE,INDEX,jflg)
      
      
      if(jflg.LT.0.AND.jflg.GT.-1000)call awrite(A,MR,NR,NC,TITLE,jflg)
      
      return
      end
C* :1 * 
      
