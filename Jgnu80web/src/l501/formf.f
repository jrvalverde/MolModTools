
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 formf"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "formf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "formf.web"
      subroutine formf(NBASIS,IONE,IOD,NSYMOP,NEQBAS)
      implicit none
      double precision a0,a00,a0c,a1,a1c,b0,Big,Da,Db,Dbuf2e,Fa,Fb,Filla
     &b,Filscr,Four,One,Onept5,pspin,pspin2,pt25
      double precision Pt5,ptotl,ptotl2,s1,s2,Small,Three,Two,val1,val2,
     &val3,Valint,Zero
      integer I,i1,Ia,Ibasd,Ibase,Ibuf2e,Icon,Ifil,iflst,ij,iji,ijj,ijk,
     &In,intape,Intcnt,IOD,IONE,Iout,Ipunch
      integer Iq,iqbufr,iqproc,Ireset,Ismode,Istat,Isym2e,Itotal,Iux,J,j
     &1,j2,j3,j4,j5,j6,Ja,jq,K,Kntt1
      integer Kntt2,Ksm,Kspin,Ksw,L,Last,Limint,lq,m,MAXBAS,MAXBP1,Mdim,
     &Mdsq,Mindx,Mode,Mshifs,Mtt,nb1,NBASIS,NEQBAS
      integer Nesk,Nest,Nest1,nfile,Nrpext,Nse,Nsep,NSYMOP,Ntt,Ntx,Nwiib
     &,Nwpi
      parameter(MAXBAS=150,MAXBP1=MAXBAS+1)
      logical Cmp,Rhf,fast1
      integer P,Q,R,S,Sindx
      integer Dbase,Dbasd,dcount
      dimension ij(MAXBP1),Ibuf2e(6400),Ia(2),IOD(4)
      dimension NEQBAS(MAXBAS,8)
      common/const/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/memry/Da(2485),Db(2485),Fa(2485),Fb(2485),Fillab(40060)
      common/scr/Dbuf2e(4760),Filscr(872)
      common/packed/I,J,K,L,Valint,Ja
      common/io/In,Iout,Ipunch
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Isym2e
      equivalence(I,P),(J,Q),(K,R),(L,S)
      equivalence(Sindx,L),(Mindx,K),(Ia(1),Valint)
      equivalence(pspin2,pspin),(ptotl2,ptotl)
      equivalence(Ibuf2e(1),Dbuf2e(1))
      data nfile/0/
      data pt25/0.25D0/
      data fast1/.FALSE./
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(25H1BAD INTEGRAL COUNT:  IS ,i9,5x,12H  SHOULD BE ,i9)
      
      
      nb1=NBASIS+1
      ij(1)=0
      do 100 I=2,nb1
      i1=I-1
      ij(I)=ij(i1)+i1
100   continue
      
      
      
      call aclear(Ntt,Fa(1))
      if(Cmp)call aclear(Ntt,Fa(1+Mtt))
      if(.NOT.(Rhf))then
      call aclear(Ntt,Fb(1))
      if(Cmp)call aclear(Ntt,Fb(1+Mtt))
      endif
      
      if(Ismode.NE.0)then
      if(Rhf)call rsetcl(NBASIS,Da(1),Da(1+Mtt),Cmp)
      if(.NOT.Rhf)call rsetop(NBASIS,Da(1),Db(1))
      endif
      
      
      
      iqbufr=1
      iqproc=2
      Intcnt=0
      intape=Iux(2)
      Ntx=1
      call iread(intape,iqbufr,Dbuf2e)
      Ifil=1
200   call iwait(intape)
      iqbufr=iabs(iqbufr-2)+1
      iqproc=iabs(iqproc-2)+1
      Ibase=Ibasd(iqproc)
      Dbase=Dbasd(iqproc)
      call labscf(Ibuf2e(Ibase),iflst)
      if(iflst.EQ.0)then
      if(Ifil.EQ.(nfile+Ntx*Icon))then
      call iwind(intape)
      Ntx=Ntx+1
      intape=Iux(Ntx+1)
      call iwind(intape)
      endif
      call iread(intape,iqbufr,Dbuf2e)
      Ifil=Ifil+1
      endif
      
      if(Mode.EQ.2)then
      
      
      
      if(.NOT.Rhf)call lnk1e
      if(Cmp)call lnk1e
      if(Kntt1.GT.0)then
      if(.NOT.fast1)then
      
      do 210 m=1,Kntt1
      Ja=Ibuf2e(m+Ibase)
      call unpck2
      Fa(I)=Fa(I)+Da(J)*Dbuf2e(m+Dbase)
      Fa(J)=Fa(J)+Da(I)*Dbuf2e(m+Dbase)
210   continue
      Intcnt=Intcnt+Kntt1
      else
      call raff1c(Fa,Da,Kntt1,Ibuf2e,Ibase,Dbase,Intcnt)
      endif
      endif
      elseif(Mode.EQ.3)then
      goto 300
      elseif(Mode.EQ.4)then
      goto 300
      else
      
      
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      do 220 m=jq,lq,Nwpi
      Ja=Ibuf2e(m)
      Ia(1)=Ibuf2e(m+1)
      Ia(2)=Ibuf2e(m+2)
      call unpck4
      a00=Valint+Valint
      iji=ij(I)
      ijj=ij(J)
      ijk=ij(K)
      j1=iji+J
      j2=ijk+L
      j3=iji+K
      if(J.LE.L)then
      
      j4=ij(L)+J
      s1=One
      else
      j4=ijj+L
      s1=-One
      endif
      j5=iji+L
      if(J.LE.K)then
      
      j6=ijk+J
      s2=One
      else
      j6=ijj+K
      s2=-One
      endif
      if(Rhf)then
      
      a0=Pt5*Valint
      a0c=Da(j2)*a00
      a1c=Da(j1)*a00
      else
      a0=Valint
      a0c=(Da(j2)+Db(j2))*a00
      a1c=(Da(j1)+Db(j1))*a00
      Fb(j1)=Fb(j1)+a0c
      Fb(j2)=Fb(j2)+a1c
      Fb(j3)=Fb(j3)-Db(j4)*a0
      Fb(j4)=Fb(j4)-Db(j3)*a0
      Fb(j5)=Fb(j5)-Db(j6)*a0
      Fb(j6)=Fb(j6)-Db(j5)*a0
      endif
      Fa(j1)=Fa(j1)+a0c
      Fa(j2)=Fa(j2)+a1c
      Fa(j3)=Fa(j3)-Da(j4)*a0
      Fa(j4)=Fa(j4)-Da(j3)*a0
      Fa(j5)=Fa(j5)-Da(j6)*a0
      Fa(j6)=Fa(j6)-Da(j5)*a0
      
      if(Cmp)then
      j3=j3+Mtt
      j4=j4+Mtt
      j5=j5+Mtt
      j6=j6+Mtt
      a1=s2*a0
      a0=s1*a0
      if(.NOT.(Rhf))then
      Fb(j3)=Fb(j3)-Db(j4)*a0
      Fb(j4)=Fb(j4)-Db(j3)*a0
      Fb(j5)=Fb(j5)-Db(j6)*a1
      Fb(j6)=Fb(j6)-Db(j5)*a1
      endif
      Fa(j3)=Fa(j3)-Da(j4)*a0
      Fa(j4)=Fa(j4)-Da(j3)*a0
      Fa(j5)=Fa(j5)-Da(j6)*a1
      Fa(j6)=Fa(j6)-Da(j5)*a1
      endif
220   continue
      Intcnt=Intcnt+Kntt1
      endif
      
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 260 m=jq,lq,Nwpi
      Ja=Ibuf2e(m)
      Ia(1)=Ibuf2e(m+1)
      Ia(2)=Ibuf2e(m+2)
      a00=Valint+Valint
      call unpck4
      Sindx=Sindx+1
      if(Sindx.EQ.1)goto 230
      if(Sindx.EQ.2)then
      
      iji=ij(P)
      ijj=ij(Q)
      j1=iji+Q
      j2=ijj+R
      j3=iji+R
      j4=ijj+Q
      s1=-One
      goto 240
      elseif(Sindx.EQ.3)then
      
      iji=ij(P)
      j1=iji+Q
      j2=ij(R)+Q
      j3=iji+R
      j4=ij(Q+1)
      s1=One
      goto 240
      elseif(Sindx.EQ.4)then
      
      iji=ij(P)
      j1=iji+Q
      j2=iji+P
      j3=ij(Q+1)
      if(Rhf)then
      
      a1c=Onept5*Da(j1)*Valint
      a1=Pt5*Valint
      else
      a0=Da(j1)
      b0=Db(j1)
      a0c=(a0+a0+b0)*Valint
      a1c=(a0+b0+b0)*Valint
      a1=Valint
      Fb(j1)=Fb(j1)+a0c
      Fb(j2)=Fb(j2)-Db(j3)*a1
      Fb(j3)=Fb(j3)-Db(j2)*a1
      endif
      Fa(j1)=Fa(j1)+a1c
      Fa(j2)=Fa(j2)-Da(j3)*a1
      Fa(j3)=Fa(j3)-Da(j2)*a1
      
      if(Cmp)then
      j1=j1+Mtt
      if(.NOT.Rhf)Fb(j1)=Fb(j1)-Db(j1)*a1
      Fa(j1)=Fa(j1)-Da(j1)*a1
      endif
      elseif(Sindx.EQ.5)then
      
      iji=ij(P)
      j1=iji+P
      j2=ij(Q)+R
      j3=iji+Q
      j4=iji+R
      if(Rhf)then
      
      a0c=Da(j2)*a00
      a1c=Da(j1)*Valint
      a1=Pt5*Valint
      else
      a0c=(Da(j2)+Db(j2))*a00
      a1c=(Da(j1)+Db(j1))*Valint
      a1=Valint
      Fb(j1)=Fb(j1)+a0c
      Fb(j2)=Fb(j2)+a1c
      Fb(j3)=Fb(j3)-Db(j4)*a1
      Fb(j4)=Fb(j4)-Db(j3)*a1
      endif
      Fa(j1)=Fa(j1)+a0c
      Fa(j2)=Fa(j2)+a1c
      Fa(j3)=Fa(j3)-Da(j4)*a1
      Fa(j4)=Fa(j4)-Da(j3)*a1
      
      if(Cmp)then
      j3=j3+Mtt
      j4=j4+Mtt
      if(.NOT.(Rhf))then
      Fb(j3)=Fb(j3)+Db(j4)*a1
      Fb(j4)=Fb(j4)+Db(j3)*a1
      endif
      Fa(j3)=Fa(j3)+Da(j4)*a1
      Fa(j4)=Fa(j4)+Da(j3)*a1
      endif
      elseif(Sindx.EQ.6)then
      
      iji=ij(P)
      j1=iji+Q
      j2=ij(R+1)
      j3=iji+R
      if(Q.LE.R)then
      
      j4=ij(R)+Q
      s1=One
      else
      j4=ij(Q)+R
      s1=-One
      endif
      if(Rhf)then
      
      a0c=Da(j2)*Valint
      a1c=Da(j1)*a00
      a1=Pt5*Valint
      else
      a0c=(Da(j2)+Db(j2))*Valint
      a1c=(Da(j1)+Db(j1))*a00
      a1=Valint
      Fb(j1)=Fb(j1)+a0c
      Fb(j2)=Fb(j2)+a1c
      Fb(j3)=Fb(j3)-Db(j4)*a1
      Fb(j4)=Fb(j4)-Db(j3)*a1
      endif
      Fa(j1)=Fa(j1)+a0c
      Fa(j2)=Fa(j2)+a1c
      Fa(j3)=Fa(j3)-Da(j4)*a1
      Fa(j4)=Fa(j4)-Da(j3)*a1
      
      if(Cmp)then
      j3=j3+Mtt
      j4=j4+Mtt
      a0=s1*a1
      if(.NOT.(Rhf))then
      Fb(j3)=Fb(j3)-Db(j4)*a0
      Fb(j4)=Fb(j4)-Db(j3)*a0
      endif
      Fa(j3)=Fa(j3)-Da(j4)*a0
      Fa(j4)=Fa(j4)-Da(j3)*a0
      endif
      elseif(Sindx.EQ.8)then
      
      j1=ij(P+1)
      if(.NOT.Rhf)then
      
      Fa(j1)=Fa(j1)+Db(j1)*Valint
      Fb(j1)=Fb(j1)+Da(j1)*Valint
      else
      Fa(j1)=Fa(j1)+Da(j1)*Pt5*Valint
      endif
      
      elseif(Mindx.EQ.1)then
      
      j1=ij(P)+Q
      j2=ij(Q+1)
      if(.NOT.Rhf)then
      
      Fa(j1)=Fa(j1)+Db(j2)*Valint
      Fa(j2)=Fa(j2)+Db(j1)*a00
      Fb(j1)=Fb(j1)+Da(j2)*Valint
      Fb(j2)=Fb(j2)+Da(j1)*a00
      else
      Fa(j1)=Fa(j1)+Da(j2)*Pt5*Valint
      Fa(j2)=Fa(j2)+Da(j1)*Valint
      endif
      elseif(Mindx.EQ.2)then
      
      iji=ij(P)
      j1=iji+P
      j2=ij(Q+1)
      j3=iji+Q
      if(Rhf)then
      
      a0c=Da(j2)*Valint
      a1c=Da(j1)*Valint
      a1=Pt5*Valint
      else
      a0c=(Da(j2)+Db(j2))*Valint
      a1c=(Da(j1)+Db(j1))*Valint
      a1=Valint
      Fb(j1)=Fb(j1)+a0c
      Fb(j2)=Fb(j2)+a1c
      Fb(j3)=Fb(j3)-Db(j3)*a1
      endif
      Fa(j1)=Fa(j1)+a0c
      Fa(j2)=Fa(j2)+a1c
      Fa(j3)=Fa(j3)-Da(j3)*a1
      
      if(Cmp)then
      j3=j3+Mtt
      if(.NOT.Rhf)Fb(j3)=Fb(j3)+Db(j3)*a1
      Fa(j3)=Fa(j3)+Da(j3)*a1
      endif
      elseif(Mindx.EQ.3)then
      
      j1=ij(P+1)
      j2=ij(P)+Q
      if(.NOT.Rhf)then
      
      Fa(j1)=Fa(j1)+Db(j2)*a00
      Fa(j2)=Fa(j2)+Db(j1)*Valint
      Fb(j1)=Fb(j1)+Da(j2)*a00
      Fb(j2)=Fb(j2)+Da(j1)*Valint
      else
      Fa(j1)=Fa(j1)+Da(j2)*Valint
      Fa(j2)=Fa(j2)+Da(j1)*Pt5*Valint
      endif
      else
      goto 230
      endif
      goto 260
      
230   iji=ij(P)
      ijj=ij(Q)
      j1=iji+Q
      j2=iji+R
      j3=iji+P
      if(Q.LE.R)then
      
      j4=ij(R)+Q
      else
      j4=ijj+R
      endif
      if(Rhf)then
      
      a0=Pt5*Valint
      a1=Valint
      a0c=Da(j2)*a00
      a1c=Da(j1)*a00
      else
      a0=Valint
      a1=a00
      a0c=(Da(j2)+Db(j2))*a00
      a1c=(Da(j1)+Db(j1))*a00
      Fb(j1)=Fb(j1)+a0c-Db(j2)*a0
      Fb(j2)=Fb(j2)+a1c-Db(j1)*a0
      Fb(j3)=Fb(j3)-Db(j4)*a1
      Fb(j4)=Fb(j4)-Db(j3)*a0
      endif
      Fa(j1)=Fa(j1)+a0c-Da(j2)*a0
      Fa(j2)=Fa(j2)+a1c-Da(j1)*a0
      Fa(j3)=Fa(j3)-Da(j4)*a1
      Fa(j4)=Fa(j4)-Da(j3)*a0
      
      if(Cmp)then
      j1=j1+Mtt
      j2=j2+Mtt
      if(.NOT.(Rhf))then
      Fb(j1)=Fb(j1)-Db(j2)*a0
      Fb(j2)=Fb(j2)-Db(j1)*a0
      endif
      Fa(j1)=Fa(j1)-Da(j2)*a0
      Fa(j2)=Fa(j2)-Da(j1)*a0
      endif
      goto 260
240   if(Rhf)then
      
      a0=Pt5*Valint
      a1=Valint
      a0c=Da(j2)*a00
      a1c=Da(j1)*a00
      else
      a0=Valint
      a1=a00
      a0c=(Da(j2)+Db(j2))*a00
      a1c=(Da(j1)+Db(j1))*a00
      Fb(j1)=Fb(j1)+a0c-Db(j2)*a0
      Fb(j2)=Fb(j2)+a1c-Db(j1)*a0
      Fb(j3)=Fb(j3)-Db(j4)*a0
      Fb(j4)=Fb(j4)-Db(j3)*a1
      endif
      Fa(j1)=Fa(j1)+a0c-Da(j2)*a0
      Fa(j2)=Fa(j2)+a1c-Da(j1)*a0
      Fa(j3)=Fa(j3)-Da(j4)*a0
      Fa(j4)=Fa(j4)-Da(j3)*a1
      
      if(Cmp)then
      j1=j1+Mtt
      j2=j2+Mtt
      a0=s1*a0
      if(.NOT.(Rhf))then
      Fb(j1)=Fb(j1)-Db(j2)*a0
      Fb(j2)=Fb(j2)-Db(j1)*a0
      endif
      Fa(j1)=Fa(j1)-Da(j2)*a0
      Fa(j2)=Fa(j2)-Da(j1)*a0
      endif
      
260   continue
      Intcnt=Intcnt+Kntt2
      endif
      endif
      goto 400
      
300   dcount=1
      if(Kntt1.GT.0)then
      if(.NOT.Rhf)then
      
      if(Cmp)call lnk1e
      do 320 m=1,Kntt1
      Ja=Ibuf2e(m+Ibase)
      call unpck2
      val1=Dbuf2e(dcount+Dbase)
      val2=Dbuf2e(dcount+1+Dbase)
      dcount=dcount+Ismode
      Fa(I)=Fa(I)+Da(J)*val1
      Fa(J)=Fa(J)+Da(I)*val1
      Fb(I)=Fb(I)+Db(J)*val2
      Fb(J)=Fb(J)+Db(I)*val2
320   continue
      Intcnt=Intcnt+Kntt1
      
      elseif(Cmp)then
      
      if(Ismode.NE.3)call lnk1e
      do 340 m=1,Kntt1
      Ja=Ibuf2e(m+Ibase)
      call unpck2
      val1=Dbuf2e(dcount+Dbase)
      val3=Dbuf2e(dcount+2+Dbase)
      dcount=dcount+Ismode
      Fa(I)=Fa(I)+Da(J)*val1
      Fa(J)=Fa(J)+Da(I)*val1
      Fa(I+Mtt)=Fa(I+Mtt)+Da(J+Mtt)*val3
      Fa(J+Mtt)=Fa(J+Mtt)+Da(I+Mtt)*val3
340   continue
      Intcnt=Intcnt+Kntt1
      else
      do 360 m=1,Kntt1
      Ja=Ibuf2e(m+Ibase)
      call unpck2
      Fa(I)=Fa(I)+Da(J)*Dbuf2e(dcount+Dbase)
      Fa(J)=Fa(J)+Da(I)*Dbuf2e(dcount+Dbase)
      dcount=dcount+Ismode
360   continue
      Intcnt=Intcnt+Kntt1
      endif
      endif
      
      
400   if(iflst.LE.0)goto 200
      call iwind(intape)
      if(Intcnt.NE.Itotal)then
      write(Iout,99001)Intcnt,Itotal
      call lnk1e
      endif
      
      
      
      if(Isym2e.NE.0)then
      call fsymm(NBASIS,Fa(1),NSYMOP,NEQBAS,ij,Da(1))
      if(Cmp)call fsymm(NBASIS,Fa(1+Mtt),NSYMOP,NEQBAS,ij,Da(1+Mtt))
      call amove(Ntt,Da(1),Fa(1))
      if(Cmp)call amove(Ntt,Da(1+Mtt),Fa(1+Mtt))
      if(.NOT.(Rhf))then
      call fsymm(NBASIS,Fb(1),NSYMOP,NEQBAS,ij,Db(1))
      if(Cmp)call fsymm(NBASIS,Fb(1+Mtt),NSYMOP,NEQBAS,ij,Db(1+Mtt))
      call amove(Ntt,Db(1),Fb(1))
      if(Cmp)call amove(Ntt,Db(1+Mtt),Fb(1+Mtt))
      endif
      endif
      
      if(Ismode.NE.0)then
      if(.NOT.Rhf)then
      call gabgst(NBASIS,Fa,Fb)
      else
      if(Cmp)call ascale(Ntt,pt25,Fa(1+Mtt),Fa(1+Mtt))
      endif
      endif
      
      call tread(IONE,Da,Ntt,1,Ntt,1,0)
      call aadd(Ntt,Da,Fa,Fa)
      if(.NOT.Rhf)call aadd(Ntt,Da,Fb,Fb)
      
      call tread(IOD(1),Da,Ntt,1,Ntt,1,0)
      if(Cmp)call tread(IOD(2),Da(1+Mtt),Ntt,1,Ntt,1,0)
      if(.NOT.(Rhf))then
      call tread(IOD(3),Db,Ntt,1,Ntt,1,0)
      if(Cmp)call tread(IOD(4),Db(1+Mtt),Ntt,1,Ntt,1,0)
      endif
      
      return
      
      end
C* :1 * 
      
