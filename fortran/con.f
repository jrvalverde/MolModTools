      SUBROUTINE CON(I2,Y,M2,D,U)
      INTEGER Y,M,D,U,DAT(24)
      DATA DAT/0,31,59,90,120,151,181,212,243,273,304,334,0,31,60,91,121
     1,152,182,213,244,274,305,335/
C     CORRECT FOR 400LEAP 100 NO 4LEAP 1NO
      IF(I2-1)2,2,3
    2 M=M2
      IF(MOD(Y,4).EQ.0.AND.MOD(Y,100).NE.0.OR.MOD(Y,400).EQ.0)M=M+12
      I=Y-1
      U=I/400
      I=I-U*400
      J=I/100
      I=I-J*100
      K=I/4
      I=I-K*4
      U=U*146097+J*36524+K*1461+I*365+DAT(M)+D-1
      RETURN
    3 D=U
      Y=D/146097
      D=D-Y*146097
      J=D/36524
      D=D-J*36524
      K=D/1461
      D=D-K*1461
      I=D/365
      D=D-I*365
      M=1
      Y=Y*400+J*100+K*4+I+1
      IF(MOD(Y,4).EQ.0.AND.MOD(Y,100).NE.0.OR.MOD(Y,400).EQ.0)M=13
      DO4L=1,12
      IF(DAT(M).GT.D)GOTO5
    4 M=M+1
    5 M=M-1
      D=D-DAT(M)+1
      IF(M.GT.12)M=M-12
      M2=M
      RETURN
      END
**END
*ENDRUN
*             EX    *PROGDEV,
*      E       FUNCT EXECUTABLE,PROG=*BIORY,ACTION=UPD,
