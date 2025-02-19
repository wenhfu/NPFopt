      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : DECONVNE
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , IDX   , SCAL  
      LOGICAL NEGIDX
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : PR        
C
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IDX    = EPVALU(IPSTRT+     1)
       NEGIDX = IDX.LE.0.0                               
       IF (NEGIDX) SCAL   = 0.0                                      
       IF (.NOT.NEGIDX) SCAL  =1.0                                      
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= SCAL * X * Y                             
       ELSE
        FUVALS(IGSTRT+     1)= SCAL * Y                                 
        FUVALS(IGSTRT+     2)= SCAL * X                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=SCAL                                     
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
