      SUBROUTINE ELFUN ( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, LTYPEE, LSTAEV, LELVAR, LNTVAR, 
     *                   LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, 
     *                   LEPVLU, IFFLAG, IFSTAT )
      INTEGER NCALCF, IFFLAG, LTYPEE, LSTAEV, LELVAR, LNTVAR
      INTEGER LSTADH, LSTEPA, LCALCF, LFVALU, LXVALU, LEPVLU
      INTEGER IFSTAT
      INTEGER ITYPEE(LTYPEE), ISTAEV(LSTAEV), IELVAR(LELVAR)
      INTEGER INTVAR(LNTVAR), ISTADH(LSTADH), ISTEPA(LSTEPA)
      INTEGER ICALCF(LCALCF)
      DOUBLE PRECISION FUVALS(LFVALU), XVALUE(LXVALU), EPVALU(LEPVLU)
C
C  Problem name : GRIDNETE  
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , A     , SQRA  , ASQRA 
      DOUBLE PRECISION ISQRA , B     , C     
      INTRINSIC SQRT  
      IFSTAT = 0
      DO     3 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2
     *                                                        ), IELTYP
C
C  Element type : SQ        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     3
C
C  Element type : SQR       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       A      = 1.0 + 2.0 * X * ( X - Y) + Y * Y         
       SQRA   = SQRT( A )                                
       ASQRA  = A * SQRA                                 
       ISQRA  = 1.0 / SQRA                               
       B      = X + X - Y                                
       C      = Y - X                                    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= SQRA                                     
       ELSE
        FUVALS(IGSTRT+     1)= B / SQRA                                 
        FUVALS(IGSTRT+     2)= C / SQRA                                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=- B * B / ASQRA + ISQRA + ISQRA          
         FUVALS(IHSTRT+     2)=- B * C / ASQRA - ISQRA                  
         FUVALS(IHSTRT+     3)=- C * C / ASQRA + ISQRA                  
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
