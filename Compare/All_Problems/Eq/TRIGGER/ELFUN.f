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
C  Problem name : TRIGGER   
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , U     , XX1   , XX3   , EXA   
      DOUBLE PRECISION B1    , B2    , BSQ   , BU2   
      INTRINSIC EXP   , ATAN  
      IFSTAT = 0
      B1     = 5.6D-8                                   
      B2     = 1962.0                                   
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
C  Element type : DIODE     
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       EXA    = B1 * EXP( 25.0 * X )                     
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= EXA - B1                                 
       ELSE
        FUVALS(IGSTRT+     1)= 25.0 * EXA                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=625.0 * EXA                              
        END IF
       END IF
       GO TO     3
C
C  Element type : OPAMP     
C
    2  CONTINUE
       XX1    = XVALUE(IELVAR(ILSTRT+     1))
       XX3    = XVALUE(IELVAR(ILSTRT+     2))
       U      = - XX1   
     *          + XX3   
       BSQ    = B2 * B2                                  
       BU2    = 1.0 + BSQ * U * U                        
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= 7.65 * ATAN( B2 * U )                    
       ELSE
        FUVALS(IGSTRT+     1)= 7.65 * B2 / BU2                          
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=-15.3 * BSQ * B2 * U / ( BU2 * BU2 )     
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
