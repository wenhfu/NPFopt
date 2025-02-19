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
C  Problem name : YATP2SQ   
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX    , YPZ   , X     , Y     , Z     
      DOUBLE PRECISION C     , S     
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
C  Element type : ATP2      
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       XX     =   X     
       YPZ    =   Y     
     *          + Z     
       C      = COS( XX )                                
       S      = SIN( XX )                                
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= YPZ * C                                  
       ELSE
        FUVALS(IGSTRT+     2)= C                                        
        FUVALS(IGSTRT+     1)= - YPZ * S                                
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=- S                                      
         FUVALS(IHSTRT+     1)=- YPZ * C                                
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  Element type : SINX      
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       S      = SIN( X )                                 
       C      = COS( X )                                 
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= S                                        
       ELSE
        FUVALS(IGSTRT+     1)= C                                        
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=- S                                      
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
