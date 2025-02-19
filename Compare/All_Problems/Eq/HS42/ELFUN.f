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
C  Problem name : HS42      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , P     , VMP   
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
C  Element type : SQMP      
C
    2  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       P      = EPVALU(IPSTRT+     1)
       VMP    = V1 - P                                   
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= VMP * VMP                                
       ELSE
        FUVALS(IGSTRT+     1)= VMP + VMP                                
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     3
C
C  Element type : SQ        
C
    1  CONTINUE
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V1 * V1                                  
       ELSE
        FUVALS(IGSTRT+     1)= V1 + V1                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
