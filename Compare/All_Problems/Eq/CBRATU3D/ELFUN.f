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
C  Problem name : CBRATU3D  
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U     , V     , EXPU  , EXPUS , EXPUC 
      INTRINSIC EXP   , COS   , SIN   
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
C  Element type : RPART     
C
    1  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       V      = XVALUE(IELVAR(ILSTRT+     2))
       EXPU   = EXP( U )                                 
       EXPUC  = EXPU * COS( V )                          
       EXPUS  = EXPU * SIN( V )                          
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= EXPUC                                    
       ELSE
        FUVALS(IGSTRT+     1)= EXPUC                                    
        FUVALS(IGSTRT+     2)= - EXPUS                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=EXPUC                                    
         FUVALS(IHSTRT+     2)=- EXPUS                                  
         FUVALS(IHSTRT+     3)=- EXPUC                                  
        END IF
       END IF
       GO TO     3
C
C  Element type : CPART     
C
    2  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       V      = XVALUE(IELVAR(ILSTRT+     2))
       EXPU   = EXP( U )                                 
       EXPUC  = EXPU * COS( V )                          
       EXPUS  = EXPU * SIN( V )                          
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= EXPUS                                    
       ELSE
        FUVALS(IGSTRT+     1)= EXPUS                                    
        FUVALS(IGSTRT+     2)= EXPUC                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=EXPUS                                    
         FUVALS(IHSTRT+     2)=EXPUC                                    
         FUVALS(IHSTRT+     3)=- EXPUS                                  
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
