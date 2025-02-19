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
C  Problem name : CHACONN1  
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V     , Z     , W     , T     
      INTRINSIC EXP   
      IFSTAT = 0
      DO     5 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4
     *                                                        ), IELTYP
C
C  Element type : SQ        
C
    1  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V * V                                    
       ELSE
        FUVALS(IGSTRT+     1)= V + V                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  Element type : TWOMSQ    
C
    4  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       T      = 2.0 - V                                  
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= T * T                                    
       ELSE
        FUVALS(IGSTRT+     1)= -2.0 * T                                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  Element type : FR        
C
    2  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V ** 4                                   
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * V ** 3                             
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V ** 2                            
        END IF
       END IF
       GO TO     5
C
C  Element type : EX        
C
    3  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       W      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = - V     
     *          + W     
       T      = EXP( Z )                                 
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= T                                        
       ELSE
        FUVALS(IGSTRT+     1)= T                                        
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=T                                        
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
