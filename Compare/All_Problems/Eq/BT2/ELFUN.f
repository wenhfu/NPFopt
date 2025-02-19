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
C  Problem name : BT2       
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V     , Z     , X     , Y     , P     
      DOUBLE PRECISION T     
      IFSTAT = 0
      DO     6 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5
     *                                                        ), IELTYP
C
C  Element type : SSQ       
C
    1  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       P      = EPVALU(IPSTRT+     1)
       T      = V - P                                    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= T * T                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * T                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  Element type : ISQ       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      =   X     
     *          - Y     
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= Z * Z                                    
       ELSE
        FUVALS(IGSTRT+     1)= Z + Z                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  Element type : FOURTH    
C
    4  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= V**4                                     
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * V**3                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V**2                              
        END IF
       END IF
       GO TO     6
C
C  Element type : I4TH      
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      =   X     
     *          - Y     
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= Z**4                                     
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * Z**3                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * Z**2                              
        END IF
       END IF
       GO TO     6
C
C  Element type : MISC      
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * ( 1.0 + Y * Y )                      
       ELSE
        FUVALS(IGSTRT+     1)= 1.0 + Y * Y                              
        FUVALS(IGSTRT+     2)= 2.0 * X * Y                              
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=2.0 * Y                                  
         FUVALS(IHSTRT+     3)=2.0 * X                                  
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
    6 CONTINUE
      RETURN
      END
