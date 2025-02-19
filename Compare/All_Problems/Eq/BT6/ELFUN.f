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
C  Problem name : BT6       
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Z     , Y     , P     , T     
      DOUBLE PRECISION SZ    
      INTRINSIC SIN   , COS   
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
       X      = XVALUE(IELVAR(ILSTRT+     1))
       P      = EPVALU(IPSTRT+     1)
       T      = X - 1.0                                  
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= T ** P                                   
       ELSE
        FUVALS(IGSTRT+     1)= P * T ** ( P - 1.0 )                     
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=P * ( P - 1.0 ) * T ** ( P - 2.0 )       
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
C  Element type : ISIN      
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      =   X     
     *          - Y     
       SZ     = SIN( Z )                                 
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= SZ                                       
       ELSE
        FUVALS(IGSTRT+     1)= COS( Z )                                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=-SZ                                      
        END IF
       END IF
       GO TO     6
C
C  Element type : XY2       
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * Y * Y                                
       ELSE
        FUVALS(IGSTRT+     1)= Y * Y                                    
        FUVALS(IGSTRT+     2)= 2.0 * X * Y                              
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=Y + Y                                    
         FUVALS(IHSTRT+     3)=X + X                                    
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
       GO TO     6
C
C  Element type : X4Y2      
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X**4 * Y**2                              
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * X**3 * Y**2                        
        FUVALS(IGSTRT+     2)= 2.0 * X**4 * Y                           
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * X**2 * Y**2                       
         FUVALS(IHSTRT+     2)=8.0 * X**3 * Y                           
         FUVALS(IHSTRT+     3)=2.0 * X**4                               
        END IF
       END IF
    6 CONTINUE
      RETURN
      END
