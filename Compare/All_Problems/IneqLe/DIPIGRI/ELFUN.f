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
C  Problem name : DIPIGRI   
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , S     
      IFSTAT = 0
      DO     7 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5,    6
     *                                                        ), IELTYP
C
C  Element type : SSQ       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       S      = EPVALU(IPSTRT+     1)
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= ( X - S )**2                             
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * ( X - S )                          
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  Element type : SQ        
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     7
C
C  Element type : P4        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X**4                                     
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * X**3                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * X**2                              
        END IF
       END IF
       GO TO     7
C
C  Element type : P6        
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X**6                                     
       ELSE
        FUVALS(IGSTRT+     1)= 6.0 * X**5                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=30.0 * X**4                              
        END IF
       END IF
       GO TO     7
C
C  Element type : OE        
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= 7.0 * X**2 + Y**4 - 4.0 * X * Y          
       ELSE
        FUVALS(IGSTRT+     1)= 14.0 * X - 4.0 * Y                       
        FUVALS(IGSTRT+     2)= 4.0 * Y**3 - 4.0 * X                     
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=14.0                                     
         FUVALS(IHSTRT+     2)=-4.0                                     
         FUVALS(IHSTRT+     3)=12.0 * Y**2                              
        END IF
       END IF
       GO TO     7
C
C  Element type : CE        
C
    6  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= 4.0 * X**2 + Y**2 - 3.0 * X * Y          
       ELSE
        FUVALS(IGSTRT+     1)= 8.0 * X - 3.0 * Y                        
        FUVALS(IGSTRT+     2)= 2.0 * Y - 3.0 * X                        
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=8.0                                      
         FUVALS(IHSTRT+     2)=-3.0                                     
         FUVALS(IHSTRT+     3)=2.0                                      
        END IF
       END IF
    7 CONTINUE
      RETURN
      END
