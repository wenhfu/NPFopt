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
C  Problem name : CHAIN     
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , U     , ONEPU2, S1PU2 
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
C  Element type : XSU       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       U      = XVALUE(IELVAR(ILSTRT+     2))
       ONEPU2 = 1.0 + U * U                              
       S1PU2  = SQRT( ONEPU2 )                           
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * S1PU2                                
       ELSE
        FUVALS(IGSTRT+     1)= S1PU2                                    
        FUVALS(IGSTRT+     2)= X * U / S1PU2                            
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=U / S1PU2                                
         FUVALS(IHSTRT+     3)=X / S1PU2 - X * U * U / S1PU2 ** 3       
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  Element type : SU        
C
    2  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       ONEPU2 = 1.0 + U * U                              
       S1PU2  = SQRT( ONEPU2 )                           
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= S1PU2                                    
       ELSE
        FUVALS(IGSTRT+     1)= U / S1PU2                                
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=1.0 / S1PU2 - U * U / S1PU2 ** 3         
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
