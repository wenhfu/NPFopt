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
C  Problem name : GOTTFR    
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , Y     , FA    , FB    
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
C  Element type : ET1       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       FA     = X + 3.0 * Y                              
       FB     = 1.0 - X                                  
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= -0.1136 * FA * FB                        
       ELSE
        FUVALS(IGSTRT+     1)= -0.1136 * ( FB - FA )                    
        FUVALS(IGSTRT+     2)= -0.3408 * FB                             
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=0.2272                                   
         FUVALS(IHSTRT+     2)=0.3408                                   
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  Element type : ET2       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       FA     = 2.0 * X - Y                              
       FB     = 1.0 - Y                                  
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= 7.5 * FA * FB                            
       ELSE
        FUVALS(IGSTRT+     1)= 15.0 * FB                                
        FUVALS(IGSTRT+     2)= - 7.5 * ( FB + FA )                      
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=-15.0                                    
         FUVALS(IHSTRT+     3)=15.0                                     
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
