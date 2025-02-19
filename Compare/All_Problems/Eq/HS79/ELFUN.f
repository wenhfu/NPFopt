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
C  Problem name : HS79      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , U     , Y     , P     , ARG   
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
C  Element type : SQ        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       P      = EPVALU(IPSTRT+     1)
       ARG    = X + P                                    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= ARG**2                                   
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * ARG                                
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  Element type : DSQ       
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       U      =   X     
     *          - Y     
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= U**2                                     
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * U                                  
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  Element type : DQU       
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       U      =   X     
     *          - Y     
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= U**4                                     
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * U**3                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * U**2                              
        END IF
       END IF
       GO TO     6
C
C  Element type : CB        
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X**3                                     
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * X**2                               
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * X                                  
        END IF
       END IF
       GO TO     6
C
C  Element type : 2PR       
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * Y                                    
       ELSE
        FUVALS(IGSTRT+     1)= Y                                        
        FUVALS(IGSTRT+     2)= X                                        
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    6 CONTINUE
      RETURN
      END
