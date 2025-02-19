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
C  Problem name : HS88      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , X1    , X2    , F     , G(2)  
      DOUBLE PRECISION H(2,2), EVAL88
      EXTERNAL EVAL88
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
C  Element type : SQR       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0D+0                                   
        END IF
       END IF
       GO TO     3
C
C  Element type : H         
C
    2  CONTINUE
       X1     = XVALUE(IELVAR(ILSTRT+     1))
       X2     = XVALUE(IELVAR(ILSTRT+     2))
       F      = EVAL88( X1, X2,                          
     *                  G, H )                           
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= F                                        
       ELSE
        FUVALS(IGSTRT+     1)= G(1)                                     
        FUVALS(IGSTRT+     2)= G(2)                                     
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=H(1,1)                                   
         FUVALS(IHSTRT+     2)=H(1,2)                                   
         FUVALS(IHSTRT+     3)=H(2,2)                                   
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
