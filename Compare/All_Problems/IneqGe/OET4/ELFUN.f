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
C  Problem name : OET4      
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X1    , X2    , X3    , W     , NUMER 
      DOUBLE PRECISION DENOM 
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : RATIO     
C
       X1     = XVALUE(IELVAR(ILSTRT+     1))
       X2     = XVALUE(IELVAR(ILSTRT+     2))
       X3     = XVALUE(IELVAR(ILSTRT+     3))
       W      = EPVALU(IPSTRT+     1)
       NUMER  = X1 + X2 * W                              
       DENOM  = 1.0 + X3 * W                             
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= NUMER / DENOM                            
       ELSE
        FUVALS(IGSTRT+     1)= 1.0 / DENOM                              
        FUVALS(IGSTRT+     2)= W / DENOM                                
        FUVALS(IGSTRT+     3)= - NUMER * W / DENOM ** 2                 
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     4)=- W / DENOM ** 2                         
         FUVALS(IHSTRT+     5)=- W * W / DENOM ** 2                     
         FUVALS(IHSTRT+     6)=2.0 * NUMER * W * W / DENOM ** 3         
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     2)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
