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
C  Problem name : POLAK3    
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION XX    , A     , B     , V     , EV    
      INTRINSIC EXP   
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : EL        
C
       XX     = XVALUE(IELVAR(ILSTRT+     1))
       A      = EPVALU(IPSTRT+     1)
       B      = EPVALU(IPSTRT+     2)
       V      = XX - SIN( A + B + B )                    
       EV     = EXP( V * V )                             
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= EV                                       
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * V * EV                             
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     1)=( 4.0 * V * V  + 2.0 ) * EV              
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
