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
C  Problem name : DRCAVTY2  
C
C  -- produced by SIFdecode 1.0
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION AA    , BB    , A1    , A2    , B1    
      DOUBLE PRECISION B2    , B3    , B4    , B5    , B6    
      DOUBLE PRECISION B7    , B8    
      IFSTAT = 0
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG == 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  Element type : IPR       
C
       A1     = XVALUE(IELVAR(ILSTRT+     1))
       A2     = XVALUE(IELVAR(ILSTRT+     2))
       B1     = XVALUE(IELVAR(ILSTRT+     3))
       B2     = XVALUE(IELVAR(ILSTRT+     4))
       B3     = XVALUE(IELVAR(ILSTRT+     5))
       B4     = XVALUE(IELVAR(ILSTRT+     6))
       B5     = XVALUE(IELVAR(ILSTRT+     7))
       B6     = XVALUE(IELVAR(ILSTRT+     8))
       B7     = XVALUE(IELVAR(ILSTRT+     9))
       B8     = XVALUE(IELVAR(ILSTRT+    10))
       AA     =   A1    
     *          - A2    
       BB     =   B1    
     *          + B2    
     *          + B3    
     *          - B4     *      4.00000
     *          + B5     *      4.00000
     *          - B6    
     *          - B7    
     *          - B8    
       IF ( IFFLAG == 1 ) THEN
        FUVALS(IELEMN)= AA * BB                                  
       ELSE
        FUVALS(IGSTRT+     1)= BB                                       
        FUVALS(IGSTRT+     2)= AA                                       
        IF ( IFFLAG == 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
