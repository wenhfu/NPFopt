      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : MINMAXBD
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , CV2   , CIND  , A     
      INTRINSIC EXP   , SIN   , COS   
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : BRD       
C
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       CV2    = EPVALU(IPSTRT+     1)
       CIND   = EPVALU(IPSTRT+     2)
       A      = V1 + CV2 * V2 - CIND                     
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= A * A                                    
       ELSE
        FUVALS(IGSTRT+     1)= 2.0 * A                                  
        FUVALS(IGSTRT+     2)= 2.0 * A * CV2                            
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
         FUVALS(IHSTRT+     2)=2.0 * CV2                                
         FUVALS(IHSTRT+     3)=2.0 * CV2 * CV2                          
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
