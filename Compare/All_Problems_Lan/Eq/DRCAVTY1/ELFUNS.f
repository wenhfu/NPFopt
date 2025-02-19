      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : DRCAVTY1
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION AA    , BB    , A1    , A2    , B1    
      DOUBLE PRECISION B2    , B3    , B4    , B5    , B6    
      DOUBLE PRECISION B7    , B8    
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : IPR       
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
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= AA * BB                                  
       ELSE
        FUVALS(IGSTRT+     1)= BB                                       
        FUVALS(IGSTRT+     2)= AA                                       
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0                                      
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
