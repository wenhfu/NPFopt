      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : CBRATU2D
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION U     , V     , EXPU  , EXPUS , EXPUC 
      INTRINSIC EXP   , COS   , SIN   
      DO     3 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : RPART     
C
    1  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       V      = XVALUE(IELVAR(ILSTRT+     2))
       EXPU   = EXP( U )                                 
       EXPUC  = EXPU * COS( V )                          
       EXPUS  = EXPU * SIN( V )                          
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EXPUC                                    
       ELSE
        FUVALS(IGSTRT+     1)= EXPUC                                    
        FUVALS(IGSTRT+     2)= - EXPUS                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=EXPUC                                    
         FUVALS(IHSTRT+     2)=- EXPUS                                  
         FUVALS(IHSTRT+     3)=- EXPUC                                  
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : CPART     
C
    2  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       V      = XVALUE(IELVAR(ILSTRT+     2))
       EXPU   = EXP( U )                                 
       EXPUC  = EXPU * COS( V )                          
       EXPUS  = EXPU * SIN( V )                          
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= EXPUS                                    
       ELSE
        FUVALS(IGSTRT+     1)= EXPUS                                    
        FUVALS(IGSTRT+     2)= EXPUC                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=EXPUS                                    
         FUVALS(IHSTRT+     2)=EXPUC                                    
         FUVALS(IHSTRT+     3)=- EXPUS                                  
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
