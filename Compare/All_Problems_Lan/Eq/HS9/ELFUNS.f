      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : HS9     
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V1    , V2    , PI    
      DO     2 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
C
C  ELEMENT TYPE : SNCS      
C
       V1     = XVALUE(IELVAR(ILSTRT+     1))
       V2     = XVALUE(IELVAR(ILSTRT+     2))
       PI     = 4.0*ATAN(1.0)                            
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= SIN(PI*V1/12.0)*COS(PI*V2/16.0)          
       ELSE
        FUVALS(IGSTRT+     1)= COS(PI*V1/12.0)*COS(PI*V2/16.0)*PI/12.0  
        FUVALS(IGSTRT+     2)= -SIN(PI*V1/12.0)*SIN(PI*V2/16.0)*PI/16.0 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=-SIN(PI*V1/12.0)*COS(PI*V2/16.0)*        
     *                         PI*PI/144.0                              
         FUVALS(IHSTRT+     3)=-SIN(PI*V1/12.0)*COS(PI*V2/16.0)*        
     *                         PI*PI/256.0                              
         FUVALS(IHSTRT+     2)=-COS(PI*V1/12.0)*SIN(PI*V2/16.0)*        
     *                         PI*PI/192.0                              
        END IF
       END IF
    2 CONTINUE
      RETURN
      END
