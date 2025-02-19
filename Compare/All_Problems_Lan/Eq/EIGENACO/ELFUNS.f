      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : EIGENACO
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION Q1    , Q2    , D     
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
C  ELEMENT TYPE : 2PROD     
C
    1  CONTINUE
       Q1     = XVALUE(IELVAR(ILSTRT+     1))
       Q2     = XVALUE(IELVAR(ILSTRT+     2))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= Q1 * Q2                                  
       ELSE
        FUVALS(IGSTRT+     1)=      Q2                                  
        FUVALS(IGSTRT+     2)= Q1                                       
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=1.0D+0                                   
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : 3PROD     
C
    2  CONTINUE
       Q1     = XVALUE(IELVAR(ILSTRT+     1))
       Q2     = XVALUE(IELVAR(ILSTRT+     2))
       D      = XVALUE(IELVAR(ILSTRT+     3))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= Q1 * Q2 * D                              
       ELSE
        FUVALS(IGSTRT+     1)=      Q2 * D                              
        FUVALS(IGSTRT+     2)= Q1      * D                              
        FUVALS(IGSTRT+     3)= Q1 * Q2                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=          D                              
         FUVALS(IHSTRT+     4)=     Q2                                  
         FUVALS(IHSTRT+     5)=Q1                                       
         FUVALS(IHSTRT+     1)=0.0D+0
         FUVALS(IHSTRT+     3)=0.0D+0
         FUVALS(IHSTRT+     6)=0.0D+0
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
