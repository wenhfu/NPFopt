      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : CHACONN2
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION V     , Z     , W     , T     
      INTRINSIC EXP   
      DO     5 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V * V                                    
       ELSE
        FUVALS(IGSTRT+     1)= V + V                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : TWOMSQ    
C
    4  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       T      = 2.0 - V                                  
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= T * T                                    
       ELSE
        FUVALS(IGSTRT+     1)= -2.0 * T                                 
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : FR        
C
    2  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= V ** 4                                   
       ELSE
        FUVALS(IGSTRT+     1)= 4.0 * V ** 3                             
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=12.0 * V ** 2                            
        END IF
       END IF
       GO TO     5
C
C  ELEMENT TYPE : EX        
C
    3  CONTINUE
       V      = XVALUE(IELVAR(ILSTRT+     1))
       W      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = - V     
     *          + W     
       T      = EXP( Z )                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= T                                        
       ELSE
        FUVALS(IGSTRT+     1)= T                                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=T                                        
        END IF
       END IF
    5 CONTINUE
      RETURN
      END
