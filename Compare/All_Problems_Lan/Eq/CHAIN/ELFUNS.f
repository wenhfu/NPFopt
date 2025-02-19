      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : CHAIN   
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , U     , ONEPU2, S1PU2 
      INTRINSIC SQRT  
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
C  ELEMENT TYPE : XSU       
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       U      = XVALUE(IELVAR(ILSTRT+     2))
       ONEPU2 = 1.0 + U * U                              
       S1PU2  = SQRT( ONEPU2 )                           
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * S1PU2                                
       ELSE
        FUVALS(IGSTRT+     1)= S1PU2                                    
        FUVALS(IGSTRT+     2)= X * U / S1PU2                            
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=U / S1PU2                                
         FUVALS(IHSTRT+     3)=X / S1PU2 - X * U * U / S1PU2 ** 3       
         FUVALS(IHSTRT+     1)=0.0D+0
        END IF
       END IF
       GO TO     3
C
C  ELEMENT TYPE : SU        
C
    2  CONTINUE
       U      = XVALUE(IELVAR(ILSTRT+     1))
       ONEPU2 = 1.0 + U * U                              
       S1PU2  = SQRT( ONEPU2 )                           
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= S1PU2                                    
       ELSE
        FUVALS(IGSTRT+     1)= U / S1PU2                                
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=1.0 / S1PU2 - U * U / S1PU2 ** 3         
        END IF
       END IF
    3 CONTINUE
      RETURN
      END
