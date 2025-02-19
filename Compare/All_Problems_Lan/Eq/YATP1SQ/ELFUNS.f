      SUBROUTINE ELFUNS( FUVALS, XVALUE, EPVALU, NCALCF, ITYPEE, 
     *                   ISTAEV, IELVAR, INTVAR, ISTADH, ISTEPA, 
     *                   ICALCF, IFFLAG )
      INTEGER NCALCF, IFFLAG
      INTEGER ITYPEE(*), ISTAEV(*), IELVAR(*), INTVAR(*)
      INTEGER ISTADH(*), ISTEPA(*), ICALCF(*)
      DOUBLE PRECISION FUVALS(*), XVALUE(*), EPVALU(*)
C
C  PROBLEM NAME : YATP1SQ 
C
      INTEGER IELEMN, IELTYP, IHSTRT, ILSTRT, IGSTRT, IPSTRT
      INTEGER JCALCF
      DOUBLE PRECISION X     , XX    , YPZ   , Y     , Z     
      DOUBLE PRECISION C     , S     
      DO     6 JCALCF = 1, NCALCF
       IELEMN = ICALCF(JCALCF) 
       ILSTRT = ISTAEV(IELEMN) - 1
       IGSTRT = INTVAR(IELEMN) - 1
       IPSTRT = ISTEPA(IELEMN) - 1
       IF ( IFFLAG .EQ. 3 ) IHSTRT = ISTADH(IELEMN) - 1
       IELTYP = ITYPEE(IELEMN)
       GO TO (    1,    2,    3,    4,    5
     *                                                        ), IELTYP
C
C  ELEMENT TYPE : SQ        
C
    1  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X                                    
       ELSE
        FUVALS(IGSTRT+     1)= X + X                                    
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=2.0                                      
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : CB        
C
    2  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= X * X * X                                
       ELSE
        FUVALS(IGSTRT+     1)= 3.0 * X * X                              
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=6.0 * X                                  
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : LXC       
C
    3  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       XX     =   X     
       YPZ    =   Y     
     *          + Z     
       C      = COS( XX )                                
       S      = SIN( XX )                                
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= YPZ * XX * C                             
       ELSE
        FUVALS(IGSTRT+     2)= XX * C                                   
        FUVALS(IGSTRT+     1)= YPZ * ( C - XX * S )                     
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=C - XX * S                               
         FUVALS(IHSTRT+     1)=- YPZ * ( S + S + XX * C )               
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : LS        
C
    4  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       Y      = XVALUE(IELVAR(ILSTRT+     2))
       Z      = XVALUE(IELVAR(ILSTRT+     3))
       XX     =   X     
       YPZ    =   Y     
     *          + Z     
       C      = COS( XX )                                
       S      = SIN( XX )                                
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= YPZ * S                                  
       ELSE
        FUVALS(IGSTRT+     2)= S                                        
        FUVALS(IGSTRT+     1)= YPZ * C                                  
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     2)=C                                        
         FUVALS(IHSTRT+     1)=- YPZ * S                                
         FUVALS(IHSTRT+     3)=0.0D+0
        END IF
       END IF
       GO TO     6
C
C  ELEMENT TYPE : RAT       
C
    5  CONTINUE
       X      = XVALUE(IELVAR(ILSTRT+     1))
       C      = COS( X )                                 
       S      = SIN( X )                                 
       IF ( IFFLAG .EQ. 1 ) THEN
        FUVALS(IELEMN)= S / X                                    
       ELSE
        FUVALS(IGSTRT+     1)= ( C - S / X ) / X                        
        IF ( IFFLAG .EQ. 3 ) THEN
         FUVALS(IHSTRT+     1)=- S/X - (C+C)/X**2 + (S+S)/X**3          
        END IF
       END IF
    6 CONTINUE
      RETURN
      END
