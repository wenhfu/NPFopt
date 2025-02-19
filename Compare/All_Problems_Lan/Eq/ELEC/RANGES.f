      SUBROUTINE RANGES( IELEMN, TRANSP, W1, W2, NELVAR, NINVAR )
      INTEGER IELEMN, NELVAR, NINVAR
      LOGICAL TRANSP
      DOUBLE PRECISION W1( * ), W2( * )
C
C  PROBLEM NAME : ELEC    
C
C  TRANSP = .FALSE. <=> W2 = U * W1
C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1
C
      INTEGER I, ITYPE
      INTEGER ITYPEE(   5250 )
      COMMON / ELTYPE / ITYPEE 
      ITYPE = ITYPEE( IELEMN )
      GO TO (    1,99998
     *                                                        ), ITYPE
C
C  ELEMENT TYPE : PE        
C
    1 CONTINUE
      IF ( TRANSP ) THEN
         W2(     1 ) =   W1(     1 ) 
         W2(     2 ) = - W1(     1 ) 
         W2(     3 ) =   W1(     2 ) 
         W2(     4 ) = - W1(     2 ) 
         W2(     5 ) =   W1(     3 ) 
         W2(     6 ) = - W1(     3 ) 
      ELSE
         W2(     1 ) =   W1(     1 ) 
     *                 - W1(     2 ) 
         W2(     2 ) =   W1(     3 ) 
     *                 - W1(     4 ) 
         W2(     3 ) =   W1(     5 ) 
     *                 - W1(     6 ) 
      END IF
      RETURN
C
C  ELEMENTS WITHOUT INTERNAL VARIABLES.
C
99998 CONTINUE
      DO 99999 I = 1, NELVAR
         W2( I ) = W1( I )
99999 CONTINUE
      RETURN
      END
