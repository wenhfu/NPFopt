      SUBROUTINE RANGES( IELEMN, TRANSP, W1, W2, NELVAR, NINVAR )
      INTEGER IELEMN, NELVAR, NINVAR
      LOGICAL TRANSP
      DOUBLE PRECISION W1( * ), W2( * )
C
C  PROBLEM NAME : HS51    
C
C  TRANSP = .FALSE. <=> W2 = U * W1
C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1
C
      INTEGER I, ITYPE
      INTEGER ITYPEE(      4 )
      COMMON / ELTYPE / ITYPEE 
      ITYPE = ITYPEE( IELEMN )
      GO TO (    1,    2,99998
     *                                                        ), ITYPE
C
C  ELEMENT TYPE : SQ1M2     
C
    1 CONTINUE
      IF ( TRANSP ) THEN
         W2(     1 ) =   W1(     1 ) 
         W2(     2 ) = - W1(     1 ) 
      ELSE
         W2(     1 ) =   W1(     1 ) 
     *                 - W1(     2 ) 
      END IF
      RETURN
C
C  ELEMENT TYPE : SQ1P2     
C
    2 CONTINUE
      IF ( TRANSP ) THEN
         W2(     1 ) =   W1(     1 ) 
         W2(     2 ) =   W1(     1 ) 
      ELSE
         W2(     1 ) =   W1(     1 ) 
     *                 + W1(     2 ) 
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
