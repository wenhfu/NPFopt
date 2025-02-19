      SUBROUTINE RANGES( IELEMN, TRANSP, W1, W2, NELVAR, NINVAR )
      INTEGER IELEMN, NELVAR, NINVAR
      LOGICAL TRANSP
      DOUBLE PRECISION W1( * ), W2( * )
C
C  PROBLEM NAME : DRCAVTY3
C
C  TRANSP = .FALSE. <=> W2 = U * W1
C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1
C
      INTEGER I, ITYPE
C
C  ELEMENT TYPE : IPR       
C
    1 CONTINUE
      IF ( TRANSP ) THEN
         W2(     1 ) =   W1(     1 ) 
         W2(     2 ) = - W1(     1 ) 
         W2(     3 ) =   W1(     2 ) 
         W2(     4 ) =   W1(     2 ) 
         W2(     5 ) =   W1(     2 ) 
         W2(     6 ) = - W1(     2 ) *      4.00000
         W2(     7 ) =   W1(     2 ) *      4.00000
         W2(     8 ) = - W1(     2 ) 
         W2(     9 ) = - W1(     2 ) 
         W2(    10 ) = - W1(     2 ) 
      ELSE
         W2(     1 ) =   W1(     1 ) 
     *                 - W1(     2 ) 
         W2(     2 ) =   W1(     3 ) 
     *                 + W1(     4 ) 
     *                 + W1(     5 ) 
     *                 - W1(     6 ) *      4.00000
     *                 + W1(     7 ) *      4.00000
     *                 - W1(     8 ) 
     *                 - W1(     9 ) 
     *                 - W1(    10 ) 
      END IF
      RETURN
      END
