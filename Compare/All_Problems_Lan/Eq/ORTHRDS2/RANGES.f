      SUBROUTINE RANGES( IELEMN, TRANSP, W1, W2, NELVAR, NINVAR )
      INTEGER IELEMN, NELVAR, NINVAR
      LOGICAL TRANSP
      DOUBLE PRECISION W1( * ), W2( * )
C
C  PROBLEM NAME : ORTHRDS2
C
C  TRANSP = .FALSE. <=> W2 = U * W1
C  TRANSP = .TRUE.  <=> W2 = U(TRANSPOSE) * W1
C
      INTEGER I, ITYPE
      INTEGER ITYPEE(    200 )
      COMMON / ELTYPE / ITYPEE 
      ITYPE = ITYPEE( IELEMN )
      GO TO (    1,    2
     *                                                        ), ITYPE
C
C  ELEMENT TYPE : TA        
C
    1 CONTINUE
      IF ( TRANSP ) THEN
         W2(     1 ) =   W1(     1 ) 
         W2(     2 ) =   W1(     2 ) 
         W2(     3 ) = - W1(     1 ) 
         W2(     4 ) = - W1(     2 ) 
      ELSE
         W2(     1 ) =   W1(     1 ) 
     *                 - W1(     3 ) 
         W2(     2 ) =   W1(     2 ) 
     *                 - W1(     4 ) 
      END IF
      RETURN
C
C  ELEMENT TYPE : TB        
C
    2 CONTINUE
      IF ( TRANSP ) THEN
         W2(     1 ) =   W1(     1 ) 
         W2(     2 ) =   W1(     2 ) 
         W2(     3 ) = - W1(     1 ) 
         W2(     4 ) = - W1(     2 ) 
         W2(     5 ) =   W1(     3 ) 
      ELSE
         W2(     1 ) =   W1(     1 ) 
     *                 - W1(     3 ) 
         W2(     2 ) =   W1(     2 ) 
     *                 - W1(     4 ) 
         W2(     3 ) =   W1(     5 ) 
      END IF
      RETURN
      END
