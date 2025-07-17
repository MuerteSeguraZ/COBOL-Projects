PROGRAM MICRODB
  IMPLICIT NONE

  INTEGER, PARAMETER :: MAXREC = 10
  CHARACTER(len=20) :: NAMES(MAXREC), NAMES_SHADOW(MAXREC)
  INTEGER :: IDS(MAXREC), IDS_SHADOW(MAXREC)
  INTEGER :: AGES(MAXREC), AGES_SHADOW(MAXREC)
  INTEGER :: COUNT, COUNT_SHADOW
  INTEGER :: IN_TRANSACTION

  CHARACTER(len=100) :: INPUT_LINE
  CHARACTER(len=20) :: CMD, ARG1, ARG2, ARG3
  INTEGER :: CHOICE, IDX, I

  COMMON /USERS/ NAMES, IDS, AGES, COUNT
  COMMON /SHADOW/ NAMES_SHADOW, IDS_SHADOW, AGES_SHADOW, COUNT_SHADOW

  DATA IN_TRANSACTION /0/
  COUNT = 0
  COUNT_SHADOW = 0

  PRINT *, 'Welcome to MICRODB! Type HELP for commands.'

10 CONTINUE
  PRINT *, 'Enter command (INSERT, SELECT, DELETE, UPDATE, COUNT, CLEAR, BEGIN, COMMIT, ROLLBACK, HELP, EXIT):'
  READ(*,'(A)') INPUT_LINE
  CALL PARSE_INPUT(INPUT_LINE, CMD, ARG1, ARG2, ARG3)

  CALL TOUPPER(CMD)
  CALL TOUPPER(ARG1)
  CALL TOUPPER(ARG2)
  CALL TOUPPER(ARG3)

  CHOICE = 0
  IF (CMD .EQ. 'INSERT') CHOICE = 1
  IF (CMD .EQ. 'SELECT') CHOICE = 2
  IF (CMD .EQ. 'DELETE') CHOICE = 3
  IF (CMD .EQ. 'BEGIN') CHOICE = 4
  IF (CMD .EQ. 'COMMIT') CHOICE = 5
  IF (CMD .EQ. 'ROLLBACK') CHOICE = 6
  IF (CMD .EQ. 'EXIT') CHOICE = 7
  IF (CMD .EQ. 'UPDATE') CHOICE = 8
  IF (CMD .EQ. 'COUNT') CHOICE = 9
  IF (CMD .EQ. 'CLEAR') CHOICE = 10
  IF (CMD .EQ. 'HELP') CHOICE = 11

  IF (CHOICE .EQ. 0) THEN
    PRINT *, 'Unknown command.'
    GOTO 10
  ENDIF

  SELECT CASE (CHOICE)
  CASE (1) ! INSERT
    IF (COUNT .GE. MAXREC) THEN
      PRINT *, 'Error: Database full!'
      GOTO 10
    ENDIF
    READ(ARG1,'(I5)',ERR=999,END=999) IDX
    COUNT = COUNT + 1
    IDS(COUNT) = IDX
    NAMES(COUNT) = ARG2
    READ(ARG3,'(I5)',ERR=999,END=999) AGES(COUNT)
    GOTO 10
999 CONTINUE
    AGES(COUNT) = 0
    GOTO 10

  CASE (2) ! SELECT
    IF (ARG1 .EQ. 'ALL') THEN
      DO I = 1, COUNT
        PRINT *, IDS(I), ':', TRIM(NAMES(I)), AGES(I)
      ENDDO
      GOTO 10
    ELSE IF (ARG1 .EQ. 'ID') THEN
      READ(ARG2,'(I5)',ERR=998,END=998) IDX
      DO I = 1, COUNT
        IF (IDS(I) .EQ. IDX) THEN
          PRINT *, IDS(I), ':', TRIM(NAMES(I)), AGES(I)
          GOTO 10
        ENDIF
      ENDDO
      PRINT *, 'No record found.'
      GOTO 10
998 CONTINUE
      PRINT *, 'Invalid ID for SELECT.'
      GOTO 10
    ELSE
      PRINT *, 'Invalid SELECT command.'
      GOTO 10
    ENDIF

  CASE (3) ! DELETE
    IF (ARG1 .EQ. 'ID') THEN
      READ(ARG2,'(I5)',ERR=997,END=997) IDX
      DO I = 1, COUNT
        IF (IDS(I) .EQ. IDX) THEN
          CALL DELETE_RECORD(I)
          PRINT *, 'Record deleted.'
          GOTO 10
        ENDIF
      ENDDO
      PRINT *, 'No record found to delete.'
      GOTO 10
997 CONTINUE
      PRINT *, 'Invalid ID for DELETE.'
      GOTO 10
    ELSE
      PRINT *, 'Invalid DELETE command.'
      GOTO 10
    ENDIF

  CASE (4) ! BEGIN
    IF (IN_TRANSACTION .EQ. 1) THEN
      PRINT *, 'Already in transaction.'
    ELSE
      CALL START_TRANSACTION()
      PRINT *, 'Transaction started.'
    ENDIF
    GOTO 10

  CASE (5) ! COMMIT
    IF (IN_TRANSACTION .EQ. 0) THEN
      PRINT *, 'No active transaction.'
    ELSE
      CALL COMMIT_TRANSACTION()
      PRINT *, 'Transaction committed.'
    ENDIF
    GOTO 10

  CASE (6) ! ROLLBACK
    IF (IN_TRANSACTION .EQ. 0) THEN
      PRINT *, 'No active transaction.'
    ELSE
      CALL ROLLBACK_TRANSACTION()
      PRINT *, 'Transaction rolled back.'
    ENDIF
    GOTO 10

  CASE (7) ! EXIT
    PRINT *, 'Bye!'
    STOP

  CASE (8) ! UPDATE
    ! Format: UPDATE ID newname newage
    IF (ARG1 .EQ. '' .OR. ARG2 .EQ. '' .OR. ARG3 .EQ. '') THEN
      PRINT *, 'Usage: UPDATE ID newname newage'
      GOTO 10
    ENDIF
    READ(ARG1,'(I5)',ERR=996,END=996) IDX
    DO I = 1, COUNT
      IF (IDS(I) .EQ. IDX) THEN
        NAMES(I) = ARG2
        READ(ARG3,'(I5)',ERR=996,END=996) AGES(I)
        PRINT *, 'Record updated.'
        GOTO 10
      ENDIF
    ENDDO
    PRINT *, 'No record found to update.'
    GOTO 10
996 CONTINUE
    PRINT *, 'Invalid ID or AGE for UPDATE.'
    GOTO 10

  CASE (9) ! COUNT
    PRINT *, 'Total records:', COUNT
    GOTO 10

  CASE (10) ! CLEAR
    COUNT = 0
    ! If inside transaction, also clear shadow data
    IF (IN_TRANSACTION .EQ. 1) THEN
      COUNT_SHADOW = 0
    ENDIF
    PRINT *, 'All records cleared.'
    GOTO 10

  CASE (11) ! HELP
    PRINT *, 'Available commands:'
    PRINT *, '  INSERT id name age      - Insert a new record'
    PRINT *, '  SELECT ALL             - Show all records'
    PRINT *, '  SELECT ID id           - Show record with ID'
    PRINT *, '  DELETE ID id           - Delete record with ID'
    PRINT *, '  UPDATE id newname newage - Update record with ID'
    PRINT *, '  COUNT                  - Show total number of records'
    PRINT *, '  CLEAR                  - Delete all records'
    PRINT *, '  BEGIN                  - Start transaction'
    PRINT *, '  COMMIT                 - Commit transaction'
    PRINT *, '  ROLLBACK               - Rollback transaction'
    PRINT *, '  HELP                   - Show this help message'
    PRINT *, '  EXIT                   - Exit the program'
    GOTO 10

  END SELECT

  GOTO 10

CONTAINS

  SUBROUTINE PARSE_INPUT(LINE, CMD, ARG1, ARG2, ARG3)
    CHARACTER(len=*), INTENT(IN) :: LINE
    CHARACTER(len=20), INTENT(OUT) :: CMD, ARG1, ARG2, ARG3
    CHARACTER(len=100) :: TMP_LINE
    INTEGER :: POS, START, LEN_LINE
    CHARACTER(len=20) :: TOKENS(10)
    INTEGER :: TOKEN_COUNT
    CHARACTER :: CH

    TMP_LINE = ADJUSTL(TRIM(LINE))
    TOKEN_COUNT = 0
    START = 1
    LEN_LINE = LEN_TRIM(TMP_LINE)

    DO WHILE (START <= LEN_LINE)
      ! Find next space or end
      POS = START
      DO
        IF (POS > LEN_LINE) EXIT
        CH = TMP_LINE(POS:POS)
        IF (CH == ' ') EXIT
        POS = POS + 1
      END DO
      TOKEN_COUNT = TOKEN_COUNT + 1
      TOKENS(TOKEN_COUNT) = TMP_LINE(START:POS-1)
      ! Skip spaces
      START = POS + 1
      DO WHILE (START <= LEN_LINE .AND. TMP_LINE(START:START) == ' ')
        START = START + 1
      END DO
    END DO

    CMD = ''
    ARG1 = ''
    ARG2 = ''
    ARG3 = ''

    IF (TOKEN_COUNT >= 1) CMD = TOKENS(1)
    IF (TOKEN_COUNT >= 2) ARG1 = TOKENS(2)
    IF (TOKEN_COUNT >= 3) ARG2 = TOKENS(3)
    IF (TOKEN_COUNT >= 4) ARG3 = TOKENS(4)
  END SUBROUTINE PARSE_INPUT

  SUBROUTINE DELETE_RECORD(IDX)
    INTEGER IDX, I
    DO I = IDX, COUNT - 1
      IDS(I) = IDS(I+1)
      NAMES(I) = NAMES(I+1)
      AGES(I) = AGES(I+1)
    ENDDO
    COUNT = COUNT - 1
  END SUBROUTINE DELETE_RECORD

  SUBROUTINE START_TRANSACTION()
    INTEGER I
    DO I = 1, COUNT
      IDS_SHADOW(I) = IDS(I)
      NAMES_SHADOW(I) = NAMES(I)
      AGES_SHADOW(I) = AGES(I)
    ENDDO
    COUNT_SHADOW = COUNT
    IN_TRANSACTION = 1
  END SUBROUTINE START_TRANSACTION

  SUBROUTINE COMMIT_TRANSACTION()
    IN_TRANSACTION = 0
  END SUBROUTINE COMMIT_TRANSACTION

  SUBROUTINE ROLLBACK_TRANSACTION()
    INTEGER I
    DO I = 1, COUNT_SHADOW
      IDS(I) = IDS_SHADOW(I)
      NAMES(I) = NAMES_SHADOW(I)
      AGES(I) = AGES_SHADOW(I)
    ENDDO
    COUNT = COUNT_SHADOW
    IN_TRANSACTION = 0
  END SUBROUTINE ROLLBACK_TRANSACTION

  SUBROUTINE TOUPPER(STR)
    CHARACTER*(*) STR
    INTEGER I, C
    CHARACTER CCHAR
    DO I = 1, LEN(STR)
      C = ICHAR(STR(I:I))
      IF (C >= 97 .AND. C <= 122) THEN
        C = C - 32
        CCHAR = CHAR(C)
        STR(I:I) = CCHAR
      ENDIF
    ENDDO
  END SUBROUTINE TOUPPER

END PROGRAM MICRODB
