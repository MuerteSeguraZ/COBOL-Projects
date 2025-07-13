       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIAGNOSTICMODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 SECTOR-ID         PIC 9(5) COMP-5 VALUE 0.
       01 MAX-SECTOR        PIC 9(5) COMP-5 VALUE 128.
       01 SECTOR-BUFFER     PIC X(508) VALUE SPACES.
       01 PREV-BUFFER       PIC X(508) VALUE SPACES.

       01 VALID-COUNT       PIC 9(5) COMP-5 VALUE 0.
       01 ERROR-COUNT       PIC 9(5) COMP-5 VALUE 0.
       01 DIVERGE-COUNT     PIC 9(5) COMP-5 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-SECTION.

           DISPLAY "Starting RAID Diagnostic Mode..."
           DISPLAY "----------------------------------"

           PERFORM VARYING SECTOR-ID FROM 0 BY 1
               UNTIL SECTOR-ID >= MAX-SECTOR

               CALL "read_floppy_" USING SECTOR-BUFFER SECTOR-ID

               IF SECTOR-BUFFER = SPACES
                   ADD 1 TO ERROR-COUNT
                   DISPLAY "Sector " WITH NO ADVANCING
                   DISPLAY SECTOR-ID WITH NO ADVANCING
                   DISPLAY " corrupted (invalid on both)"
               ELSE
                   IF SECTOR-ID = 0
                       ADD 1 TO VALID-COUNT
                   ELSE
                       IF SECTOR-BUFFER NOT = PREV-BUFFER
                           ADD 1 TO DIVERGE-COUNT
                           DISPLAY "Warning: Sector " WITH NO ADVANCING
                           DISPLAY SECTOR-ID WITH NO ADVANCING
                           DISPLAY " diverged (A <> B)"
                       ELSE
                           ADD 1 TO VALID-COUNT
                       END-IF
                   END-IF
               END-IF

               MOVE SECTOR-BUFFER TO PREV-BUFFER

           END-PERFORM

           DISPLAY "----------------------------------"
           DISPLAY "RAID Diagnostic Complete:"
           DISPLAY "  Valid sectors:     " VALID-COUNT
           DISPLAY "  Diverged sectors:  " DIVERGE-COUNT
           DISPLAY "  Corrupted sectors: " ERROR-COUNT

           STOP RUN.
