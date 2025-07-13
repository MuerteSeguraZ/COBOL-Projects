       IDENTIFICATION DIVISION.
       PROGRAM-ID. http_handler.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REQ-METHOD      PIC X(3) VALUE SPACES.
       01 WS-REQ-PATH        PIC X(20) VALUE SPACES.
       01 WS-FOUND-SPACE     PIC X     VALUE "N".
       01 WS-IDX             PIC 9(4) COMP VALUE 1.
       01 WS-RESPONSE-LEN    PIC S9(9) BINARY VALUE 0.
       01 WS-TEMP            PIC X(8192) VALUE SPACES.

       LINKAGE SECTION.
       01 REQUEST-DATA       PIC X(4096).
       01 REQUEST-LEN        PIC S9(9) COMP-5.
       01 RESPONSE-DATA      PIC X(8192).
       01 RESPONSE-LEN       PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING REQUEST-DATA REQUEST-LEN RESPONSE-DATA RESPONSE-LEN.

       MAIN-PARA.
           DISPLAY "COBOL: Handling HTTP request."
           DISPLAY "DEBUG: REQUEST-LEN = " REQUEST-LEN
           DISPLAY "DEBUG: REQUEST-DATA first 20 chars: [" REQUEST-DATA(1:20) "]"

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 20
               DISPLAY "DEBUG: BYTE " WS-IDX " = " FUNCTION ORD(REQUEST-DATA(WS-IDX:1))
           END-PERFORM

           MOVE SPACES TO RESPONSE-DATA
           MOVE 0 TO RESPONSE-LEN
           MOVE SPACES TO WS-TEMP
           MOVE SPACES TO WS-REQ-METHOD
           MOVE SPACES TO WS-REQ-PATH
           MOVE "N" TO WS-FOUND-SPACE
           MOVE 1 TO WS-IDX
           MOVE 0 TO WS-RESPONSE-LEN

           IF REQUEST-LEN <= 0 OR REQUEST-LEN > LENGTH OF REQUEST-DATA
               DISPLAY "DEBUG: Invalid request length."
               PERFORM RESP-400-BAD-REQUEST
               GO TO END-PROCESS
           END-IF

           MOVE REQUEST-DATA(1:3) TO WS-REQ-METHOD
           DISPLAY "DEBUG: WS-REQ-METHOD=[" WS-REQ-METHOD "]"

           INSPECT WS-REQ-METHOD 
                CONVERTING "abcdefghijklmnopqrstuvwxyz" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

           DISPLAY "DEBUG: WS-REQ-METHOD uppercase=[" WS-REQ-METHOD "]"

           IF WS-REQ-METHOD = "GET"
               PERFORM EXTRACT-PATH
               DISPLAY "DEBUG: WS-REQ-PATH=[" WS-REQ-PATH "]"
               IF WS-REQ-PATH(1:1) = "/" OR WS-REQ-PATH(1:2) = "/h"
                   PERFORM RESP-200-HELLO
               ELSE
                   PERFORM RESP-404-NOT-FOUND
               END-IF
           ELSE
               DISPLAY "DEBUG: Unsupported method."
               PERFORM RESP-400-BAD-REQUEST
           END-IF.

       END-PROCESS.
           GOBACK.

       EXTRACT-PATH.
           MOVE SPACES TO WS-REQ-PATH
           MOVE "N" TO WS-FOUND-SPACE
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-IDX > 20 OR WS-FOUND-SPACE = "Y"
               IF REQUEST-DATA(5 + WS-IDX - 1:1) = SPACE OR
                  REQUEST-DATA(5 + WS-IDX - 1:1) = X"0D" OR
                  REQUEST-DATA(5 + WS-IDX - 1:1) = X"0A"
                   MOVE "Y" TO WS-FOUND-SPACE
               ELSE
                   MOVE REQUEST-DATA(5 + WS-IDX - 1:1) TO WS-REQ-PATH(WS-IDX:1)
               END-IF
               ADD 1 TO WS-IDX
           END-PERFORM.

       RESP-200-HELLO.
           MOVE SPACES TO WS-TEMP
           STRING
               "HTTP/1.1 200 OK" DELIMITED BY SIZE
               X"0D0A"
               "Content-Type: text/plain" DELIMITED BY SIZE
               X"0D0A"
               "Content-Length: 24" DELIMITED BY SIZE
               X"0D0A"
               X"0D0A"
               "Hello, world from COBOL!"
               DELIMITED BY SIZE
               INTO WS-TEMP
           END-STRING

           PERFORM FIND-RESPONSE-LEN

           MOVE WS-TEMP TO RESPONSE-DATA
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN.

       RESP-404-NOT-FOUND.
           MOVE SPACES TO WS-TEMP
           STRING
               "HTTP/1.1 404 Not Found" DELIMITED BY SIZE
               X"0D0A"
               "Content-Type: text/plain" DELIMITED BY SIZE
               X"0D0A"
               "Content-Length: 9" DELIMITED BY SIZE
               X"0D0A"
               X"0D0A"
               "Not Found"
               DELIMITED BY SIZE
               INTO WS-TEMP
           END-STRING

           PERFORM FIND-RESPONSE-LEN

           MOVE WS-TEMP TO RESPONSE-DATA
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN.

       RESP-400-BAD-REQUEST.
           MOVE SPACES TO WS-TEMP
           STRING
               "HTTP/1.1 400 Bad Request" DELIMITED BY SIZE
               X"0D0A"
               "Content-Type: text/plain" DELIMITED BY SIZE
               X"0D0A"
               "Content-Length: 11" DELIMITED BY SIZE
               X"0D0A"
               X"0D0A"
               "Bad Request"
               DELIMITED BY SIZE
               INTO WS-TEMP
           END-STRING

           PERFORM FIND-RESPONSE-LEN

           MOVE WS-TEMP TO RESPONSE-DATA
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN.

       FIND-RESPONSE-LEN.
           MOVE 8192 TO WS-IDX
           PERFORM UNTIL WS-IDX = 1
               IF WS-TEMP(WS-IDX:1) NOT = SPACE
                   MOVE WS-IDX TO WS-RESPONSE-LEN
                   EXIT PERFORM
               END-IF
               SUBTRACT 1 FROM WS-IDX
           END-PERFORM.
