       IDENTIFICATION DIVISION.
       PROGRAM-ID. http_handler.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REQ-METHOD            PIC X(4) VALUE SPACES.
       01 WS-REQ-PATH              PIC X(20) VALUE SPACES.
       01 WS-FOUND-SPACE           PIC X VALUE "N".
       01 WS-IDX                   PIC 9(4) COMP VALUE 1.
       01 WS-RESPONSE-LEN          PIC S9(9) BINARY VALUE 0.
       01 WS-TEMP                  PIC X(8192) VALUE SPACES.
       01 WS-REQ-BODY              PIC X(4096) VALUE SPACES.
       01 WS-BODY-START            PIC 9(4) COMP VALUE 0.
       01 WS-BODY-LEN              PIC 9(5) COMP VALUE 0.
       01 WS-BODY-LEN-TXT          PIC X(10) VALUE SPACES.
       01 WS-BODY-LEN-DISPLAY      REDEFINES WS-BODY-LEN-TXT PIC 9(10).
       01 WS-BODY-EXACT            PIC X(4096) VALUE SPACES.

       77 WS-DIGIT                 PIC 9 VALUE 0.
       77 WS-I                     PIC 9 COMP VALUE 0.
       77 WS-TEMP-CHAR             PIC X VALUE SPACE.
       77 WS-IDX-SHORT             PIC 9 COMP VALUE 0.
       77 WS-BODY-LEN-DISPLAY-SHORT PIC 9 COMP VALUE 0.

       LINKAGE SECTION.
       01 REQUEST-DATA             PIC X(4096).
       01 REQUEST-LEN              PIC S9(9) COMP-5.
       01 RESPONSE-DATA            PIC X(8192).
       01 RESPONSE-LEN             PIC S9(9) COMP-5.

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

           MOVE REQUEST-DATA(1:4) TO WS-REQ-METHOD
           INSPECT WS-REQ-METHOD CONVERTING "abcdefghijklmnopqrstuvwxyz" TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           DISPLAY "DEBUG: WS-REQ-METHOD uppercase=[" WS-REQ-METHOD "]"

           IF WS-REQ-METHOD = "GET "
               PERFORM EXTRACT-PATH
               DISPLAY "DEBUG: WS-REQ-PATH=[" WS-REQ-PATH "]"
               IF WS-REQ-PATH(1:1) = "/" OR WS-REQ-PATH(1:2) = "/h"
                   PERFORM RESP-200-HELLO
               ELSE
                   PERFORM RESP-404-NOT-FOUND
               END-IF
           ELSE IF WS-REQ-METHOD = "POST"
               PERFORM RESP-200-POST-RECEIVED
           ELSE IF WS-REQ-METHOD = "PUT "
               PERFORM RESP-200-PUT-RECEIVED
           ELSE IF WS-REQ-METHOD = "HEAD"
               PERFORM RESP-200-HEAD
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
               IF REQUEST-DATA(4 + WS-IDX:1) = SPACE OR
                  REQUEST-DATA(4 + WS-IDX:1) = X"0D" OR
                  REQUEST-DATA(4 + WS-IDX:1) = X"0A"
                   MOVE "Y" TO WS-FOUND-SPACE
               ELSE
                   MOVE REQUEST-DATA(4 + WS-IDX:1) TO WS-REQ-PATH(WS-IDX:1)
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
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN
           DISPLAY "DEBUG: Response length = " WS-RESPONSE-LEN
           DISPLAY "DEBUG: Response data (first 100 chars) = [" RESPONSE-DATA(1:100) "]"
           EXIT.

       RESP-200-POST-RECEIVED.
           PERFORM PARSE-BODY
           PERFORM BUILD-RESPONSE
           EXIT.

       RESP-200-PUT-RECEIVED.
           PERFORM PARSE-BODY
           PERFORM BUILD-RESPONSE
           EXIT.

       RESP-200-HEAD.
           MOVE SPACES TO WS-TEMP
           STRING
               "HTTP/1.1 200 OK" DELIMITED BY SIZE
               X"0D0A"
               "Content-Type: text/plain" DELIMITED BY SIZE
               X"0D0A"
               "Content-Length: 0" DELIMITED BY SIZE
               X"0D0A"
               X"0D0A"
               INTO WS-TEMP
           END-STRING

           PERFORM FIND-RESPONSE-LEN
           MOVE WS-TEMP TO RESPONSE-DATA
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN
           DISPLAY "DEBUG: Response length = " WS-RESPONSE-LEN
           DISPLAY "DEBUG: Response data (first 100 chars) = [" RESPONSE-DATA(1:100) "]"
           EXIT.

       RESP-400-BAD-REQUEST.
           MOVE SPACES TO WS-TEMP
           STRING
               "HTTP/1.1 400 Bad Request" DELIMITED BY SIZE
               X"0D0A"
               "Content-Length: 0" DELIMITED BY SIZE
               X"0D0A"
               X"0D0A"
               INTO WS-TEMP
           END-STRING

           PERFORM FIND-RESPONSE-LEN
           MOVE WS-TEMP TO RESPONSE-DATA
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN
           DISPLAY "DEBUG: Response length = " WS-RESPONSE-LEN
           DISPLAY "DEBUG: Response data (first 100 chars) = [" RESPONSE-DATA(1:100) "]"
           EXIT.

       RESP-404-NOT-FOUND.
           MOVE SPACES TO WS-TEMP
           STRING
               "HTTP/1.1 404 Not Found" DELIMITED BY SIZE
               X"0D0A"
               "Content-Length: 0" DELIMITED BY SIZE
               X"0D0A"
               X"0D0A"
               INTO WS-TEMP
           END-STRING

           PERFORM FIND-RESPONSE-LEN
           MOVE WS-TEMP TO RESPONSE-DATA
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN
           DISPLAY "DEBUG: Response length = " WS-RESPONSE-LEN
           DISPLAY "DEBUG: Response data (first 100 chars) = [" RESPONSE-DATA(1:100) "]"
           EXIT.

       PARSE-BODY.
           MOVE SPACES TO WS-REQ-BODY
           MOVE 0 TO WS-BODY-START
           *> Find start of body (after double CRLF = X"0D0A0D0A")
           PERFORM VARYING WS-IDX-SHORT FROM 1 BY 1 UNTIL WS-IDX-SHORT > REQUEST-LEN - 3
               IF REQUEST-DATA(WS-IDX-SHORT:4) = X"0D0A0D0A"
                   ADD 4 TO WS-IDX-SHORT
                   MOVE WS-IDX-SHORT TO WS-BODY-START
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-BODY-START > 0
               MOVE REQUEST-DATA(WS-BODY-START:REQUEST-LEN - WS-BODY-START + 1) TO WS-REQ-BODY
           ELSE
               DISPLAY "DEBUG: Could not find body start."
               MOVE 0 TO WS-BODY-LEN
           END-IF.

           *> Calculate body length
           COMPUTE WS-BODY-LEN = REQUEST-LEN - WS-BODY-START + 1

           *> Trim trailing spaces safely
           MOVE WS-BODY-LEN TO WS-IDX-SHORT
           PERFORM UNTIL WS-IDX-SHORT = 0
               IF WS-REQ-BODY(WS-IDX-SHORT:1) = SPACE
                   SUBTRACT 1 FROM WS-IDX-SHORT
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
           MOVE WS-IDX-SHORT TO WS-BODY-LEN

           IF WS-BODY-LEN = 0
               MOVE 0 TO WS-BODY-LEN
           END-IF

           *> Copy exact trimmed body to WS-BODY-EXACT
           MOVE SPACES TO WS-BODY-EXACT
           IF WS-BODY-LEN > 0
               MOVE WS-REQ-BODY(1:WS-BODY-LEN) TO WS-BODY-EXACT(1:WS-BODY-LEN)
           END-IF

           *> Convert WS-BODY-LEN (numeric) to string for Content-Length header
           MOVE SPACES TO WS-BODY-LEN-TXT
           IF WS-BODY-LEN = 0
               MOVE "0" TO WS-BODY-LEN-TXT(1:1)
               MOVE 1 TO WS-IDX-SHORT
           ELSE
               MOVE WS-BODY-LEN TO WS-BODY-LEN-DISPLAY-SHORT
               MOVE 0 TO WS-IDX-SHORT
               PERFORM UNTIL WS-BODY-LEN-DISPLAY-SHORT = 0
                   ADD 1 TO WS-IDX-SHORT
                   COMPUTE WS-DIGIT = FUNCTION MOD(WS-BODY-LEN-DISPLAY-SHORT 10)
                   COMPUTE WS-BODY-LEN-DISPLAY-SHORT = WS-BODY-LEN-DISPLAY-SHORT / 10
                   MOVE FUNCTION CHAR(48 + WS-DIGIT) TO WS-BODY-LEN-TXT(WS-IDX-SHORT:1)
               END-PERFORM

               *> Reverse the digits
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= (WS-IDX-SHORT / 2) + 1
                   MOVE WS-BODY-LEN-TXT(WS-I:1) TO WS-TEMP-CHAR
                   MOVE WS-BODY-LEN-TXT(WS-IDX-SHORT - WS-I + 1:1) TO WS-BODY-LEN-TXT(WS-I:1)
                   MOVE WS-TEMP-CHAR TO WS-BODY-LEN-TXT(WS-IDX-SHORT - WS-I + 1:1)
               END-PERFORM
           END-IF.
           EXIT.

       BUILD-RESPONSE.
           MOVE SPACES TO WS-TEMP
           STRING
               "HTTP/1.1 200 OK" DELIMITED BY SIZE
               X"0D0A"
               "Content-Type: text/plain" DELIMITED BY SIZE
               X"0D0A"
               "Content-Length: " DELIMITED BY SIZE
               WS-BODY-LEN-TXT(1:WS-IDX-SHORT) DELIMITED BY SIZE
               X"0D0A"
               X"0D0A"
               WS-BODY-EXACT(1:WS-BODY-LEN) DELIMITED BY SIZE
               INTO WS-TEMP
           END-STRING

           MOVE WS-TEMP TO RESPONSE-DATA

           PERFORM FIND-RESPONSE-LEN
           MOVE WS-RESPONSE-LEN TO RESPONSE-LEN

           DISPLAY "DEBUG: Response length = " WS-RESPONSE-LEN
           DISPLAY "DEBUG: Response data (first 100 chars) = [" RESPONSE-DATA(1:100) "]"
           EXIT.

       FIND-RESPONSE-LEN.
           MOVE LENGTH OF RESPONSE-DATA TO WS-IDX
           PERFORM UNTIL WS-IDX = 0
               IF RESPONSE-DATA(WS-IDX:1) NOT = SPACE
                   MOVE WS-IDX TO WS-RESPONSE-LEN
                   EXIT PERFORM
               END-IF
               SUBTRACT 1 FROM WS-IDX
           END-PERFORM
           IF WS-IDX = 0
               MOVE 0 TO WS-RESPONSE-LEN
           END-IF
           EXIT.
