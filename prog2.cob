       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-GRADING-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-INPUT-FILE ASSIGN TO "STUDENT-INPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUDENT-REPORT-FILE ASSIGN TO "STUDENT-REPORT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-INPUT-FILE.
       01  STUDENT-INPUT-REC PIC X(100).

       FD  STUDENT-REPORT-FILE.
       01  STUDENT-REPORT-REC PIC X(120).

       WORKING-STORAGE SECTION.

       01  PASSING-GRADE        PIC 99 VALUE 75.
       01  WS-EOF-FLAG          PIC 9 VALUE 0.

       01  YEAR-LEVELS.
           05 YEAR-NAME OCCURS 4 TIMES PIC X(10).

       01  STUDENT-COUNTS.
           05 NUM-STUDENTS OCCURS 4 TIMES PIC 999 VALUE 0.

       01  STUDENT-TABLE.
           05 YEAR-TABLE OCCURS 4 TIMES.
              10 STUDENT-RECORD OCCURS 50 TIMES.
                 15 PRELIM-GRADE   PIC 999V99.
                 15 MIDTERM-GRADE  PIC 999V99.
                 15 FINAL-GRADE    PIC 999V99.
                 15 AVERAGE-GRADE  PIC 999V99.

       01  YEAR-TOTALS.
           05 PASSED-COUNT OCCURS 4 TIMES PIC 999 VALUE 0.
           05 FAILED-COUNT OCCURS 4 TIMES PIC 999 VALUE 0.

       01  YEAR-SUMS.
           05 SUM-PRELIM   OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
           05 SUM-MIDTERM  OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
           05 SUM-FINAL    OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
           05 SUM-AVG      OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.

       01  GRAND-TOTALS.
           05 GRAND-STUDENTS PIC 999 VALUE 0.
           05 GRAND-PASSED   PIC 999 VALUE 0.
           05 GRAND-FAILED   PIC 999 VALUE 0.

       01  Y PIC 9.
       01  S PIC 99.

       01  WS-TXT-PARSE.
           05 WS-TXT-LINE       PIC X(100).
           05 WS-TXT-YEAR       PIC X(15).
           05 WS-TXT-DUMMY      PIC X(10).
           05 WS-TXT-PRELIM     PIC X(10).
           05 WS-TXT-MIDTERM    PIC X(10).
           05 WS-TXT-FINAL      PIC X(10).

       01  WS-AVG-PRELIM   PIC 999V99.
       01  WS-AVG-MIDTERM  PIC 999V99.
       01  WS-AVG-FINAL    PIC 999V99.
       01  WS-AVG-AVERAGE  PIC 999V99.

       01  WS-CURRENT-YEAR PIC 9 VALUE 0.
       01  WS-CURRENT-STU  PIC 99 VALUE 0.

       01  WS-OUTPUT-LINES.
           05 WS-HEADER1.
              10 FILLER         PIC X(26) VALUE SPACES.
              10 FILLER         PIC X(42) 
                 VALUE "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES".
           05 WS-HEADER2.
              10 FILLER         PIC X(25) VALUE SPACES.
              10 FILLER         PIC X(50) 
                 VALUE "COLLEGE OF COMPUTER AND INFORMATION SCIENCE".
           05 WS-HEADER3.
              10 FILLER         PIC X(32) VALUE SPACES.
              10 FILLER         PIC X(22) 
                 VALUE "STUDENT GRADING SYSTEM".
           05 WS-BLANK-LINE     PIC X(90) VALUE SPACES.
           05 WS-COL-HDR1.
              10 FILLER         PIC X(10) VALUE "YEAR LEVEL".
              10 FILLER         PIC X(2) VALUE SPACES.
              10 FILLER         PIC X(6) VALUE "NO. OF".
              10 FILLER         PIC X(4) VALUE SPACES.
              10 FILLER         PIC X(6) VALUE "PRELIM".
              10 FILLER         PIC X(4) VALUE SPACES.
              10 FILLER         PIC X(7) VALUE "MIDTERM".
              10 FILLER         PIC X(4) VALUE SPACES.
              10 FILLER         PIC X(11) VALUE "FINAL GRADE".
              10 FILLER         PIC X(2) VALUE SPACES.
              10 FILLER         PIC X(7) VALUE "AVERAGE".
              10 FILLER         PIC X(3) VALUE SPACES.
              10 FILLER         PIC X(6) VALUE "PASSED".
              10 FILLER         PIC X(6) VALUE SPACES.
              10 FILLER         PIC X(6) VALUE "FAILED".
           05 WS-COL-HDR2.
              10 FILLER         PIC X(12) VALUE SPACES.
              10 FILLER         PIC X(8) VALUE "STUDENTS".
              10 FILLER         PIC X(2) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".
              10 FILLER         PIC X(5) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".
              10 FILLER         PIC X(19) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".
           05 WS-DETAIL-LINE.
              10 WS-DL-YEAR     PIC X(10).
              10 FILLER         PIC X(4) VALUE SPACES.
              10 WS-DL-COUNT    PIC Z9.
              10 FILLER         PIC X(6) VALUE SPACES.
              10 WS-DL-PRELIM   PIC Z9.99.
              10 FILLER         PIC X(5) VALUE SPACES.
              10 WS-DL-MIDTERM  PIC Z9.99.
              10 FILLER         PIC X(7) VALUE SPACES.
              10 WS-DL-FINAL    PIC Z9.99.
              10 FILLER         PIC X(7) VALUE SPACES.
              10 WS-DL-AVG      PIC Z9.99.
              10 FILLER         PIC X(6) VALUE SPACES.
              10 WS-DL-PASSED   PIC Z9.
              10 FILLER         PIC X(10) VALUE SPACES.
              10 WS-DL-FAILED   PIC Z9.
           05 WS-TOTAL-LINE.
              10 FILLER         PIC X(10) VALUE "TOTAL".
              10 FILLER         PIC X(4) VALUE SPACES.
              10 WS-TL-COUNT    PIC Z9.
              10 FILLER         PIC X(51) VALUE SPACES.
              10 WS-TL-PASSED   PIC Z9.
              10 FILLER         PIC X(10) VALUE SPACES.
              10 WS-TL-FAILED   PIC Z9.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM INIT-YEAR-NAMES
           PERFORM READ-INPUT-FILE
           PERFORM COMPUTE-RESULTS
           PERFORM WRITE-REPORT
           CLOSE STUDENT-REPORT-FILE
           DISPLAY "Report generated: STUDENT-REPORT.TXT"
           STOP RUN.

       INIT-YEAR-NAMES.
           MOVE "Freshmen" TO YEAR-NAME(1)
           MOVE "Sophomore" TO YEAR-NAME(2)
           MOVE "Junior" TO YEAR-NAME(3)
           MOVE "Senior" TO YEAR-NAME(4)
           OPEN INPUT STUDENT-INPUT-FILE
           OPEN OUTPUT STUDENT-REPORT-FILE.

       READ-INPUT-FILE.
           MOVE 0 TO WS-EOF-FLAG
           PERFORM UNTIL WS-EOF-FLAG = 1
               READ STUDENT-INPUT-FILE INTO WS-TXT-LINE
                   AT END
                       MOVE 1 TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM PARSE-INPUT-LINE
               END-READ
           END-PERFORM
           CLOSE STUDENT-INPUT-FILE.

       PARSE-INPUT-LINE.
           UNSTRING WS-TXT-LINE DELIMITED BY ","
               INTO WS-TXT-YEAR
                    WS-TXT-DUMMY
                    WS-TXT-PRELIM
                    WS-TXT-MIDTERM
                    WS-TXT-FINAL
           END-UNSTRING

           EVALUATE FUNCTION TRIM(WS-TXT-YEAR)
               WHEN "Freshmen"
                   MOVE 1 TO WS-CURRENT-YEAR
               WHEN "Sophomore"
                   MOVE 2 TO WS-CURRENT-YEAR
               WHEN "Junior"
                   MOVE 3 TO WS-CURRENT-YEAR
               WHEN "Senior"
                   MOVE 4 TO WS-CURRENT-YEAR
               WHEN OTHER
                   MOVE 0 TO WS-CURRENT-YEAR
           END-EVALUATE

           IF WS-CURRENT-YEAR > 0
               ADD 1 TO NUM-STUDENTS(WS-CURRENT-YEAR)
               MOVE NUM-STUDENTS(WS-CURRENT-YEAR) 
                   TO WS-CURRENT-STU
               ADD 1 TO GRAND-STUDENTS

               MOVE FUNCTION NUMVAL(WS-TXT-PRELIM)
                   TO PRELIM-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
               MOVE FUNCTION NUMVAL(WS-TXT-MIDTERM)
                   TO MIDTERM-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
               MOVE FUNCTION NUMVAL(WS-TXT-FINAL)
                   TO FINAL-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
           END-IF.

       COMPUTE-RESULTS.
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 4
               PERFORM VARYING S FROM 1 BY 1
                   UNTIL S > NUM-STUDENTS(Y)
                   COMPUTE AVERAGE-GRADE(Y, S) =
                       ( PRELIM-GRADE(Y, S)
                       + MIDTERM-GRADE(Y, S)
                       + FINAL-GRADE(Y, S) ) / 3
                   ADD PRELIM-GRADE(Y, S)  TO SUM-PRELIM(Y)
                   ADD MIDTERM-GRADE(Y, S) TO SUM-MIDTERM(Y)
                   ADD FINAL-GRADE(Y, S)   TO SUM-FINAL(Y)
                   ADD AVERAGE-GRADE(Y, S) TO SUM-AVG(Y)
                   IF AVERAGE-GRADE(Y, S) >= PASSING-GRADE
                       ADD 1 TO PASSED-COUNT(Y)
                       ADD 1 TO GRAND-PASSED
                   ELSE
                       ADD 1 TO FAILED-COUNT(Y)
                       ADD 1 TO GRAND-FAILED
                   END-IF
               END-PERFORM
           END-PERFORM.

       WRITE-REPORT.
           WRITE STUDENT-REPORT-REC FROM WS-HEADER1
           WRITE STUDENT-REPORT-REC FROM WS-HEADER2
           WRITE STUDENT-REPORT-REC FROM WS-HEADER3
           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
           WRITE STUDENT-REPORT-REC FROM WS-COL-HDR1
           WRITE STUDENT-REPORT-REC FROM WS-COL-HDR2
           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 4
               MOVE SPACES TO WS-DETAIL-LINE

               IF NUM-STUDENTS(Y) > 0
                   COMPUTE WS-AVG-PRELIM = 
                       SUM-PRELIM(Y) / NUM-STUDENTS(Y)
                   COMPUTE WS-AVG-MIDTERM = 
                       SUM-MIDTERM(Y) / NUM-STUDENTS(Y)
                   COMPUTE WS-AVG-FINAL = 
                       SUM-FINAL(Y) / NUM-STUDENTS(Y)
                   COMPUTE WS-AVG-AVERAGE = 
                       SUM-AVG(Y) / NUM-STUDENTS(Y)
               ELSE
                   MOVE 0 TO WS-AVG-PRELIM
                   MOVE 0 TO WS-AVG-MIDTERM
                   MOVE 0 TO WS-AVG-FINAL
                   MOVE 0 TO WS-AVG-AVERAGE
               END-IF

               MOVE YEAR-NAME(Y) TO WS-DL-YEAR
               MOVE NUM-STUDENTS(Y) TO WS-DL-COUNT
               MOVE WS-AVG-PRELIM TO WS-DL-PRELIM
               MOVE WS-AVG-MIDTERM TO WS-DL-MIDTERM
               MOVE WS-AVG-FINAL TO WS-DL-FINAL
               MOVE WS-AVG-AVERAGE TO WS-DL-AVG
               MOVE PASSED-COUNT(Y) TO WS-DL-PASSED
               MOVE FAILED-COUNT(Y) TO WS-DL-FAILED

               WRITE STUDENT-REPORT-REC FROM WS-DETAIL-LINE
               WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
           END-PERFORM

           MOVE GRAND-STUDENTS TO WS-TL-COUNT
           MOVE GRAND-PASSED TO WS-TL-PASSED
           MOVE GRAND-FAILED TO WS-TL-FAILED
           WRITE STUDENT-REPORT-REC FROM WS-TOTAL-LINE.
