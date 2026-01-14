      *================================================================
      * PROGRAM: STUDENT GRADING SYSTEM WITH INPUT VALIDATION
      *================================================================
      * PURPOSE: This program processes student grades by year level
      *          (Freshmen, Sophomore, Junior, Senior), calculates
      *          averages, determines pass/fail status, and generates
      *          a formatted report.
      *
      * HOW IT WORKS:
      *   1. Reads student data from STUDENT-INPUT.TXT
      *   2. Validates each field before processing:
      *      - Year Level: Must be alphabetical only
      *      - Prelim Grade: Must be numeric
      *      - Midterm Grade: Must be numeric
      *      - Final Grade: Must be numeric
      *   3. If validation fails, displays error and stops
      *   4. If validation passes, computes averages and pass/fail
      *   5. Generates report file (STUDENT-REPORT.TXT)
      *
      * GRADING FORMULAS:
      *   - Average = (Prelim + Midterm + Final) / 3
      *   - Passing Grade = 75 or higher
      *
      * INPUT FILE FORMAT (CSV):
      *   YearLevel,StudentID,PrelimGrade,MidtermGrade,FinalGrade
      *   Example: Freshmen,2024001,85,90,88
      *
      * VALIDATION RULES:
      *   - Year Level: Alphabetic characters and spaces only
      *   - All Grades: Valid numeric values
      *
      * DATA STRUCTURES USED:
      *   - Arrays (OCCURS): Store data for 4 year levels
      *   - 2D Arrays: Store up to 50 students per year level
      *
      * ALGORITHMS USED:
      *   - Linear Search: Character-by-character validation
      *   - Accumulation: Sum grades for averaging
      *================================================================

       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-GRADING-SYSTEM.
      * Program name - used when compiling and running

       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------
      * ENVIRONMENT DIVISION - File connections
      *----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-INPUT-FILE ASSIGN TO "STUDENT-INPUT.TXT"
      * Input file containing student grade records
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUDENT-REPORT-FILE ASSIGN TO "STUDENT-REPORT.TXT"
      * Output file for the generated report
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *----------------------------------------------------------------
      * DATA DIVISION - All data definitions
      *----------------------------------------------------------------
       FILE SECTION.
      * File record buffers
       FD  STUDENT-INPUT-FILE.
       01  STUDENT-INPUT-REC PIC X(100).
      * Buffer for reading input lines

       FD  STUDENT-REPORT-FILE.
       01  STUDENT-REPORT-REC PIC X(120).
      * Buffer for writing report lines

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * WORKING-STORAGE - Program variables and constants
      *----------------------------------------------------------------

       01  PASSING-GRADE        PIC 99 VALUE 75.
      * Constant: Minimum grade to pass (75%)
      * VALUE 75 = Initialize at program start

       01  WS-EOF-FLAG          PIC 9 VALUE 0.
      * End-of-file indicator: 0 = more data, 1 = end reached

      *----------------------------------------------------------------
      * VALIDATION VARIABLES
      * Support input validation logic
      *----------------------------------------------------------------
       01  WS-VALIDATION.
           05 WS-VALID-FLAG      PIC 9 VALUE 0.
      * Per-field result: 0 = valid, 1 = invalid
           05 WS-HAS-ERROR       PIC 9 VALUE 0.
      * Global error flag: 0 = no errors, 1 = error found
      * If 1, report will NOT be generated
           05 WS-CHAR-INDEX      PIC 99 VALUE 0.
      * Loop counter for character validation
           05 WS-CURRENT-CHAR    PIC X(1).
      * Single character being checked
           05 WS-TRIMMED-YEAR    PIC X(15).
      * Year level after trimming whitespace
           05 WS-TRIMMED-PRELIM  PIC X(10).
      * Prelim grade as trimmed string
           05 WS-TRIMMED-MIDTERM PIC X(10).
      * Midterm grade as trimmed string
           05 WS-TRIMMED-FINAL   PIC X(10).
      * Final grade as trimmed string

      *----------------------------------------------------------------
      * YEAR LEVEL DATA
      *----------------------------------------------------------------
       01  YEAR-LEVELS.
      * Names of the 4 year levels
           05 YEAR-NAME OCCURS 4 TIMES PIC X(10).
      * Array: (1)=Freshmen, (2)=Sophomore, (3)=Junior, (4)=Senior

       01  STUDENT-COUNTS.
      * Number of students in each year level
           05 NUM-STUDENTS OCCURS 4 TIMES PIC 999 VALUE 0.
      * Array of counters, initialized to 0

      *----------------------------------------------------------------
      * STUDENT GRADE TABLE - 2D Array Structure
      * This is a TWO-DIMENSIONAL array:
      *   - First dimension: Year level (1-4)
      *   - Second dimension: Student number (1-50)
      * Access: PRELIM-GRADE(year, student)
      *----------------------------------------------------------------
       01  STUDENT-TABLE.
           05 YEAR-TABLE OCCURS 4 TIMES.
      * Outer array: 4 year levels
              10 STUDENT-RECORD OCCURS 50 TIMES.
      * Inner array: up to 50 students per year
                 15 PRELIM-GRADE   PIC 999V99.
      * Prelim grade with 2 decimal places
                 15 MIDTERM-GRADE  PIC 999V99.
                 15 FINAL-GRADE    PIC 999V99.
                 15 AVERAGE-GRADE  PIC 999V99.
      * Calculated: (Prelim + Midterm + Final) / 3

      *----------------------------------------------------------------
      * PASS/FAIL COUNTERS
      *----------------------------------------------------------------
       01  YEAR-TOTALS.
           05 PASSED-COUNT OCCURS 4 TIMES PIC 999 VALUE 0.
      * Students who passed (avg >= 75) per year level
           05 FAILED-COUNT OCCURS 4 TIMES PIC 999 VALUE 0.
      * Students who failed (avg < 75) per year level

      *----------------------------------------------------------------
      * GRADE SUMS FOR AVERAGING
      * Accumulate totals to calculate averages per year level
      *----------------------------------------------------------------
       01  YEAR-SUMS.
           05 SUM-PRELIM   OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
      * Sum of all prelim grades per year
           05 SUM-MIDTERM  OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
           05 SUM-FINAL    OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
           05 SUM-AVG      OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
      * Sum of all average grades per year

      *----------------------------------------------------------------
      * GRAND TOTALS - Across all year levels
      *----------------------------------------------------------------
       01  GRAND-TOTALS.
           05 GRAND-STUDENTS PIC 999 VALUE 0.
      * Total students across all years
           05 GRAND-PASSED   PIC 999 VALUE 0.
      * Total passed across all years
           05 GRAND-FAILED   PIC 999 VALUE 0.
      * Total failed across all years

      *----------------------------------------------------------------
      * LOOP COUNTERS
      *----------------------------------------------------------------
       01  Y PIC 9.
      * Year level loop counter (1-4)
       01  S PIC 99.
      * Student loop counter (1-50)

      *----------------------------------------------------------------
      * INPUT PARSING VARIABLES
      *----------------------------------------------------------------
       01  WS-TXT-PARSE.
           05 WS-TXT-LINE       PIC X(100).
      * Entire line read from file
           05 WS-TXT-YEAR       PIC X(15).
      * Year level field from CSV
           05 WS-TXT-DUMMY      PIC X(10).
      * Student ID (not used in calculations, hence "dummy")
           05 WS-TXT-PRELIM     PIC X(10).
      * Prelim grade as text
           05 WS-TXT-MIDTERM    PIC X(10).
      * Midterm grade as text
           05 WS-TXT-FINAL      PIC X(10).
      * Final grade as text

      *----------------------------------------------------------------
      * CALCULATED AVERAGES FOR REPORT
      *----------------------------------------------------------------
       01  WS-AVG-PRELIM   PIC 999V99.
      * Average prelim grade for a year level
       01  WS-AVG-MIDTERM  PIC 999V99.
       01  WS-AVG-FINAL    PIC 999V99.
       01  WS-AVG-AVERAGE  PIC 999V99.
      * Average of averages for a year level

       01  WS-CURRENT-YEAR PIC 9 VALUE 0.
      * Current year level being processed (1-4)
       01  WS-CURRENT-STU  PIC 99 VALUE 0.
      * Current student number within year
       01  WS-RECORD-NUM   PIC 999 VALUE 0.
      * Record counter for error messages

      *----------------------------------------------------------------
      * REPORT LAYOUT VARIABLES
      *----------------------------------------------------------------
       01  WS-OUTPUT-LINES.
           05 WS-HEADER1.
      * University name header
              10 FILLER         PIC X(26) VALUE SPACES.
              10 FILLER         PIC X(42) 
                 VALUE "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES".
           05 WS-HEADER2.
      * College name
              10 FILLER         PIC X(25) VALUE SPACES.
              10 FILLER         PIC X(50) 
                 VALUE "COLLEGE OF COMPUTER AND INFORMATION SCIENCE".
           05 WS-HEADER3.
      * Report title
              10 FILLER         PIC X(32) VALUE SPACES.
              10 FILLER         PIC X(22) 
                 VALUE "STUDENT GRADING SYSTEM".
           05 WS-BLANK-LINE     PIC X(90) VALUE SPACES.
      * Blank line for spacing

           05 WS-COL-HDR1.
      * Column headers - row 1
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
      * Column headers - row 2
              10 FILLER         PIC X(12) VALUE SPACES.
              10 FILLER         PIC X(8) VALUE "STUDENTS".
              10 FILLER         PIC X(2) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".
              10 FILLER         PIC X(5) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".
              10 FILLER         PIC X(19) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".

           05 WS-DETAIL-LINE.
      * Template for each year level row
              10 WS-DL-YEAR     PIC X(10).
              10 FILLER         PIC X(4) VALUE SPACES.
              10 WS-DL-COUNT    PIC Z9.
      * Z = zero suppression
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
      * Template for grand totals row
              10 FILLER         PIC X(10) VALUE "TOTAL".
              10 FILLER         PIC X(4) VALUE SPACES.
              10 WS-TL-COUNT    PIC Z9.
              10 FILLER         PIC X(51) VALUE SPACES.
              10 WS-TL-PASSED   PIC Z9.
              10 FILLER         PIC X(10) VALUE SPACES.
              10 WS-TL-FAILED   PIC Z9.

       PROCEDURE DIVISION.
      *================================================================
      * PROCEDURE DIVISION - Program logic
      *================================================================

       MAIN.
      *----------------------------------------------------------------
      * MAIN - Program entry point and control flow
      * Controls overall execution with validation check.
      *----------------------------------------------------------------
           PERFORM INIT-YEAR-NAMES
      * Initialize year level names and open files

           PERFORM READ-INPUT-FILE
      * Read and validate all input records

           IF WS-HAS-ERROR = 0
      * Only proceed if NO validation errors
               PERFORM COMPUTE-RESULTS
      * Calculate averages and pass/fail counts
               PERFORM WRITE-REPORT
      * Generate the report
               CLOSE STUDENT-REPORT-FILE
               DISPLAY "Report generated: STUDENT-REPORT.TXT"
           ELSE
      * Validation failed
               DISPLAY "Report not generated due to validation error/s."
               CLOSE STUDENT-REPORT-FILE
           END-IF
           STOP RUN.

       INIT-YEAR-NAMES.
      *----------------------------------------------------------------
      * INIT-YEAR-NAMES - Initialize year level names and open files
      *----------------------------------------------------------------
           MOVE "Freshmen" TO YEAR-NAME(1)
           MOVE "Sophomore" TO YEAR-NAME(2)
           MOVE "Junior" TO YEAR-NAME(3)
           MOVE "Senior" TO YEAR-NAME(4)
      * Populate the year level name array

           OPEN INPUT STUDENT-INPUT-FILE
           OPEN OUTPUT STUDENT-REPORT-FILE.

       READ-INPUT-FILE.
      *----------------------------------------------------------------
      * READ-INPUT-FILE - Read all records from input
      * Reads until end of file, validating each record.
      *----------------------------------------------------------------
           MOVE 0 TO WS-EOF-FLAG
           MOVE 0 TO WS-RECORD-NUM
      * Initialize counters

           PERFORM UNTIL WS-EOF-FLAG = 1
               READ STUDENT-INPUT-FILE INTO WS-TXT-LINE
                   AT END
                       MOVE 1 TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO WS-RECORD-NUM
      * Track record number for error messages
                       PERFORM PARSE-INPUT-LINE
               END-READ
           END-PERFORM
           CLOSE STUDENT-INPUT-FILE.

       PARSE-INPUT-LINE.
      *----------------------------------------------------------------
      * PARSE-INPUT-LINE - Parse and validate input record
      * Splits CSV line and validates each field in sequence:
      *   1. Year Level (alphabetic)
      *   2. Prelim Grade (numeric)
      *   3. Midterm Grade (numeric)
      *   4. Final Grade (numeric)
      * If any validation fails, sets error flag.
      *----------------------------------------------------------------
           UNSTRING WS-TXT-LINE DELIMITED BY ","
      * Split by comma
               INTO WS-TXT-YEAR
                    WS-TXT-DUMMY
      * Student ID - parsed but not validated/used
                    WS-TXT-PRELIM
                    WS-TXT-MIDTERM
                    WS-TXT-FINAL
           END-UNSTRING

      * Trim whitespace from all fields
           MOVE FUNCTION TRIM(WS-TXT-YEAR) TO WS-TRIMMED-YEAR
           MOVE FUNCTION TRIM(WS-TXT-PRELIM) TO WS-TRIMMED-PRELIM
           MOVE FUNCTION TRIM(WS-TXT-MIDTERM) TO WS-TRIMMED-MIDTERM
           MOVE FUNCTION TRIM(WS-TXT-FINAL) TO WS-TRIMMED-FINAL

      * VALIDATION CHAIN - Nested IF structure
           PERFORM VALIDATE-YEAR-LEVEL
           IF WS-VALID-FLAG = 1
      * Year level validation FAILED
               DISPLAY "ERROR: Invalid year level at record "
                   WS-RECORD-NUM ": " WS-TRIMMED-YEAR
               MOVE 1 TO WS-HAS-ERROR
           ELSE
               PERFORM VALIDATE-PRELIM-GRADE
               IF WS-VALID-FLAG = 1
      * Prelim grade validation FAILED
                   DISPLAY "ERROR: Invalid prelim grade at record "
                       WS-RECORD-NUM ": " WS-TRIMMED-PRELIM
                   MOVE 1 TO WS-HAS-ERROR
               ELSE
                   PERFORM VALIDATE-MIDTERM-GRADE
                   IF WS-VALID-FLAG = 1
      * Midterm grade validation FAILED
                       DISPLAY "ERROR: Invalid midterm at record "
                           WS-RECORD-NUM ": " WS-TRIMMED-MIDTERM
                       MOVE 1 TO WS-HAS-ERROR
                   ELSE
                       PERFORM VALIDATE-FINAL-GRADE
                       IF WS-VALID-FLAG = 1
      * Final grade validation FAILED
                           DISPLAY "ERROR: Invalid final at record "
                               WS-RECORD-NUM ": " WS-TRIMMED-FINAL
                           MOVE 1 TO WS-HAS-ERROR
                       ELSE
      * ALL VALIDATIONS PASSED - Store the data
                           PERFORM STORE-STUDENT-DATA
                       END-IF
                   END-IF
               END-IF
           END-IF.

       VALIDATE-YEAR-LEVEL.
      *----------------------------------------------------------------
      * VALIDATE-YEAR-LEVEL - Check if year level is alphabetic
      * Algorithm: Linear search through each character
      * Time Complexity: O(n) where n = string length
      * Valid: A-Z, a-z, space
      *----------------------------------------------------------------
           MOVE 0 TO WS-VALID-FLAG
           MOVE 1 TO WS-CHAR-INDEX

           PERFORM UNTIL WS-CHAR-INDEX >
               FUNCTION LENGTH(FUNCTION TRIM(WS-TRIMMED-YEAR))
      * Loop through each character
               MOVE WS-TRIMMED-YEAR(WS-CHAR-INDEX:1)
                   TO WS-CURRENT-CHAR
      * Reference modification: extract 1 char at position

               IF NOT (WS-CURRENT-CHAR IS ALPHABETIC
                   OR WS-CURRENT-CHAR = SPACE)
      * IS ALPHABETIC = COBOL class test
                   MOVE 1 TO WS-VALID-FLAG
               END-IF
               ADD 1 TO WS-CHAR-INDEX
           END-PERFORM.

       VALIDATE-PRELIM-GRADE.
      *----------------------------------------------------------------
      * VALIDATE-PRELIM-GRADE - Check if prelim is numeric
      * Uses TEST-NUMVAL: returns 0 if valid, non-zero if invalid
      *----------------------------------------------------------------
           MOVE 0 TO WS-VALID-FLAG
           IF FUNCTION TEST-NUMVAL(WS-TRIMMED-PRELIM) IS NOT ZERO
               MOVE 1 TO WS-VALID-FLAG
           END-IF.

       VALIDATE-MIDTERM-GRADE.
      *----------------------------------------------------------------
      * VALIDATE-MIDTERM-GRADE - Check if midterm is numeric
      *----------------------------------------------------------------
           MOVE 0 TO WS-VALID-FLAG
           IF FUNCTION TEST-NUMVAL(WS-TRIMMED-MIDTERM) IS NOT ZERO
               MOVE 1 TO WS-VALID-FLAG
           END-IF.

       VALIDATE-FINAL-GRADE.
      *----------------------------------------------------------------
      * VALIDATE-FINAL-GRADE - Check if final is numeric
      *----------------------------------------------------------------
           MOVE 0 TO WS-VALID-FLAG
           IF FUNCTION TEST-NUMVAL(WS-TRIMMED-FINAL) IS NOT ZERO
               MOVE 1 TO WS-VALID-FLAG
           END-IF.

       STORE-STUDENT-DATA.
      *----------------------------------------------------------------
      * STORE-STUDENT-DATA - Store validated data in arrays
      * Maps year level name to array index using EVALUATE.
      *----------------------------------------------------------------
           EVALUATE WS-TRIMMED-YEAR
      * EVALUATE = COBOL's CASE/SWITCH statement
               WHEN "Freshmen"
                   MOVE 1 TO WS-CURRENT-YEAR
               WHEN "Sophomore"
                   MOVE 2 TO WS-CURRENT-YEAR
               WHEN "Junior"
                   MOVE 3 TO WS-CURRENT-YEAR
               WHEN "Senior"
                   MOVE 4 TO WS-CURRENT-YEAR
               WHEN OTHER
      * Unknown year level - skip this record
                   MOVE 0 TO WS-CURRENT-YEAR
           END-EVALUATE

           IF WS-CURRENT-YEAR > 0
      * Valid year level found
               ADD 1 TO NUM-STUDENTS(WS-CURRENT-YEAR)
      * Increment student count for this year

               MOVE NUM-STUDENTS(WS-CURRENT-YEAR)
                   TO WS-CURRENT-STU
      * Get the student slot number

               ADD 1 TO GRAND-STUDENTS
      * Increment total student count

      * Store grades in 2D array
      * Access: GRADE(year-level, student-number)
               MOVE FUNCTION NUMVAL(WS-TRIMMED-PRELIM)
                   TO PRELIM-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
               MOVE FUNCTION NUMVAL(WS-TRIMMED-MIDTERM)
                   TO MIDTERM-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
               MOVE FUNCTION NUMVAL(WS-TRIMMED-FINAL)
                   TO FINAL-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
           END-IF.

       COMPUTE-RESULTS.
      *----------------------------------------------------------------
      * COMPUTE-RESULTS - Calculate averages and pass/fail counts
      * Nested loops: outer = year levels, inner = students
      * For each student:
      *   1. Calculate average grade
      *   2. Accumulate sums for year-level averages
      *   3. Determine pass/fail status
      *----------------------------------------------------------------
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 4
      * Loop through 4 year levels
               PERFORM VARYING S FROM 1 BY 1
                   UNTIL S > NUM-STUDENTS(Y)
      * Loop through students in this year level

                   COMPUTE AVERAGE-GRADE(Y, S) =
                       ( PRELIM-GRADE(Y, S)
                       + MIDTERM-GRADE(Y, S)
                       + FINAL-GRADE(Y, S) ) / 3
      * Calculate student's average

      * Accumulate sums for year-level averages
                   ADD PRELIM-GRADE(Y, S)  TO SUM-PRELIM(Y)
                   ADD MIDTERM-GRADE(Y, S) TO SUM-MIDTERM(Y)
                   ADD FINAL-GRADE(Y, S)   TO SUM-FINAL(Y)
                   ADD AVERAGE-GRADE(Y, S) TO SUM-AVG(Y)

      * Determine pass/fail
                   IF AVERAGE-GRADE(Y, S) >= PASSING-GRADE
      * Student passed (average >= 75)
                       ADD 1 TO PASSED-COUNT(Y)
                       ADD 1 TO GRAND-PASSED
                   ELSE
      * Student failed (average < 75)
                       ADD 1 TO FAILED-COUNT(Y)
                       ADD 1 TO GRAND-FAILED
                   END-IF
               END-PERFORM
           END-PERFORM.

       WRITE-REPORT.
      *----------------------------------------------------------------
      * WRITE-REPORT - Generate formatted grade report
      * Writes headers, year-level summaries, and grand totals.
      *----------------------------------------------------------------
      * Write report headers
           WRITE STUDENT-REPORT-REC FROM WS-HEADER1
           WRITE STUDENT-REPORT-REC FROM WS-HEADER2
           WRITE STUDENT-REPORT-REC FROM WS-HEADER3
           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
           WRITE STUDENT-REPORT-REC FROM WS-COL-HDR1
           WRITE STUDENT-REPORT-REC FROM WS-COL-HDR2
           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE

      * Write data row for each year level
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 4
               MOVE SPACES TO WS-DETAIL-LINE
      * Clear the detail line

               IF NUM-STUDENTS(Y) > 0
      * Calculate averages for this year level
                   COMPUTE WS-AVG-PRELIM = 
                       SUM-PRELIM(Y) / NUM-STUDENTS(Y)
                   COMPUTE WS-AVG-MIDTERM = 
                       SUM-MIDTERM(Y) / NUM-STUDENTS(Y)
                   COMPUTE WS-AVG-FINAL = 
                       SUM-FINAL(Y) / NUM-STUDENTS(Y)
                   COMPUTE WS-AVG-AVERAGE = 
                       SUM-AVG(Y) / NUM-STUDENTS(Y)
               ELSE
      * No students in this year level
                   MOVE 0 TO WS-AVG-PRELIM
                   MOVE 0 TO WS-AVG-MIDTERM
                   MOVE 0 TO WS-AVG-FINAL
                   MOVE 0 TO WS-AVG-AVERAGE
               END-IF

      * Populate detail line
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

      * Write grand totals row
           MOVE GRAND-STUDENTS TO WS-TL-COUNT
           MOVE GRAND-PASSED TO WS-TL-PASSED
           MOVE GRAND-FAILED TO WS-TL-FAILED
           WRITE STUDENT-REPORT-REC FROM WS-TOTAL-LINE.
