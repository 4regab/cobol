      *================================================================
      * PROGRAM: EMPLOYEE PAYROLL SYSTEM WITH INPUT VALIDATION
      *================================================================
      * PURPOSE: This program calculates payroll for different types
      *          of employees (Faculty, Administrative, Utility, 
      *          Security) and generates a formatted report.
      *          Includes comprehensive input validation.
      *
      * HOW IT WORKS:
      *   1. Reads employee data from a text file (PAYROLL-INPUT.TXT)
      *   2. Validates each field before processing:
      *      - Employee Type: Must be alphabetical only
      *      - Employee Count: Must be numeric
      *      - Basic Pay: Must be numeric
      *   3. If validation fails, displays error and stops
      *   4. If validation passes, calculates payroll values
      *   5. Generates a report file (PAYROLL-REPORT.TXT)
      *
      * PAYROLL FORMULAS USED:
      *   - Basic Pay Total = Basic Pay × Number of Employees
      *   - Allowance = 10% of Basic Pay Total
      *   - Deduction = 12% of Basic Pay Total  
      *   - Gross Pay = Basic Pay Total + Allowance
      *   - Net Pay = Gross Pay - Deduction
      *
      * INPUT FILE FORMAT (CSV - Comma Separated Values):
      *   EmployeeType,NumberOfEmployees,BasicPay
      *   Example: Faculty,99,450
      *
      * VALIDATION RULES:
      *   - Employee Type: Alphabetic characters and spaces only
      *   - Employee Count: Valid numeric value
      *   - Basic Pay: Valid numeric value
      *
      * AUTHOR: GROUP2
      *================================================================

       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------
      * IDENTIFICATION DIVISION - Required in every COBOL program
      * This section identifies the program with basic metadata.
      *----------------------------------------------------------------
       PROGRAM-ID. EMPLOYEE-PAYROLL.
      * PROGRAM-ID is REQUIRED - gives the program a unique name
       AUTHOR. GROUP2.
      * AUTHOR is OPTIONAL - documents who wrote the program

       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------
      * ENVIRONMENT DIVISION - Describes the computing environment
      * Connects the program to external files on the system.
      *----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
      * INPUT-OUTPUT SECTION - Defines all file connections
       FILE-CONTROL.
      * FILE-CONTROL - Lists each file with its physical location

       SELECT PAYROLL-INPUT-FILE ASSIGN TO "PAYROLL-INPUT.TXT"
      * SELECT creates a file connection:
      *   - PAYROLL-INPUT-FILE = logical name used in COBOL code
      *   - "PAYROLL-INPUT.TXT" = actual filename on disk
            ORGANIZATION IS LINE SEQUENTIAL.
      * LINE SEQUENTIAL = each record is one line of text

       SELECT PAYROLL-OUTPUT-FILE ASSIGN TO "PAYROLL-REPORT.TXT"
      * Output file where the report will be written
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *----------------------------------------------------------------
      * DATA DIVISION - Defines all data structures and variables
      * Everything must be declared before use in COBOL.
      *----------------------------------------------------------------
       FILE SECTION.
      *----------------------------------------------------------------
      * FILE SECTION - Describes the structure of files
      *----------------------------------------------------------------
       FD  PAYROLL-INPUT-FILE.
      * FD = File Description for the input file
       01  PAYROLL-INPUT-RECORD        PIC X(100).
      * Record buffer - holds one line (up to 100 characters)

       FD  PAYROLL-OUTPUT-FILE.
       01  PAYROLL-OUTPUT-RECORDS     PIC X(120).
      * Output record - 120 chars to accommodate report width

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * WORKING-STORAGE SECTION - Program variables
      * Variables here persist throughout program execution.
      *----------------------------------------------------------------

       01  WS-EMPLOYEE-TYPES.
      * Array to store employee type names
               05 WS-EMP-TYPE            PIC X(15) OCCURS 4 TIMES.
      * OCCURS 4 TIMES = Array with 4 elements
      * Stores: "Faculty", "Administrative", "Utility", "Security"

       01  WS-EMPLOYEE-DATA.
      * Array to store employee counts and pay rates
               05  WS-EMP-ENTRY OCCURS 4 TIMES.
      * Each entry contains:
                   10 WS-NO-OF-EMPLOYEES PIC 999.
      * Number of employees (0-999)
                   10 WS-BASIC-PAY       PIC 9(7)V99.
      * Basic pay with 2 decimal places
      * V = implied decimal point (not stored physically)

       01  WS-CALCULATED-VALUES.
      * Array to store calculated payroll values
               05  WS-CALC-ENTRY OCCURS 4 TIMES.
                   10 ALLOWANCE          PIC 9(9)V99.
      * 10% of basic pay total
                   10 DEDUCTION          PIC 9(9)V99.
      * 12% of basic pay total
                   10 GROSS-PAY          PIC 9(9)V99.
      * Basic + Allowance
                   10 NET-PAY            PIC 9(9)V99.
      * Gross - Deduction
                   10 BASIC-PAY-TOTAL    PIC 9(9)V99.
      * Basic Pay × Number of Employees

       01  WS-ALL-TOTAL.
      * Grand totals across all employee types
               05 WS-AT-EMPLOYEES        PIC 9999 VALUE ZEROS.
      * VALUE ZEROS = Initialize to 0 at program start
               05 WS-AT-BASIC            PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-ALLOWANCE        PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-GROSS            PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-DEDUCTION        PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-NET              PIC 9(10)V99 VALUE ZEROS.

       01  WS-COUNTERS.
      * Loop counter for iterating through arrays
               05 WS-COUNTER             PIC 9 VALUE 1.

       01  WS-EOF-FLAG                   PIC 9 VALUE 0.
      * End-Of-File flag: 0 = more data, 1 = end reached

      *----------------------------------------------------------------
      * VALIDATION VARIABLES
      * These variables support the input validation logic.
      *----------------------------------------------------------------
       01  WS-VALIDATION.
               05 WS-VALID-FLAG          PIC 9 VALUE 0.
      * Per-field validation result: 0 = valid, 1 = invalid
               05 WS-HAS-ERROR           PIC 9 VALUE 0.
      * Global error flag: 0 = no errors, 1 = error found
      * If this is 1, report will NOT be generated
               05 WS-CHAR-INDEX          PIC 99 VALUE 0.
      * Loop counter for character-by-character validation
               05 WS-CURRENT-CHAR        PIC X(1).
      * Holds single character being validated
               05 WS-TRIMMED-TYPE        PIC X(15).
      * Employee type after removing leading/trailing spaces
               05 WS-TRIMMED-COUNT       PIC X(10).
      * Employee count as trimmed string (before numeric conversion)
               05 WS-TRIMMED-BASIC       PIC X(15).
      * Basic pay as trimmed string (before numeric conversion)

       01  WS-TXT-PARSE.
      * Variables for parsing CSV input lines
               05 WS-TXT-LINE            PIC X(100).
      * Entire line read from file
               05 WS-TXT-EMP-TYPE        PIC X(15).
      * First field after UNSTRING (employee type)
               05 WS-TXT-EMP-COUNT       PIC X(10).
      * Second field (employee count as text)
               05 WS-TXT-BASIC           PIC X(15).
      * Third field (basic pay as text)

      *----------------------------------------------------------------
      * REPORT LAYOUT VARIABLES
      * Define the visual structure of the output report.
      *----------------------------------------------------------------
       01  WS-OUTPUT-HDG.
               05 WS-HEADER1.
      * Company name header, centered
                   10 FILLER              PIC X(35) VALUE SPACES.
      * FILLER = anonymous variable for spacing
                   10 FILLER              PIC X(30)
                                      VALUE "ABCDEF TECHNOLOGY COMPANY".
               05 WS-HEADER2.
      * Report title
                   10 FILLER              PIC X(40) VALUE SPACES.
                   10 FILLER              PIC X(20) VALUE
                                            "EMPLOYEE PAYROLL".
               05 WS-BLANK-LINE           PIC X(100) VALUE SPACES.
      * Blank line for spacing in report

               05 WS-COLUMN-HEADER.
      * Column headers - row 1
                   10 FILLER           PIC X(16) VALUE "EMPLOYEE TYPE".
                   10 FILLER           PIC X(9) VALUE "NO. OF".
                   10 FILLER           PIC X(14) VALUE "BASIC PAY".
                   10 FILLER           PIC X(14) VALUE "ALLOWANCE".
                   10 FILLER           PIC X(14) VALUE "GROSS PAY".
                   10 FILLER           PIC X(14) VALUE "DEDUCTION".
                   10 FILLER           PIC X(14) VALUE "NET PAY".
               05 WS-COLUMN-HEADER2.
      * Column headers - row 2
                   10 FILLER           PIC X(16) VALUE SPACES.
                   10 FILLER           PIC X(9) VALUE "EMPLOYEES".
                   10 FILLER           PIC X(84) VALUE SPACES.

               05 WS-DETAIL-LINE.
      * Template for each data row
                   10 WS-DL-EMP-TYPE      PIC X(16).
                   10 WS-DL-NO-EMPS       PIC ZZ9.
      * Z = zero suppression (leading zeros become spaces)
                   10 FILLER              PIC X(6)  VALUE SPACES.
                   10 WS-DL-BASIC         PIC ZZZ,ZZ9.99.
      * Comma editing for thousands separator
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-ALLOWANCE     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-GROSS         PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-DEDUCTION     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-NET           PIC ZZZ,ZZ9.99.

               05 WS-TOTAL-LINE.
      * Template for grand totals row
                   10 FILLER              PIC X(16) VALUE "TOTAL".
                   10 WS-TL-NO-EMPS       PIC ZZ9.
                   10 FILLER              PIC X(6)  VALUE SPACES.
                   10 WS-TL-BASIC         PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-TL-ALLOWANCE     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-TL-GROSS         PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-TL-DEDUCTION     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-TL-NET           PIC ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
      *================================================================
      * PROCEDURE DIVISION - Executable program logic
      * Contains all the instructions that perform the actual work.
      *================================================================

       MAIN-PROCEDURE.
      *----------------------------------------------------------------
      * MAIN-PROCEDURE - Program entry point and control flow
      * Orchestrates the overall program execution.
      *----------------------------------------------------------------
           PERFORM INITIALIZE-EMP-TYPE.
      * Step 1: Open input and output files

           PERFORM READ-INPUT.
      * Step 2: Read and validate all input data

           IF WS-HAS-ERROR = 0
      * Step 3: Only proceed if NO validation errors occurred
               PERFORM CALCULATE-PAYROLL
      * Calculate payroll and generate report
           ELSE
      * Validation failed - do not generate report
               DISPLAY "Report not generated due to validation error/s."
               CLOSE PAYROLL-OUTPUT-FILE
      * Must close the file even on error
           END-IF.

           STOP RUN.
      * End program execution

       INITIALIZE-EMP-TYPE.
      *----------------------------------------------------------------
      * INITIALIZE-EMP-TYPE - Open files for processing
      *----------------------------------------------------------------
           OPEN INPUT PAYROLL-INPUT-FILE
      * Open for reading (file must exist)
           OPEN OUTPUT PAYROLL-OUTPUT-FILE.
      * Open for writing (creates or overwrites file)

       READ-INPUT.
      *----------------------------------------------------------------
      * READ-INPUT - Read all records from input file
      * Reads up to 4 employee type records.
      *----------------------------------------------------------------
           MOVE 1 TO WS-COUNTER
      * Start at record 1

           PERFORM UNTIL WS-EOF-FLAG = 1 OR WS-COUNTER > 4
      * Loop until end of file OR 4 records read
               READ PAYROLL-INPUT-FILE INTO WS-TXT-LINE
                   AT END
                       MOVE 1 TO WS-EOF-FLAG
      * No more data
                   NOT AT END
                       PERFORM PARSE-TXT-LINE
      * Process the line (includes validation)
                       ADD 1 TO WS-COUNTER
               END-READ
           END-PERFORM
           CLOSE PAYROLL-INPUT-FILE.

       PARSE-TXT-LINE.
      *----------------------------------------------------------------
      * PARSE-TXT-LINE - Parse and validate input line
      * Splits CSV line and validates each field before storing.
      * Uses nested IF structure to validate in sequence:
      *   1. Employee Type (alphabetic)
      *   2. Employee Count (numeric)
      *   3. Basic Pay (numeric)
      * If any validation fails, sets error flag and stops processing.
      *----------------------------------------------------------------
           UNSTRING WS-TXT-LINE DELIMITED BY ","
      * Split line by comma delimiter
               INTO WS-TXT-EMP-TYPE
                    WS-TXT-EMP-COUNT
                    WS-TXT-BASIC
           END-UNSTRING

      * Trim whitespace from all fields
           MOVE FUNCTION TRIM(WS-TXT-EMP-TYPE)
               TO WS-TRIMMED-TYPE
           MOVE FUNCTION TRIM(WS-TXT-EMP-COUNT)
               TO WS-TRIMMED-COUNT
           MOVE FUNCTION TRIM(WS-TXT-BASIC)
               TO WS-TRIMMED-BASIC

      * VALIDATION CHAIN - Each field validated in sequence
           PERFORM VALIDATE-EMP-TYPE
           IF WS-VALID-FLAG = 1
      * Employee type validation FAILED
               DISPLAY "ERROR: Invalid employee type at record "
                   WS-COUNTER ": " WS-TRIMMED-TYPE
               MOVE 1 TO WS-HAS-ERROR
      * Set global error flag - report will not be generated
           ELSE
      * Employee type is valid, check employee count
               PERFORM VALIDATE-EMP-COUNT
               IF WS-VALID-FLAG = 1
      * Employee count validation FAILED
                   DISPLAY "ERROR: Invalid employee count at record "
                       WS-COUNTER ": " WS-TRIMMED-COUNT
                   MOVE 1 TO WS-HAS-ERROR
               ELSE
      * Employee count is valid, check basic pay
                   PERFORM VALIDATE-BASIC-PAY
                   IF WS-VALID-FLAG = 1
      * Basic pay validation FAILED
                       DISPLAY "ERROR: Invalid basic pay at record "
                           WS-COUNTER ": " WS-TRIMMED-BASIC
                       MOVE 1 TO WS-HAS-ERROR
                   ELSE
      * ALL VALIDATIONS PASSED - Store the data
                       MOVE WS-TRIMMED-TYPE 
                           TO WS-EMP-TYPE(WS-COUNTER)
                       MOVE FUNCTION NUMVAL(WS-TRIMMED-COUNT)
                           TO WS-NO-OF-EMPLOYEES(WS-COUNTER)
      * NUMVAL converts text "99" to number 99
                       MOVE FUNCTION NUMVAL(WS-TRIMMED-BASIC)
                           TO WS-BASIC-PAY(WS-COUNTER)
                   END-IF
               END-IF
           END-IF.

       VALIDATE-EMP-TYPE.
      *----------------------------------------------------------------
      * VALIDATE-EMP-TYPE - Check if employee type is alphabetic
      * Algorithm: Linear search through each character
      * Time Complexity: O(n) where n = length of string
      * Valid characters: A-Z, a-z, and space
      *----------------------------------------------------------------
           MOVE 0 TO WS-VALID-FLAG
      * Assume valid until proven otherwise

           MOVE 1 TO WS-CHAR-INDEX
      * Start at first character

           PERFORM UNTIL WS-CHAR-INDEX > 
               FUNCTION LENGTH(FUNCTION TRIM(WS-TRIMMED-TYPE))
      * Loop through each character in the trimmed string
               MOVE WS-TRIMMED-TYPE(WS-CHAR-INDEX:1) 
                   TO WS-CURRENT-CHAR
      * Extract single character using reference modification
      * (WS-CHAR-INDEX:1) means "1 character starting at position"

               IF NOT (WS-CURRENT-CHAR IS ALPHABETIC 
                   OR WS-CURRENT-CHAR = SPACE)
      * IS ALPHABETIC = COBOL class test for A-Z, a-z
      * If character is NOT alphabetic AND NOT space, it's invalid
                   MOVE 1 TO WS-VALID-FLAG
      * Mark as invalid
               END-IF
               ADD 1 TO WS-CHAR-INDEX
      * Move to next character
           END-PERFORM.

       VALIDATE-EMP-COUNT.
      *----------------------------------------------------------------
      * VALIDATE-EMP-COUNT - Check if employee count is numeric
      * Uses TEST-NUMVAL intrinsic function for validation.
      * TEST-NUMVAL returns 0 if string is valid numeric, non-zero otherwise.
      *----------------------------------------------------------------
           MOVE 0 TO WS-VALID-FLAG
           IF FUNCTION TEST-NUMVAL(WS-TRIMMED-COUNT) IS NOT ZERO
      * TEST-NUMVAL returns 0 for valid numbers
      * Non-zero means the string cannot be converted to a number
               MOVE 1 TO WS-VALID-FLAG
           END-IF.

       VALIDATE-BASIC-PAY.
      *----------------------------------------------------------------
      * VALIDATE-BASIC-PAY - Check if basic pay is numeric
      * Same logic as VALIDATE-EMP-COUNT.
      *----------------------------------------------------------------
           MOVE 0 TO WS-VALID-FLAG
           IF FUNCTION TEST-NUMVAL(WS-TRIMMED-BASIC) IS NOT ZERO
               MOVE 1 TO WS-VALID-FLAG
           END-IF.

       CALCULATE-PAYROLL.
      *----------------------------------------------------------------
      * CALCULATE-PAYROLL - Perform all payroll calculations
      * Formulas:
      *   Basic Pay Total = Basic Pay × Number of Employees
      *   Allowance = 10% of Basic Pay Total
      *   Deduction = 12% of Basic Pay Total
      *   Gross Pay = Basic Pay Total + Allowance
      *   Net Pay = Gross Pay - Deduction
      *----------------------------------------------------------------
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 4
      * Loop through all 4 employee types

               COMPUTE BASIC-PAY-TOTAL(WS-COUNTER) =
                   WS-BASIC-PAY(WS-COUNTER) *
                   WS-NO-OF-EMPLOYEES(WS-COUNTER)
      * Total = Pay × Employees

               COMPUTE ALLOWANCE(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) * 0.10
      * 10% allowance

               COMPUTE DEDUCTION(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) * 0.12
      * 12% deduction

               COMPUTE GROSS-PAY(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) +
                   ALLOWANCE(WS-COUNTER)
      * Gross = Basic + Allowance

               COMPUTE NET-PAY(WS-COUNTER) =
                   GROSS-PAY(WS-COUNTER) - DEDUCTION(WS-COUNTER)
      * Net = Gross - Deduction

      * Accumulate grand totals
               ADD WS-NO-OF-EMPLOYEES(WS-COUNTER) TO WS-AT-EMPLOYEES
               ADD BASIC-PAY-TOTAL(WS-COUNTER) TO WS-AT-BASIC
               ADD ALLOWANCE(WS-COUNTER) TO WS-AT-ALLOWANCE
               ADD GROSS-PAY(WS-COUNTER) TO WS-AT-GROSS
               ADD DEDUCTION(WS-COUNTER) TO WS-AT-DEDUCTION
               ADD NET-PAY(WS-COUNTER) TO WS-AT-NET
           END-PERFORM.

           PERFORM WRITE-REPORT.

       WRITE-REPORT.
      *----------------------------------------------------------------
      * WRITE-REPORT - Generate the formatted payroll report
      * Writes headers, data rows, and totals to output file.
      *----------------------------------------------------------------
      * Write report headers
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-HEADER1
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-HEADER2
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-COLUMN-HEADER
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-COLUMN-HEADER2
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE

      * Write data rows for each employee type
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 4
               MOVE WS-EMP-TYPE(WS-COUNTER) TO WS-DL-EMP-TYPE
               MOVE WS-NO-OF-EMPLOYEES(WS-COUNTER) TO WS-DL-NO-EMPS
               MOVE BASIC-PAY-TOTAL(WS-COUNTER) TO WS-DL-BASIC
               MOVE ALLOWANCE(WS-COUNTER) TO WS-DL-ALLOWANCE
               MOVE GROSS-PAY(WS-COUNTER) TO WS-DL-GROSS
               MOVE DEDUCTION(WS-COUNTER) TO WS-DL-DEDUCTION
               MOVE NET-PAY(WS-COUNTER) TO WS-DL-NET
               WRITE PAYROLL-OUTPUT-RECORDS FROM WS-DETAIL-LINE
               WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           END-PERFORM.

      * Write totals row
           MOVE WS-AT-EMPLOYEES TO WS-TL-NO-EMPS
           MOVE WS-AT-BASIC TO WS-TL-BASIC
           MOVE WS-AT-ALLOWANCE TO WS-TL-ALLOWANCE
           MOVE WS-AT-GROSS TO WS-TL-GROSS
           MOVE WS-AT-DEDUCTION TO WS-TL-DEDUCTION
           MOVE WS-AT-NET TO WS-TL-NET
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-TOTAL-LINE

           CLOSE PAYROLL-OUTPUT-FILE.
      * Always close files when done

           DISPLAY "Report generated: PAYROLL-REPORT.TXT".
      * Confirm completion to user

       END PROGRAM EMPLOYEE-PAYROLL.
