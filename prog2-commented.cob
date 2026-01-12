      *================================================================
      * PROGRAM: STUDENT GRADING SYSTEM
      *================================================================
      * PURPOSE: This program processes student grades and generates
      *          a report showing pass/fail statistics per year level.
      *
      * HOW IT WORKS:
      *   1. Reads student grades from STUDENT-INPUT.TXT
      *   2. Calculates average grade for each student
      *   3. Determines if each student passed or failed
      *   4. Generates a summary report by year level
      *
      * GRADING RULES:
      *   - Average = (Prelim + Midterm + Final) / 3
      *   - Pass if Average >= 75
      *   - Fail if Average < 75
      *
      * INPUT FILE FORMAT (CSV):
      *   YearLevel,StudentNumber,PrelimGrade,MidtermGrade,FinalGrade
      *   Example: Freshmen,1,85,80,88
      *
      * YEAR LEVELS: Freshmen, Sophomore, Junior, Senior
      *================================================================

       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------
      * IDENTIFICATION DIVISION - Required first division
      * Provides basic identification information about the program.
      * Think of it as the "title page" of your program.
      *----------------------------------------------------------------
       PROGRAM-ID. STUDENT-GRADING-SYSTEM.
      * PROGRAM-ID is REQUIRED - gives your program a unique name
      * This name is used when compiling and running the program
      * Best practice: Use descriptive names that explain what it does

       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------
      * ENVIRONMENT DIVISION - Connects program to external world
      * Defines the files and devices the program will use.
      * This is where we "link" our program to actual files on disk.
      *----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * FILE-CONTROL lists all files used by the program

           SELECT STUDENT-INPUT-FILE ASSIGN TO "STUDENT-INPUT.TXT"
      * SELECT creates a connection between:
      *   - STUDENT-INPUT-FILE (logical name in COBOL code)
      *   - "STUDENT-INPUT.TXT" (physical file on your computer)
               ORGANIZATION IS LINE SEQUENTIAL.
      * LINE SEQUENTIAL = Text file, one record per line
      * Each line ends with a newline character

           SELECT STUDENT-REPORT-FILE ASSIGN TO "STUDENT-REPORT.TXT"
      * Output file for our generated report
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
      *----------------------------------------------------------------
      * DATA DIVISION - Defines ALL data used in the program
      * COBOL requires every variable to be declared before use.
      * This section is like declaring variables at the top of a function,
      * but for the ENTIRE program.
      *----------------------------------------------------------------
       FILE SECTION.
      * FILE SECTION describes the structure of file records

       FD  STUDENT-INPUT-FILE.
      * FD = File Description for input file
       01  STUDENT-INPUT-REC PIC X(100).
      * Each input line can be up to 100 characters
      * PIC X(100) = 100 alphanumeric characters

       FD  STUDENT-REPORT-FILE.
       01  STUDENT-REPORT-REC PIC X(120).
      * Output lines can be up to 120 characters (wider for report)

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * WORKING-STORAGE SECTION - Program's internal variables
      * All variables declared here persist throughout program execution.
      * They are initialized when the program starts.
      *----------------------------------------------------------------

       01  PASSING-GRADE        PIC 99 VALUE 75.
      * The minimum grade required to pass
      * PIC 99 = 2-digit number (00-99)
      * VALUE 75 = Initialize to 75 when program starts
      * This is a CONSTANT - we never change it, just compare against it

       01  WS-EOF-FLAG          PIC 9 VALUE 0.
      * End Of File indicator
      *   0 = More data available to read
      *   1 = End of file reached, stop reading
      * We check this flag in our read loop

       01  YEAR-LEVELS.
      * Group to store the names of year levels
           05 YEAR-NAME OCCURS 4 TIMES PIC X(10).
      * OCCURS 4 TIMES = Array with 4 elements
      * This will hold: "Freshmen", "Sophomore", "Junior", "Senior"
      * Access using subscript: YEAR-NAME(1), YEAR-NAME(2), etc.
      *
      * WHY USE ARRAYS (OCCURS)?
      * Instead of creating 4 separate variables, we create ONE
      * that holds 4 values. This lets us use loops to process
      * all year levels with the same code.

       01  STUDENT-COUNTS.
      * Tracks how many students are in each year level
           05 NUM-STUDENTS OCCURS 4 TIMES PIC 999 VALUE 0.
      * Array of 4 counters, each can hold 0-999
      * NUM-STUDENTS(1) = count of Freshmen
      * NUM-STUDENTS(2) = count of Sophomores, etc.
      * VALUE 0 = All start at zero

       01  STUDENT-TABLE.
      *----------------------------------------------------------------
      * STUDENT-TABLE - Main data structure for storing grades
      * This is a TWO-DIMENSIONAL ARRAY (table within a table)
      * Structure: 4 year levels × 50 students × 4 grade fields
      *
      * Think of it like a spreadsheet:
      *   - 4 sheets (one per year level)
      *   - Each sheet has 50 rows (students)
      *   - Each row has 4 columns (prelim, midterm, final, average)
      *----------------------------------------------------------------
           05 YEAR-TABLE OCCURS 4 TIMES.
      * First dimension: 4 year levels
      * YEAR-TABLE(1) = all Freshmen data
      * YEAR-TABLE(2) = all Sophomore data, etc.

              10 STUDENT-RECORD OCCURS 50 TIMES.
      * Second dimension: up to 50 students per year level
      * STUDENT-RECORD(1,1) = first Freshman
      * STUDENT-RECORD(1,2) = second Freshman
      * STUDENT-RECORD(2,1) = first Sophomore, etc.

                 15 PRELIM-GRADE   PIC 999V99.
      * Prelim exam grade
      * PIC 999V99 = 3 digits, implied decimal, 2 decimal places
      * Can hold 0.00 to 999.99
      * The V is an IMPLIED decimal - doesn't take storage space
      * but COBOL knows where the decimal point is

                 15 MIDTERM-GRADE  PIC 999V99.
      * Midterm exam grade

                 15 FINAL-GRADE    PIC 999V99.
      * Final exam grade

                 15 AVERAGE-GRADE  PIC 999V99.
      * Calculated average: (Prelim + Midterm + Final) / 3
      *
      * To access a specific grade:
      *   PRELIM-GRADE(2, 3) = Prelim grade of 3rd Sophomore
      *   AVERAGE-GRADE(4, 1) = Average of 1st Senior

       01  YEAR-TOTALS.
      * Counts of passed/failed students per year level
           05 PASSED-COUNT OCCURS 4 TIMES PIC 999 VALUE 0.
      * How many students passed in each year level
           05 FAILED-COUNT OCCURS 4 TIMES PIC 999 VALUE 0.
      * How many students failed in each year level

       01  YEAR-SUMS.
      *----------------------------------------------------------------
      * YEAR-SUMS - Accumulates grade totals for calculating averages
      * We sum all grades per year level, then divide by student count
      * to get the average grade for that year level.
      *----------------------------------------------------------------
           05 SUM-PRELIM   OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
      * Sum of all prelim grades for each year level
      * PIC 9(7)V99 = Larger size because we're summing many grades
      * Can hold up to 9,999,999.99

           05 SUM-MIDTERM  OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
      * Sum of all midterm grades

           05 SUM-FINAL    OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
      * Sum of all final grades

           05 SUM-AVG      OCCURS 4 TIMES PIC 9(7)V99 VALUE 0.
      * Sum of all average grades

       01  GRAND-TOTALS.
      * Overall totals across ALL year levels
           05 GRAND-STUDENTS PIC 999 VALUE 0.
      * Total number of all students (all years combined)
           05 GRAND-PASSED   PIC 999 VALUE 0.
      * Total students who passed (all years)
           05 GRAND-FAILED   PIC 999 VALUE 0.
      * Total students who failed (all years)

       01  Y PIC 9.
      * Loop counter for year levels (1 to 4)
      * Used in PERFORM VARYING loops
      * Single digit is enough since we only have 4 year levels

       01  S PIC 99.
      * Loop counter for students (1 to 50)
      * Two digits because we can have up to 50 students

       01  WS-TXT-PARSE.
      *----------------------------------------------------------------
      * WS-TXT-PARSE - Variables for parsing input lines
      * When we read "Freshmen,1,85,80,88" from the file,
      * we need to split it into separate pieces.
      *----------------------------------------------------------------
           05 WS-TXT-LINE       PIC X(100).
      * Holds the entire line read from the file
      * Example: "Freshmen,1,85,80,88"

           05 WS-TXT-YEAR       PIC X(15).
      * After parsing: holds year level (e.g., "Freshmen")

           05 WS-TXT-DUMMY      PIC X(10).
      * After parsing: holds student number
      * Called "DUMMY" because we don't actually use this value
      * We just need to capture it during UNSTRING

           05 WS-TXT-PRELIM     PIC X(10).
      * After parsing: holds prelim grade as TEXT (e.g., "85")
      * Note: This is text, not a number yet!

           05 WS-TXT-MIDTERM    PIC X(10).
      * After parsing: holds midterm grade as text

           05 WS-TXT-FINAL      PIC X(10).
      * After parsing: holds final grade as text

       01  WS-AVG-PRELIM   PIC 999V99.
      * Working variable for calculated year-level average prelim
      * Used when preparing data for the report

       01  WS-AVG-MIDTERM  PIC 999V99.
      * Working variable for year-level average midterm

       01  WS-AVG-FINAL    PIC 999V99.
      * Working variable for year-level average final

       01  WS-AVG-AVERAGE  PIC 999V99.
      * Working variable for year-level overall average

       01  WS-CURRENT-YEAR PIC 9 VALUE 0.
      * Tracks which year level we're currently processing
      * Set based on the year level text we read from input
      * 1=Freshmen, 2=Sophomore, 3=Junior, 4=Senior, 0=Invalid

       01  WS-CURRENT-STU  PIC 99 VALUE 0.
      * Tracks the current student number within a year level
      * Used as the second subscript when storing grades

       01  WS-OUTPUT-LINES.
      *----------------------------------------------------------------
      * WS-OUTPUT-LINES - Report layout templates
      * Each 05-level item represents one line (or line template)
      * in our output report. We fill in the variable parts and
      * write the whole line to the file.
      *----------------------------------------------------------------
           05 WS-HEADER1.
      * First header line - University name (centered)
              10 FILLER         PIC X(26) VALUE SPACES.
      * FILLER = Anonymous variable (no name needed)
      * 26 spaces to center the text
              10 FILLER         PIC X(42) 
                 VALUE "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES".
      * The actual university name

           05 WS-HEADER2.
      * Second header line - College name
              10 FILLER         PIC X(25) VALUE SPACES.
              10 FILLER         PIC X(50) 
                 VALUE "COLLEGE OF COMPUTER AND INFORMATION SCIENCE".

           05 WS-HEADER3.
      * Third header line - System name
              10 FILLER         PIC X(32) VALUE SPACES.
              10 FILLER         PIC X(22) 
                 VALUE "STUDENT GRADING SYSTEM".

           05 WS-BLANK-LINE     PIC X(90) VALUE SPACES.
      * A completely blank line for spacing in the report
      * We write this between sections for readability

           05 WS-COL-HDR1.
      * Column headers - First row
      * Each FILLER defines one column with specific width
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
      * Column headers - Second row (sub-headers)
              10 FILLER         PIC X(12) VALUE SPACES.
              10 FILLER         PIC X(8) VALUE "STUDENTS".
              10 FILLER         PIC X(2) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".
              10 FILLER         PIC X(5) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".
              10 FILLER         PIC X(19) VALUE SPACES.
              10 FILLER         PIC X(5) VALUE "GRADE".

           05 WS-DETAIL-LINE.
      *----------------------------------------------------------------
      * WS-DETAIL-LINE - Template for each year level's data row
      * Variables (WS-DL-*) will be filled with actual values
      * FILLERs provide spacing between columns
      *----------------------------------------------------------------
              10 WS-DL-YEAR     PIC X(10).
      * Year level name (e.g., "Freshmen")

              10 FILLER         PIC X(4) VALUE SPACES.
      * Spacing

              10 WS-DL-COUNT    PIC Z9.
      * Number of students in this year level
      * PIC Z9 = Zero suppression
      *   Z = Leading zeros become spaces
      *   9 = Always show this digit
      * So "03" displays as " 3" and "10" displays as "10"

              10 FILLER         PIC X(6) VALUE SPACES.

              10 WS-DL-PRELIM   PIC Z9.99.
      * Average prelim grade for this year level
      * PIC Z9.99 = One digit with zero suppression, decimal, 2 digits
      * Example: 82.33 displays as "82.33"

              10 FILLER         PIC X(5) VALUE SPACES.

              10 WS-DL-MIDTERM  PIC Z9.99.
      * Average midterm grade

              10 FILLER         PIC X(7) VALUE SPACES.

              10 WS-DL-FINAL    PIC Z9.99.
      * Average final grade

              10 FILLER         PIC X(7) VALUE SPACES.

              10 WS-DL-AVG      PIC Z9.99.
      * Overall average grade for this year level

              10 FILLER         PIC X(6) VALUE SPACES.

              10 WS-DL-PASSED   PIC Z9.
      * Number of students who passed

              10 FILLER         PIC X(10) VALUE SPACES.

              10 WS-DL-FAILED   PIC Z9.
      * Number of students who failed

           05 WS-TOTAL-LINE.
      *----------------------------------------------------------------
      * WS-TOTAL-LINE - Template for the grand totals row
      * Shows totals across all year levels
      * Note: No grade averages in total row (just counts)
      *----------------------------------------------------------------
              10 FILLER         PIC X(10) VALUE "TOTAL".
      * Literal label "TOTAL"

              10 FILLER         PIC X(4) VALUE SPACES.

              10 WS-TL-COUNT    PIC Z9.
      * Total number of all students

              10 FILLER         PIC X(51) VALUE SPACES.
      * Large space (no grade columns in total row)

              10 WS-TL-PASSED   PIC Z9.
      * Total students who passed (all years)

              10 FILLER         PIC X(10) VALUE SPACES.

              10 WS-TL-FAILED   PIC Z9.
      * Total students who failed (all years)

       PROCEDURE DIVISION.
      *================================================================
      * PROCEDURE DIVISION - The executable program logic
      * Everything above was DECLARATIONS (setting up data).
      * Now we write the actual INSTRUCTIONS.
      *
      * COBOL executes statements sequentially (top to bottom).
      * We use PERFORM to call paragraphs (like functions).
      * Each paragraph is a named block of code.
      *================================================================

       MAIN.
      *----------------------------------------------------------------
      * MAIN - Program entry point and flow control
      * This is like main() in C/Java - where execution begins.
      * We organize the program into logical steps using PERFORM.
      *----------------------------------------------------------------
           PERFORM INIT-YEAR-NAMES
      * Step 1: Set up year level names and open files
      * PERFORM = Execute the named paragraph, then return here

           PERFORM READ-INPUT-FILE
      * Step 2: Read all student data from the input file

           PERFORM COMPUTE-RESULTS
      * Step 3: Calculate averages and determine pass/fail

           PERFORM WRITE-REPORT
      * Step 4: Generate the output report

           CLOSE STUDENT-REPORT-FILE
      * Step 5: Close the output file
      * Always close files when done!

           DISPLAY "Report generated: STUDENT-REPORT.TXT"
      * DISPLAY = Print message to the screen (console)
      * Confirms to user that program completed successfully

           STOP RUN.
      * STOP RUN = End the program
      * Returns control to the operating system

       INIT-YEAR-NAMES.
      *----------------------------------------------------------------
      * INIT-YEAR-NAMES - Initialize year level names and open files
      * We store the year names in an array so we can:
      *   1. Look them up by index (1=Freshmen, 2=Sophomore, etc.)
      *   2. Use them in the report output
      *----------------------------------------------------------------
           MOVE "Freshmen" TO YEAR-NAME(1)
      * MOVE = Assignment statement
      * Store "Freshmen" in the first element of YEAR-NAME array
      * YEAR-NAME(1) = first element (COBOL arrays start at 1, not 0)

           MOVE "Sophomore" TO YEAR-NAME(2)
      * Store "Sophomore" in second element

           MOVE "Junior" TO YEAR-NAME(3)
           MOVE "Senior" TO YEAR-NAME(4)

           OPEN INPUT STUDENT-INPUT-FILE
      * OPEN INPUT = Open file for READING
      * The file must exist or you'll get an error
      * After opening, we can use READ to get data

           OPEN OUTPUT STUDENT-REPORT-FILE.
      * OPEN OUTPUT = Open file for WRITING
      * If file exists, it will be OVERWRITTEN
      * If it doesn't exist, it will be CREATED

       READ-INPUT-FILE.
      *----------------------------------------------------------------
      * READ-INPUT-FILE - Read all student records from input file
      * This paragraph reads the file line by line until EOF.
      * For each line, it calls PARSE-INPUT-LINE to process it.
      *
      * The loop continues until WS-EOF-FLAG becomes 1,
      * which happens when we try to read past the last line.
      *----------------------------------------------------------------
           MOVE 0 TO WS-EOF-FLAG
      * Reset the end-of-file flag to 0 (not at end)
      * This ensures we start fresh

           PERFORM UNTIL WS-EOF-FLAG = 1
      * PERFORM UNTIL = Loop that continues until condition is TRUE
      * This is like "while (WS-EOF-FLAG != 1)" in other languages
      * The loop body is everything until END-PERFORM

               READ STUDENT-INPUT-FILE INTO WS-TXT-LINE
      * READ = Get the next record (line) from the file
      * INTO WS-TXT-LINE = Store the line in this variable
      * Each READ automatically advances to the next line

                   AT END
      * AT END = What to do when there's no more data
      * This triggers when we try to read past the last line
                       MOVE 1 TO WS-EOF-FLAG
      * Set flag to 1 to signal "end of file"
      * The loop will exit on the next iteration check

                   NOT AT END
      * NOT AT END = What to do when we successfully read a line
                       PERFORM PARSE-INPUT-LINE
      * Process the line we just read
      * This extracts the data and stores it in our tables
               END-READ
      * END-READ closes the READ statement block
           END-PERFORM
      * END-PERFORM closes the loop

           CLOSE STUDENT-INPUT-FILE.
      * CLOSE = We're done reading this file
      * Always close files when finished to:
      *   1. Release system resources
      *   2. Allow other programs to access the file

       PARSE-INPUT-LINE.
      *----------------------------------------------------------------
      * PARSE-INPUT-LINE - Extract data from a CSV line
      * Input format: "YearLevel,StudentNum,Prelim,Midterm,Final"
      * Example: "Freshmen,1,85,80,88"
      *
      * Steps:
      *   1. Split the line by commas (UNSTRING)
      *   2. Determine which year level (EVALUATE)
      *   3. Convert text grades to numbers (NUMVAL)
      *   4. Store in the appropriate table position
      *----------------------------------------------------------------
           UNSTRING WS-TXT-LINE DELIMITED BY ","
      * UNSTRING = Split a string into parts
      * DELIMITED BY "," = Use comma as the separator
      * This is like String.split(",") in Java/JavaScript
               INTO WS-TXT-YEAR
      * First part goes here (e.g., "Freshmen")
                    WS-TXT-DUMMY
      * Second part goes here (e.g., "1" - student number)
      * We call it DUMMY because we don't use this value
                    WS-TXT-PRELIM
      * Third part = prelim grade (e.g., "85")
                    WS-TXT-MIDTERM
      * Fourth part = midterm grade (e.g., "80")
                    WS-TXT-FINAL
      * Fifth part = final grade (e.g., "88")
           END-UNSTRING

      *----------------------------------------------------------------
      * EVALUATE - COBOL's version of switch/case statement
      * We check the year level text and set a numeric code
      * This lets us use the year level as an array index
      *----------------------------------------------------------------
           EVALUATE FUNCTION TRIM(WS-TXT-YEAR)
      * FUNCTION TRIM = Remove leading/trailing spaces
      * Important because UNSTRING might leave extra spaces

               WHEN "Freshmen"
                   MOVE 1 TO WS-CURRENT-YEAR
      * If year is "Freshmen", set index to 1

               WHEN "Sophomore"
                   MOVE 2 TO WS-CURRENT-YEAR
      * If year is "Sophomore", set index to 2

               WHEN "Junior"
                   MOVE 3 TO WS-CURRENT-YEAR

               WHEN "Senior"
                   MOVE 4 TO WS-CURRENT-YEAR

               WHEN OTHER
                   MOVE 0 TO WS-CURRENT-YEAR
      * WHEN OTHER = Default case (like "default:" in switch)
      * If year level doesn't match any known value, set to 0
      * This handles invalid data gracefully
           END-EVALUATE

           IF WS-CURRENT-YEAR > 0
      * Only process if we recognized the year level
      * If WS-CURRENT-YEAR is 0, we skip this student

               ADD 1 TO NUM-STUDENTS(WS-CURRENT-YEAR)
      * Increment the student count for this year level
      * ADD 1 TO X is like X = X + 1 or X++

               MOVE NUM-STUDENTS(WS-CURRENT-YEAR) 
                   TO WS-CURRENT-STU
      * Get the new student count - this becomes our student index
      * If this is the 3rd Freshman, WS-CURRENT-STU = 3

               ADD 1 TO GRAND-STUDENTS
      * Also increment the total student count

               MOVE FUNCTION NUMVAL(WS-TXT-PRELIM)
                   TO PRELIM-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
      * FUNCTION NUMVAL = Convert text to number
      * "85" (text) becomes 85 (number)
      * Store in the 2D array using both subscripts:
      *   - WS-CURRENT-YEAR = which year level (1-4)
      *   - WS-CURRENT-STU = which student in that year (1-50)

               MOVE FUNCTION NUMVAL(WS-TXT-MIDTERM)
                   TO MIDTERM-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
      * Convert and store midterm grade

               MOVE FUNCTION NUMVAL(WS-TXT-FINAL)
                   TO FINAL-GRADE(WS-CURRENT-YEAR, WS-CURRENT-STU)
      * Convert and store final grade
           END-IF.

       COMPUTE-RESULTS.
      *----------------------------------------------------------------
      * COMPUTE-RESULTS - Calculate averages and pass/fail status
      * This paragraph processes all stored student data:
      *   1. Loops through each year level (1 to 4)
      *   2. For each year, loops through all students
      *   3. Calculates each student's average grade
      *   4. Determines if student passed or failed
      *   5. Accumulates sums for year-level averages
      *
      * FORMULA: Average = (Prelim + Midterm + Final) / 3
      * RULE: Pass if Average >= 75, Fail if Average < 75
      *----------------------------------------------------------------
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 4
      * PERFORM VARYING = For loop in COBOL
      *   FROM 1 = Start Y at 1
      *   BY 1 = Increment Y by 1 each iteration
      *   UNTIL Y > 4 = Stop when Y exceeds 4
      * This is like: for(Y=1; Y<=4; Y++)
      * Y represents the year level: 1=Freshmen, 2=Sophomore, etc.

               PERFORM VARYING S FROM 1 BY 1
                   UNTIL S > NUM-STUDENTS(Y)
      * NESTED LOOP - For each student in this year level
      * S goes from 1 to however many students are in year Y
      * NUM-STUDENTS(Y) tells us how many students in this year
      * If there are 3 Freshmen, this loops S = 1, 2, 3

                   COMPUTE AVERAGE-GRADE(Y, S) =
                       ( PRELIM-GRADE(Y, S)
                       + MIDTERM-GRADE(Y, S)
                       + FINAL-GRADE(Y, S) ) / 3
      * COMPUTE = Arithmetic calculation
      * Calculate average: (Prelim + Midterm + Final) / 3
      * Example: (85 + 80 + 88) / 3 = 84.33
      * Store result in AVERAGE-GRADE for this student
      * The (Y, S) subscripts identify the exact student

                   ADD PRELIM-GRADE(Y, S)  TO SUM-PRELIM(Y)
      * Add this student's prelim to the year's prelim sum
      * We'll use this later to calculate year-level averages

                   ADD MIDTERM-GRADE(Y, S) TO SUM-MIDTERM(Y)
      * Add midterm to year's midterm sum

                   ADD FINAL-GRADE(Y, S)   TO SUM-FINAL(Y)
      * Add final to year's final sum

                   ADD AVERAGE-GRADE(Y, S) TO SUM-AVG(Y)
      * Add this student's average to year's average sum

                   IF AVERAGE-GRADE(Y, S) >= PASSING-GRADE
      * Check if student passed (average >= 75)
      * PASSING-GRADE was set to 75 in WORKING-STORAGE
                       ADD 1 TO PASSED-COUNT(Y)
      * Increment passed count for this year level
                       ADD 1 TO GRAND-PASSED
      * Increment total passed count (all years)
                   ELSE
      * Student failed (average < 75)
                       ADD 1 TO FAILED-COUNT(Y)
      * Increment failed count for this year level
                       ADD 1 TO GRAND-FAILED
      * Increment total failed count (all years)
                   END-IF
               END-PERFORM
      * End of inner loop (students)
           END-PERFORM.
      * End of outer loop (year levels)

       WRITE-REPORT.
      *----------------------------------------------------------------
      * WRITE-REPORT - Generate the formatted output report
      * This paragraph creates the report file with:
      *   1. University/college headers (centered)
      *   2. Column headers
      *   3. Data rows (one per year level with averages)
      *   4. Totals row
      *
      * We use WRITE FROM to output each line.
      * The FROM clause specifies which template/variable to use.
      *----------------------------------------------------------------
           WRITE STUDENT-REPORT-REC FROM WS-HEADER1
      * WRITE = Output a record to the file
      * FROM WS-HEADER1 = Use data from this variable
      * This writes: "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES"

           WRITE STUDENT-REPORT-REC FROM WS-HEADER2
      * Write college name header

           WRITE STUDENT-REPORT-REC FROM WS-HEADER3
      * Write system name header

           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
      * Write 2 blank lines for spacing

           WRITE STUDENT-REPORT-REC FROM WS-COL-HDR1
      * Write column headers row 1

           WRITE STUDENT-REPORT-REC FROM WS-COL-HDR2
      * Write column headers row 2

           WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
      * Blank line before data rows

           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 4
      * Loop through all 4 year levels to write their data

               MOVE SPACES TO WS-DETAIL-LINE
      * Clear the detail line template
      * MOVE SPACES fills the entire variable with spaces
      * This ensures no leftover data from previous iteration

               IF NUM-STUDENTS(Y) > 0
      * Only calculate averages if there are students
      * Prevents division by zero error!

                   COMPUTE WS-AVG-PRELIM = 
                       SUM-PRELIM(Y) / NUM-STUDENTS(Y)
      * Calculate average prelim for this year level
      * Sum of all prelims / number of students
      * Example: (85+78+84) / 3 = 82.33

                   COMPUTE WS-AVG-MIDTERM = 
                       SUM-MIDTERM(Y) / NUM-STUDENTS(Y)
      * Calculate average midterm for this year level

                   COMPUTE WS-AVG-FINAL = 
                       SUM-FINAL(Y) / NUM-STUDENTS(Y)
      * Calculate average final for this year level

                   COMPUTE WS-AVG-AVERAGE = 
                       SUM-AVG(Y) / NUM-STUDENTS(Y)
      * Calculate average of all student averages
               ELSE
      * No students in this year level
                   MOVE 0 TO WS-AVG-PRELIM
                   MOVE 0 TO WS-AVG-MIDTERM
                   MOVE 0 TO WS-AVG-FINAL
                   MOVE 0 TO WS-AVG-AVERAGE
      * Set all averages to 0 to avoid garbage values
               END-IF

      * Now fill in the detail line template with calculated values
               MOVE YEAR-NAME(Y) TO WS-DL-YEAR
      * Copy year level name (e.g., "Freshmen")

               MOVE NUM-STUDENTS(Y) TO WS-DL-COUNT
      * Copy student count

               MOVE WS-AVG-PRELIM TO WS-DL-PRELIM
      * Copy average prelim (will be formatted by PIC Z9.99)

               MOVE WS-AVG-MIDTERM TO WS-DL-MIDTERM
               MOVE WS-AVG-FINAL TO WS-DL-FINAL
               MOVE WS-AVG-AVERAGE TO WS-DL-AVG
               MOVE PASSED-COUNT(Y) TO WS-DL-PASSED
               MOVE FAILED-COUNT(Y) TO WS-DL-FAILED

               WRITE STUDENT-REPORT-REC FROM WS-DETAIL-LINE
      * Write the completed data row to the file

               WRITE STUDENT-REPORT-REC FROM WS-BLANK-LINE
      * Write blank line after each year for readability
           END-PERFORM

      * Now write the grand totals row
           MOVE GRAND-STUDENTS TO WS-TL-COUNT
      * Copy total student count

           MOVE GRAND-PASSED TO WS-TL-PASSED
      * Copy total passed count

           MOVE GRAND-FAILED TO WS-TL-FAILED
      * Copy total failed count

           WRITE STUDENT-REPORT-REC FROM WS-TOTAL-LINE.
      * Write the totals row to complete the report
