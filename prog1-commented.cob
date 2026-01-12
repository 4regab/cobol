      *================================================================
      * PROGRAM: EMPLOYEE PAYROLL SYSTEM
      *================================================================
      * PURPOSE: This program calculates payroll for different types
      *          of employees (Faculty, Administrative, Utility, 
      *          Security) and generates a formatted report.
      *
      * HOW IT WORKS:
      *   1. Reads employee data from a text file (PAYROLL-INPUT.TXT)
      *   2. Calculates payroll values for each employee type
      *   3. Generates a report file (PAYROLL-REPORT.TXT)
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
      * AUTHOR: GROUP2
      *================================================================

       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------
      * IDENTIFICATION DIVISION - Required in every COBOL program
      * This section tells the computer basic info about the program.
      * Think of it like the cover page of a book - it has the title
      * and author information.
      *----------------------------------------------------------------
       PROGRAM-ID. EMPLOYEE-PAYROLL.
      * PROGRAM-ID is REQUIRED - it gives your program a unique name.
      * This name is used when you compile and run the program.
      * Rules: Must start with a letter, can have letters/numbers/hyphens
      * Maximum 30 characters in most COBOL compilers.

       AUTHOR. GROUP2.
      * AUTHOR is OPTIONAL - documents who wrote the program.
      * Good practice for team projects so others know who to ask
      * questions about the code.

       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------
      * ENVIRONMENT DIVISION - Describes the computing environment
      * This section connects your program to the outside world.
      * It tells COBOL what files you want to use and where they are.
      * Think of it as setting up the "bridges" between your program
      * and external resources like files on your hard drive.
      *----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
      * INPUT-OUTPUT SECTION - Specifically for file handling
      * This is where we define all the files our program will use.
      * Every file you want to read from or write to must be declared here.

       FILE-CONTROL.
      * FILE-CONTROL - The actual list of files
      * Each SELECT statement below creates a connection between:
      *   1. A logical name (used in your COBOL code)
      *   2. A physical file (the actual file on your computer)

       SELECT PAYROLL-INPUT-FILE ASSIGN TO "PAYROLL-INPUT.TXT"
      * SELECT creates a file connection:
      *   - PAYROLL-INPUT-FILE = the name we use in our COBOL code
      *   - "PAYROLL-INPUT.TXT" = the actual filename on disk
      * 
      * Why use different names? The logical name (PAYROLL-INPUT-FILE)
      * follows COBOL naming rules and is descriptive. The physical
      * filename can be anything your operating system allows.
            ORGANIZATION IS LINE SEQUENTIAL.
      * ORGANIZATION tells COBOL how the file is structured:
      *   - LINE SEQUENTIAL = each record is one line of text
      *   - Records are separated by newline characters
      *   - This is like a regular text file you'd open in Notepad
      * Other options: INDEXED (for databases), RELATIVE (by position)

       SELECT PAYROLL-OUTPUT-FILE ASSIGN TO "PAYROLL-REPORT.TXT"
      * Our output file - where the report will be written
      * Same concept as above, but this file will be CREATED by our
      * program (it doesn't need to exist beforehand)
            ORGANIZATION IS LINE SEQUENTIAL.
      * Also line sequential - we'll write one line at a time

       DATA DIVISION.
      *----------------------------------------------------------------
      * DATA DIVISION - The heart of data definition in COBOL
      * This is where we define ALL variables, records, and data
      * structures used in our program. COBOL is very strict about
      * data - everything must be declared before use.
      *
      * Think of this section as creating "containers" to hold data.
      * Each container has a specific size and type.
      *----------------------------------------------------------------
       FILE SECTION.
      *----------------------------------------------------------------
      * FILE SECTION - Describes the structure of our files
      * For each file in FILE-CONTROL, we need an FD (File Description)
      * that tells COBOL what each record (line) looks like.
      *----------------------------------------------------------------

       FD  PAYROLL-INPUT-FILE.
      * FD = File Description (or File Definition)
      * This describes the structure of PAYROLL-INPUT-FILE
      * The name must match exactly what we used in SELECT statement
       01  PAYROLL-INPUT-RECORD        PIC X(100).
      * 01 = Level number (01 is the highest, means "this is a record")
      * PAYROLL-INPUT-RECORD = name of the record variable
      * PIC X(100) = Picture clause defining the data type:
      *   - PIC = Picture (describes the format)
      *   - X = Alphanumeric (can hold letters, numbers, symbols)
      *   - (100) = 100 characters long
      * So this can hold any line up to 100 characters from our input file

       FD  PAYROLL-OUTPUT-FILE.
      * File Description for our output file
       01  PAYROLL-OUTPUT-RECORDS PIC X(120).
      * Each line we write can be up to 120 characters
      * We made it longer than input because our report has more columns

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------
      * WORKING-STORAGE SECTION - Program variables
      * This is where we declare all variables that our program uses
      * internally. Unlike FILE SECTION variables (which are for I/O),
      * these are for calculations, counters, temporary storage, etc.
      *
      * Variables here keep their values throughout program execution.
      * They are initialized when the program starts.
      *----------------------------------------------------------------

       01  WS-EMPLOYEE-TYPES.
      * 01 level = Group item (container for related data)
      * WS- prefix = Working Storage (common naming convention)
      * This group will hold the names of employee types
               05 WS-EMP-TYPE            PIC X(15) OCCURS 4 TIMES.
      * 05 level = subordinate to 01 (child of WS-EMPLOYEE-TYPES)
      * PIC X(15) = 15-character alphanumeric field
      * OCCURS 4 TIMES = This is an ARRAY with 4 elements!
      *
      * ARRAYS IN COBOL (OCCURS clause):
      * Instead of creating 4 separate variables like:
      *   WS-EMP-TYPE-1, WS-EMP-TYPE-2, WS-EMP-TYPE-3, WS-EMP-TYPE-4
      * We create ONE variable that holds 4 values.
      * Access them using subscripts: WS-EMP-TYPE(1), WS-EMP-TYPE(2), etc.
      *
      * This will store: "Faculty", "Administrative", "Utility", "Security"

       01  WS-EMPLOYEE-DATA.
      * Group to store employee counts and basic pay rates
               05  WS-EMP-ENTRY OCCURS 4 TIMES.
      * Another array with 4 elements, but this one is a GROUP
      * Each element contains multiple fields (defined below)
                   10 WS-NO-OF-EMPLOYEES PIC 999.
      * 10 level = subordinate to 05 (grandchild of 01)
      * PIC 999 = Numeric, 3 digits (0-999)
      * This holds how many employees of each type (e.g., 99 Faculty)
                   10 WS-BASIC-PAY       PIC 9(7)V99.
      * PIC 9(7)V99 breakdown:
      *   - 9(7) = 7 numeric digits before decimal
      *   - V = IMPLIED decimal point (not stored, just for alignment)
      *   - 99 = 2 numeric digits after decimal
      * Total: 9 digits, can hold up to 9,999,999.99
      * The V is important: the decimal point exists logically but
      * doesn't take up storage space. COBOL handles it automatically.

       01  WS-CALCULATED-VALUES.
      * Group to store all our calculated payroll values
               05  WS-CALC-ENTRY OCCURS 4 TIMES.
      * Array of 4 calculation records (one per employee type)
                   10 ALLOWANCE          PIC 9(9)V99.
      * Allowance amount - calculated as 10% of basic pay total
      * PIC 9(9)V99 = up to 999,999,999.99
                   10 DEDUCTION          PIC 9(9)V99.
      * Deduction amount - calculated as 12% of basic pay total
                   10 GROSS-PAY          PIC 9(9)V99.
      * Gross pay = Basic Pay Total + Allowance
                   10 NET-PAY            PIC 9(9)V99.
      * Net pay = Gross Pay - Deduction (what employee actually gets)
                   10 BASIC-PAY-TOTAL    PIC 9(9)V99.
      * Total basic pay = Basic Pay × Number of Employees

       01  WS-ALL-TOTAL.
      * Group for GRAND TOTALS (sum across all employee types)
               05 WS-AT-EMPLOYEES        PIC 9999 VALUE ZEROS.
      * Total count of ALL employees (all types combined)
      * VALUE ZEROS = Initialize to 0 when program starts
      * This is important! Without VALUE, the variable might contain
      * garbage data left over in memory.
               05 WS-AT-BASIC            PIC 9(10)V99 VALUE ZEROS.
      * Grand total of basic pay (larger PIC because it's a sum)
               05 WS-AT-ALLOWANCE        PIC 9(10)V99 VALUE ZEROS.
      * Grand total of all allowances
               05 WS-AT-GROSS            PIC 9(10)V99 VALUE ZEROS.
      * Grand total of all gross pay
               05 WS-AT-DEDUCTION        PIC 9(10)V99 VALUE ZEROS.
      * Grand total of all deductions
               05 WS-AT-NET              PIC 9(10)V99 VALUE ZEROS.
      * Grand total of all net pay

       01  WS-COUNTERS.
      * Group for loop counters and control variables
               05 WS-COUNTER             PIC 9 VALUE 1.
      * Loop counter - single digit (1-9)
      * Used to iterate through our arrays
      * VALUE 1 = Start at 1 (COBOL arrays typically start at 1, not 0)

       01  WS-EOF-FLAG                   PIC 9 VALUE 0.
      * EOF = End Of File flag
      * This is a BOOLEAN-like variable (COBOL doesn't have true booleans)
      *   0 = There is more data to read
      *   1 = We've reached the end of the file
      * We check this flag to know when to stop reading

       01  WS-TXT-PARSE.
      * Group for parsing (splitting) input text lines
      * When we read a line like "Faculty,99,450", we need to
      * break it into separate pieces
               05 WS-TXT-LINE            PIC X(100).
      * Holds the entire line read from the file
               05 WS-TXT-EMP-TYPE        PIC X(15).
      * After parsing: holds "Faculty"
               05 WS-TXT-EMP-COUNT       PIC X(10).
      * After parsing: holds "99" (as text, not number yet)
               05 WS-TXT-BASIC           PIC X(15).
      * After parsing: holds "450" (as text, not number yet)
      * Note: These are all X (alphanumeric) because UNSTRING
      * works with text. We convert to numbers later using NUMVAL.

       01  WS-OUTPUT-HDG.
      *----------------------------------------------------------------
      * REPORT LAYOUT SECTION
      * This group defines the visual structure of our report.
      * Each 05-level item is one line (or template) in the report.
      * FILLER is used for spaces and literal text that doesn't change.
      *----------------------------------------------------------------
               05 WS-HEADER1.
      * First header line - Company name, centered
                   10 FILLER              PIC X(35) VALUE SPACES.
      * FILLER = Anonymous variable (we don't need to reference it by name)
      * PIC X(35) VALUE SPACES = 35 spaces (for centering)
                   10 FILLER              PIC X(30)
                                      VALUE "ABCDEF TECHNOLOGY COMPANY".
      * The actual company name text

               05 WS-HEADER2.
      * Second header line - Report title
                   10 FILLER              PIC X(40) VALUE SPACES.
      * 40 spaces for centering
                   10 FILLER              PIC X(20) VALUE
                                            "EMPLOYEE PAYROLL".
      * Report title

               05 WS-BLANK-LINE           PIC X(100) VALUE SPACES.
      * A completely blank line - used for spacing in the report
      * We write this between sections to make the report readable

               05 WS-COLUMN-HEADER.
      * Column headers - First row (main titles)
      * Each FILLER defines one column header with specific width
                   10 FILLER           PIC X(16) VALUE "EMPLOYEE TYPE".
                   10 FILLER           PIC X(9) VALUE "NO. OF".
                   10 FILLER           PIC X(14) VALUE "BASIC PAY".
                   10 FILLER           PIC X(14) VALUE "ALLOWANCE".
                   10 FILLER           PIC X(14) VALUE "GROSS PAY".
                   10 FILLER           PIC X(14) VALUE "DEDUCTION".
                   10 FILLER           PIC X(14) VALUE "NET PAY".

               05 WS-COLUMN-HEADER2.
      * Column headers - Second row (sub-titles)
                   10 FILLER           PIC X(16) VALUE SPACES.
      * Empty under "EMPLOYEE TYPE"
                   10 FILLER           PIC X(9) VALUE "EMPLOYEES".
      * "EMPLOYEES" goes under "NO. OF"
                   10 FILLER           PIC X(84) VALUE SPACES.
      * Rest is blank

               05 WS-DETAIL-LINE.
      *----------------------------------------------------------------
      * DETAIL LINE - Template for each data row
      * This is where actual employee data will be displayed.
      * Variables (not FILLER) will be filled with calculated values.
      *----------------------------------------------------------------
                   10 WS-DL-EMP-TYPE      PIC X(16).
      * Employee type name (e.g., "Faculty")
                   10 WS-DL-NO-EMPS       PIC ZZ9.
      * Number of employees with EDITING:
      *   Z = Zero suppression (leading zeros become spaces)
      *   So 099 displays as "  99" instead of "099"
      *   Makes numbers look cleaner in reports
                   10 FILLER              PIC X(6)  VALUE SPACES.
      * Spacing between columns
                   10 WS-DL-BASIC         PIC ZZZ,ZZ9.99.
      * Basic pay with COMMA EDITING:
      *   ZZZ,ZZ9.99 breakdown:
      *   - ZZZ = 3 digits with zero suppression
      *   - , = Literal comma (appears in output if needed)
      *   - ZZ9 = 2 more digits with suppression, then 1 required
      *   - . = Literal decimal point
      *   - 99 = 2 decimal places (always shown)
      *   Example: 44550.00 displays as " 44,550.00"
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-ALLOWANCE     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-GROSS         PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-DEDUCTION     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-NET           PIC ZZZ,ZZ9.99.

               05 WS-TOTAL-LINE.
      * TOTAL LINE - Template for the grand totals row
      * Similar to detail line but with "TOTAL" label
                   10 FILLER              PIC X(16) VALUE "TOTAL".
      * Literal "TOTAL" label
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
      * PROCEDURE DIVISION - The executable code
      * This is where the actual program logic lives. Everything above
      * was just DECLARATIONS (setting up data structures).
      * Now we write the INSTRUCTIONS that tell the computer what to do.
      *
      * COBOL executes statements in order, top to bottom.
      * We use PERFORM to call paragraphs (like functions in other languages).
      *================================================================

       MAIN-PROCEDURE.
      *----------------------------------------------------------------
      * MAIN-PROCEDURE - The main entry point
      * This is like the "main()" function in C or Java.
      * It controls the overall flow of the program.
      * We break the work into smaller paragraphs for organization.
      *----------------------------------------------------------------
           PERFORM INITIALIZE-EMP-TYPE.
      * PERFORM = Call/execute another paragraph
      * This runs the INITIALIZE-EMP-TYPE paragraph, then returns here
      * Step 1: Open the files we need

           PERFORM READ-INPUT.
      * Step 2: Read all data from the input file

           PERFORM CALCULATE-PAYROLL.
      * Step 3: Do all calculations and write the report

           STOP RUN.
      * STOP RUN = End the program
      * This is how COBOL programs terminate
      * Control returns to the operating system

       INITIALIZE-EMP-TYPE.
      *----------------------------------------------------------------
      * INITIALIZE-EMP-TYPE - Open files for processing
      * Before we can read or write files, we must OPEN them.
      * This is like double-clicking a file to open it.
      *----------------------------------------------------------------
           OPEN INPUT PAYROLL-INPUT-FILE
      * OPEN INPUT = Open file for READING only
      * The file must exist, or you'll get an error
           OPEN OUTPUT PAYROLL-OUTPUT-FILE.
      * OPEN OUTPUT = Open file for WRITING
      * If the file exists, it will be OVERWRITTEN (erased and recreated)
      * If it doesn't exist, it will be CREATED

       READ-INPUT.
      *----------------------------------------------------------------
      * READ-INPUT - Read all records from the input file
      * This paragraph reads the file line by line until:
      *   1. We reach the end of the file (EOF), OR
      *   2. We've read 4 employee types (our maximum)
      *----------------------------------------------------------------
           MOVE 1 TO WS-COUNTER
      * MOVE = Assignment statement (like = in other languages)
      * Set counter to 1 (we'll read employee types 1, 2, 3, 4)

           PERFORM UNTIL WS-EOF-FLAG = 1 OR WS-COUNTER > 4
      * PERFORM UNTIL = Loop that continues until condition is true
      * This is like "while NOT (condition)" in other languages
      * Loop continues while: EOF not reached AND counter <= 4

               READ PAYROLL-INPUT-FILE INTO WS-TXT-LINE
      * READ = Get the next record (line) from the file
      * INTO WS-TXT-LINE = Store it in this variable
      * Each READ advances to the next line automatically

                   AT END
      * AT END = What to do when there's no more data
                       MOVE 1 TO WS-EOF-FLAG
      * Set flag to 1 to signal "end of file reached"
      * The loop will stop on the next iteration

                   NOT AT END
      * NOT AT END = What to do when we successfully read a line
                       PERFORM PARSE-TXT-LINE
      * Process the line we just read
                       ADD 1 TO WS-COUNTER
      * ADD 1 TO = Increment (like counter++ in other languages)
      * Move to the next employee type slot
               END-READ
      * END-READ = Closes the READ statement block
           END-PERFORM
      * END-PERFORM = Closes the PERFORM UNTIL loop

           CLOSE PAYROLL-INPUT-FILE.
      * CLOSE = We're done with this file
      * Always close files when finished - this:
      *   1. Releases system resources
      *   2. Ensures all data is properly saved
      *   3. Allows other programs to access the file

       PARSE-TXT-LINE.
      *----------------------------------------------------------------
      * PARSE-TXT-LINE - Split a CSV line into separate fields
      * Input line format: "Faculty,99,450"
      * We need to extract: "Faculty", "99", "450" separately
      *----------------------------------------------------------------
           UNSTRING WS-TXT-LINE DELIMITED BY ","
      * UNSTRING = Split a string into parts
      * DELIMITED BY "," = Use comma as the separator
      * This is like String.split(",") in Java
               INTO WS-TXT-EMP-TYPE
      * First part goes here (e.g., "Faculty")
                    WS-TXT-EMP-COUNT
      * Second part goes here (e.g., "99")
                    WS-TXT-BASIC
      * Third part goes here (e.g., "450")
           END-UNSTRING

           MOVE FUNCTION TRIM(WS-TXT-EMP-TYPE)
               TO WS-EMP-TYPE(WS-COUNTER)
      * FUNCTION TRIM = Remove leading/trailing spaces
      * WS-EMP-TYPE(WS-COUNTER) = Store in array at current position
      * If WS-COUNTER is 1, this stores in WS-EMP-TYPE(1)

           MOVE FUNCTION NUMVAL(WS-TXT-EMP-COUNT)
               TO WS-NO-OF-EMPLOYEES(WS-COUNTER)
      * FUNCTION NUMVAL = Convert text string to a number
      * "99" (text) becomes 99 (number)
      * This is necessary because UNSTRING gives us text,
      * but we need numbers for calculations

           MOVE FUNCTION NUMVAL(WS-TXT-BASIC)
               TO WS-BASIC-PAY(WS-COUNTER).
      * Convert basic pay text to number and store it

       CALCULATE-PAYROLL.
      *----------------------------------------------------------------
      * CALCULATE-PAYROLL - Perform all payroll calculations
      * For each employee type, we calculate:
      *   1. Basic Pay Total = Basic Pay × Number of Employees
      *   2. Allowance = 10% of Basic Pay Total
      *   3. Deduction = 12% of Basic Pay Total
      *   4. Gross Pay = Basic Pay Total + Allowance
      *   5. Net Pay = Gross Pay - Deduction
      * We also accumulate grand totals as we go.
      *----------------------------------------------------------------
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 4
      * PERFORM VARYING = For loop
      *   FROM 1 = Start at 1
      *   BY 1 = Increment by 1 each iteration
      *   UNTIL WS-COUNTER > 4 = Stop when counter exceeds 4
      * This is like: for(counter=1; counter<=4; counter++)

               COMPUTE BASIC-PAY-TOTAL(WS-COUNTER) =
                   WS-BASIC-PAY(WS-COUNTER) *
                   WS-NO-OF-EMPLOYEES(WS-COUNTER)
      * COMPUTE = Arithmetic calculation
      * Formula: Total = Pay × Employees
      * Example: 450 × 99 = 44,550

               COMPUTE ALLOWANCE(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) * 0.10
      * 10% allowance
      * Example: 44,550 × 0.10 = 4,455

               COMPUTE DEDUCTION(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) * 0.12
      * 12% deduction
      * Example: 44,550 × 0.12 = 5,346

               COMPUTE GROSS-PAY(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) +
                   ALLOWANCE(WS-COUNTER)
      * Gross = Basic + Allowance
      * Example: 44,550 + 4,455 = 49,005

               COMPUTE NET-PAY(WS-COUNTER) =
                   GROSS-PAY(WS-COUNTER) - DEDUCTION(WS-COUNTER)
      * Net = Gross - Deduction
      * Example: 49,005 - 5,346 = 43,659

      * Now add to grand totals (accumulate across all types)
               ADD WS-NO-OF-EMPLOYEES(WS-COUNTER) TO WS-AT-EMPLOYEES
      * Add this type's employee count to total
               ADD BASIC-PAY-TOTAL(WS-COUNTER) TO WS-AT-BASIC
               ADD ALLOWANCE(WS-COUNTER) TO WS-AT-ALLOWANCE
               ADD GROSS-PAY(WS-COUNTER) TO WS-AT-GROSS
               ADD DEDUCTION(WS-COUNTER) TO WS-AT-DEDUCTION
               ADD NET-PAY(WS-COUNTER) TO WS-AT-NET
           END-PERFORM.

           PERFORM WRITE-REPORT.
      * Now that calculations are done, generate the report

       WRITE-REPORT.
      *----------------------------------------------------------------
      * WRITE-REPORT - Generate the formatted payroll report
      * We write each line to the output file using WRITE FROM.
      * The report structure:
      *   1. Company header
      *   2. Report title
      *   3. Column headers
      *   4. Data rows (one per employee type)
      *   5. Totals row
      *----------------------------------------------------------------
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-HEADER1
      * WRITE = Output a record to the file
      * FROM WS-HEADER1 = Use the data from this variable
      * This writes the company name line

           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
      * Write a blank line for spacing

           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-HEADER2
      * Write the report title

           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
      * Two blank lines before the data

           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-COLUMN-HEADER
      * Write column headers row 1

           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-COLUMN-HEADER2
      * Write column headers row 2

           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
      * Blank line before data rows

           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 4
      * Loop through all 4 employee types to write their data

               MOVE WS-EMP-TYPE(WS-COUNTER) TO WS-DL-EMP-TYPE
      * Copy employee type name to the detail line template
      * The MOVE copies data from source to destination

               MOVE WS-NO-OF-EMPLOYEES(WS-COUNTER) TO WS-DL-NO-EMPS
      * Copy employee count (will be formatted by PIC ZZ9)

               MOVE BASIC-PAY-TOTAL(WS-COUNTER) TO WS-DL-BASIC
      * Copy basic pay (will be formatted with commas)

               MOVE ALLOWANCE(WS-COUNTER) TO WS-DL-ALLOWANCE
               MOVE GROSS-PAY(WS-COUNTER) TO WS-DL-GROSS
               MOVE DEDUCTION(WS-COUNTER) TO WS-DL-DEDUCTION
               MOVE NET-PAY(WS-COUNTER) TO WS-DL-NET
      * Copy all calculated values to the detail line

               WRITE PAYROLL-OUTPUT-RECORDS FROM WS-DETAIL-LINE
      * Write the completed detail line to the file

               WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
      * Blank line after each data row for readability
           END-PERFORM.

      * Now write the totals row
           MOVE WS-AT-EMPLOYEES TO WS-TL-NO-EMPS
           MOVE WS-AT-BASIC TO WS-TL-BASIC
           MOVE WS-AT-ALLOWANCE TO WS-TL-ALLOWANCE
           MOVE WS-AT-GROSS TO WS-TL-GROSS
           MOVE WS-AT-DEDUCTION TO WS-TL-DEDUCTION
           MOVE WS-AT-NET TO WS-TL-NET
      * Copy all grand totals to the totals line template

           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-TOTAL-LINE
      * Write the totals row

           CLOSE PAYROLL-OUTPUT-FILE.
      * Close the output file - IMPORTANT!
      * This ensures all data is written to disk

           DISPLAY "Report generated: PAYROLL-REPORT.TXT".
      * DISPLAY = Show message on the screen (console)
      * This confirms to the user that the program completed

       END PROGRAM EMPLOYEE-PAYROLL.
      * END PROGRAM = Marks the end of the program source code
      * The name must match the PROGRAM-ID
