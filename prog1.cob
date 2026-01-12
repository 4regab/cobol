       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-PAYROLL.
       AUTHOR. GROUP2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT PAYROLL-INPUT-FILE ASSIGN TO "PAYROLL-INPUT.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.
       SELECT PAYROLL-OUTPUT-FILE ASSIGN TO "PAYROLL-REPORT.TXT"
            ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-INPUT-FILE.
       01  PAYROLL-INPUT-RECORD        PIC X(100).

       FD  PAYROLL-OUTPUT-FILE.
       01  PAYROLL-OUTPUT-RECORDS PIC X(120).

       WORKING-STORAGE SECTION.
       01  WS-EMPLOYEE-TYPES.
               05 WS-EMP-TYPE            PIC X(15) OCCURS 4 TIMES.

       01  WS-EMPLOYEE-DATA.
               05  WS-EMP-ENTRY OCCURS 4 TIMES.
                   10 WS-NO-OF-EMPLOYEES PIC 999.
                   10 WS-BASIC-PAY       PIC 9(7)V99.

       01  WS-CALCULATED-VALUES.
               05  WS-CALC-ENTRY OCCURS 4 TIMES.
                   10 ALLOWANCE          PIC 9(9)V99.
                   10 DEDUCTION          PIC 9(9)V99.
                   10 GROSS-PAY          PIC 9(9)V99.
                   10 NET-PAY            PIC 9(9)V99.
                   10 BASIC-PAY-TOTAL    PIC 9(9)V99.

       01  WS-ALL-TOTAL.
               05 WS-AT-EMPLOYEES        PIC 9999 VALUE ZEROS.
               05 WS-AT-BASIC            PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-ALLOWANCE        PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-GROSS            PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-DEDUCTION        PIC 9(10)V99 VALUE ZEROS.
               05 WS-AT-NET              PIC 9(10)V99 VALUE ZEROS.

       01  WS-COUNTERS.
               05 WS-COUNTER             PIC 9 VALUE 1.

       01  WS-EOF-FLAG                   PIC 9 VALUE 0.

       01  WS-TXT-PARSE.
               05 WS-TXT-LINE            PIC X(100).
               05 WS-TXT-EMP-TYPE        PIC X(15).
               05 WS-TXT-EMP-COUNT       PIC X(10).
               05 WS-TXT-BASIC           PIC X(15).

       01  WS-OUTPUT-HDG.
               05 WS-HEADER1.
                   10 FILLER              PIC X(35) VALUE SPACES.
                   10 FILLER              PIC X(30)
                                      VALUE "ABCDEF TECHNOLOGY COMPANY".
               05 WS-HEADER2.
                   10 FILLER              PIC X(40) VALUE SPACES.
                   10 FILLER              PIC X(20) VALUE
                                            "EMPLOYEE PAYROLL".
               05 WS-BLANK-LINE           PIC X(100) VALUE SPACES.
               05 WS-COLUMN-HEADER.
                   10 FILLER           PIC X(16) VALUE "EMPLOYEE TYPE".
                   10 FILLER           PIC X(9) VALUE "NO. OF".
                   10 FILLER           PIC X(14) VALUE "BASIC PAY".
                   10 FILLER           PIC X(14) VALUE "ALLOWANCE".
                   10 FILLER           PIC X(14) VALUE "GROSS PAY".
                   10 FILLER           PIC X(14) VALUE "DEDUCTION".
                   10 FILLER           PIC X(14) VALUE "NET PAY".
               05 WS-COLUMN-HEADER2.
                   10 FILLER           PIC X(16) VALUE SPACES.
                   10 FILLER           PIC X(9) VALUE "EMPLOYEES".
                   10 FILLER           PIC X(84) VALUE SPACES.
               05 WS-DETAIL-LINE.
                   10 WS-DL-EMP-TYPE      PIC X(16).
                   10 WS-DL-NO-EMPS       PIC ZZ9.
                   10 FILLER              PIC X(6)  VALUE SPACES.
                   10 WS-DL-BASIC         PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-ALLOWANCE     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-GROSS         PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-DEDUCTION     PIC ZZZ,ZZ9.99.
                   10 FILLER              PIC X(4)  VALUE SPACES.
                   10 WS-DL-NET           PIC ZZZ,ZZ9.99.
               05 WS-TOTAL-LINE.
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
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-EMP-TYPE.
           PERFORM READ-INPUT.
           PERFORM CALCULATE-PAYROLL.

           STOP RUN.

       INITIALIZE-EMP-TYPE.
           OPEN INPUT PAYROLL-INPUT-FILE
           OPEN OUTPUT PAYROLL-OUTPUT-FILE.

       READ-INPUT.
           MOVE 1 TO WS-COUNTER
           PERFORM UNTIL WS-EOF-FLAG = 1 OR WS-COUNTER > 4
               READ PAYROLL-INPUT-FILE INTO WS-TXT-LINE
                   AT END
                       MOVE 1 TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM PARSE-TXT-LINE
                       ADD 1 TO WS-COUNTER
               END-READ
           END-PERFORM
           CLOSE PAYROLL-INPUT-FILE.

       PARSE-TXT-LINE.
           UNSTRING WS-TXT-LINE DELIMITED BY ","
               INTO WS-TXT-EMP-TYPE
                    WS-TXT-EMP-COUNT
                    WS-TXT-BASIC
           END-UNSTRING

           MOVE FUNCTION TRIM(WS-TXT-EMP-TYPE)
               TO WS-EMP-TYPE(WS-COUNTER)
           MOVE FUNCTION NUMVAL(WS-TXT-EMP-COUNT)
               TO WS-NO-OF-EMPLOYEES(WS-COUNTER)
           MOVE FUNCTION NUMVAL(WS-TXT-BASIC)
               TO WS-BASIC-PAY(WS-COUNTER).

       CALCULATE-PAYROLL.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 4
               COMPUTE BASIC-PAY-TOTAL(WS-COUNTER) =
                   WS-BASIC-PAY(WS-COUNTER) *
                   WS-NO-OF-EMPLOYEES(WS-COUNTER)
               COMPUTE ALLOWANCE(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) * 0.10
               COMPUTE DEDUCTION(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) * 0.12
               COMPUTE GROSS-PAY(WS-COUNTER) =
                   BASIC-PAY-TOTAL(WS-COUNTER) +
                   ALLOWANCE(WS-COUNTER)
               COMPUTE NET-PAY(WS-COUNTER) =
                   GROSS-PAY(WS-COUNTER) - DEDUCTION(WS-COUNTER)

               ADD WS-NO-OF-EMPLOYEES(WS-COUNTER) TO WS-AT-EMPLOYEES
               ADD BASIC-PAY-TOTAL(WS-COUNTER) TO WS-AT-BASIC
               ADD ALLOWANCE(WS-COUNTER) TO WS-AT-ALLOWANCE
               ADD GROSS-PAY(WS-COUNTER) TO WS-AT-GROSS
               ADD DEDUCTION(WS-COUNTER) TO WS-AT-DEDUCTION
               ADD NET-PAY(WS-COUNTER) TO WS-AT-NET
           END-PERFORM.

           PERFORM WRITE-REPORT.

       WRITE-REPORT.
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-HEADER1
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-HEADER2
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-COLUMN-HEADER
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-COLUMN-HEADER2
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-BLANK-LINE

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

           MOVE WS-AT-EMPLOYEES TO WS-TL-NO-EMPS
           MOVE WS-AT-BASIC TO WS-TL-BASIC
           MOVE WS-AT-ALLOWANCE TO WS-TL-ALLOWANCE
           MOVE WS-AT-GROSS TO WS-TL-GROSS
           MOVE WS-AT-DEDUCTION TO WS-TL-DEDUCTION
           MOVE WS-AT-NET TO WS-TL-NET
           WRITE PAYROLL-OUTPUT-RECORDS FROM WS-TOTAL-LINE

           CLOSE PAYROLL-OUTPUT-FILE.

           DISPLAY "Report generated: PAYROLL-REPORT.TXT".

       END PROGRAM EMPLOYEE-PAYROLL.
