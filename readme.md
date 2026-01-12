# COBOL Case Study

## Prerequisites
- [GnuCOBOL compiler](https://superbol.eu/developers/windows/) installed

Note: Make sure you are in the right directory when running the programs. If not, run `cd cobol` first.

---

## Program Files Overview

| File | Description |
|------|-------------|
| `prog1.cob` | Employee Payroll System |
| `prog1-commented.cob` | Employee Payroll System - heavily commented version for learning |
| `prog2.cob` | Student Grading System  |
| `prog2-commented.cob` | Student Grading System - heavily commented version for learning |

---

## Program 1: Employee Payroll System

Calculates payroll for different employee types (Faculty, Administrative, Utility, Security) and generates a formatted report.

### Payroll Formulas
- Basic Pay Total = Basic Pay × Number of Employees
- Allowance = 10% of Basic Pay Total
- Deduction = 12% of Basic Pay Total
- Gross Pay = Basic Pay Total + Allowance
- Net Pay = Gross Pay - Deduction

- **Input:** `PAYROLL-INPUT.txt` (CSV format: EmployeeType,Count,BasicPay)
- **Output:** `PAYROLL-REPORT.TXT`

### Compile and Run
```
cobc -x -o prog1 prog1.cob
.\prog1.exe
```

---

## Program 2: Student Grading System

Processes student grades and generates a report showing pass/fail statistics per year level (Freshmen, Sophomore, Junior, Senior).

### Grading Rules
- Average = (Prelim + Midterm + Final) / 3
- Pass if Average >= 75
- Fail if Average < 75

- **Input:** `STUDENT-INPUT.TXT` (CSV format: YearLevel,StudentNum,Prelim,Midterm,Final)
- **Output:** `STUDENT-REPORT.TXT`

### Compile and Run
```
cobc -x -o prog2 prog2.cob
.\prog2.exe
```

---


