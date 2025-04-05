# Utilities

This repository contains a collection of utility functions and packages written in Common Lisp. These utilities are designed to simplify and streamline various programming tasks by providing reusable and efficient implementations of common operations.

## Features

- **Mathematical Functions**: Includes functions for calculating averages, factorials, binomial coefficients, harmonic numbers, and more.
- **List Operations**: Provides utilities for comparing list lengths and other list-related operations.
- **Symbol Manipulation**: Includes functions like `symbol-paste` for dynamically creating symbols.
- **Series Integration**: Leverages the `SERIES` package for functional-style operations like mapping, reducing, and scanning.
- **Custom Comparators**: Implements custom comparison functions for lists and numbers.
- **Trapezoidal Integration**: A simple implementation of numerical integration using the trapezoidal rule.

## Installation

Clone the repository to your local machine:

```bash
git clone https://github.com/your-username/utilities.git
```

Load the system using ASDF:

```lisp
(asdf:load-system :utilities)
```

## Usage

Define your project package and use the utilities:

```lisp
(defpackage "MY-PROJECT"
  (:use "COMMON-LISP" "UTILITIES"))

(in-package "MY-PROJECT")

(format t "The average of 4 and 8 is ~a~%" (average 4 8))
(format t "The factorial of 5 is ~a~%" (utilities:factorial 5))
```

## Function Reference with Examples

Here is a detailed reference for some of the key functions provided by the `UTILITIES` package, along with usage examples.

### 1. `average`
Calculates the average of two numbers.

**Usage:**
```lisp
(format t "The average of 4 and 8 is ~a~%" (utilities:average 4 8))
```

**Output:**
```
The average of 4 and 8 is 6
```

### 2. `factorial`
Calculates the factorial of a number.

**Usage:**
```lisp
(format t "The factorial of 5 is ~a~%" (utilities:factorial 5))
```

**Output:**
```
The factorial of 5 is 120
```

### 3. `binomial`
Calculates the binomial coefficient (n choose k).

**Usage:**
```lisp
(format t "The binomial coefficient of 5 choose 2 is ~a~%" (utilities:binomial 5 2))
```

**Output:**
```
The binomial coefficient of 5 choose 2 is 10
```

### 4. `harmonic`
Calculates the nth harmonic number.

**Usage:**
```lisp
(format t "The 5th harmonic number is ~a~%" (utilities:harmonic 5))
```

**Output:**
```
The 5th harmonic number is 2.283333333333333
```

### 5. `symbol-paste`
Dynamically creates a symbol by concatenating strings.

**Usage:**
```lisp
(format t "The symbol created is ~a~%" (utilities:symbol-paste '("hello" "world") "COMMON-LISP-USER"))
```

**Output:**
```
The symbol created is |helloworld|
```

### 6. `trapezoid`
Performs numerical integration using the trapezoidal rule.

**Usage:**
```lisp
(let ((integrate (utilities::trapezoid #'sin 0 pi)))
  (format t "The integral of sin(x) from 0 to pi is ~a~%" (funcall integrate 100)))
```

**Output:**
```
The integral of sin(x) from 0 to pi is 1.9998355038874436d0
```

### 7. `list-length<?`, `list-length<=?`, `list-length=?`, `list-length<>?`, `list-length>=?`, `list-length>?`
Compare the lengths of two lists.

**Usage:**
```lisp
(format t "Is the length of list1 less than list2? ~a~%" (utilities:list-length<? '(1 2) '(1 2 3)))
```

**Output:**
```
Is the length of list1 less than list2? T
```

### 8. `length<?`, `length<=?`, `length=?`, `length<>?`, `length>=?`, `length>?`
Compare the length of a list to another list or a number.

**Usage:**
```lisp
(format t "Is the length of list1 less than 5? ~a~%" (utilities:length<? '(1 2 3) 5))
```

**Output:**
```
Is the length of list1 less than 5? T
```

### 9. `big-pi` and `big-sigma`
Perform product and summation operations over a range.

**Usage:**
```lisp
(format t "The product of numbers from 1 to 4 is ~a~%" (utilities:big-pi #'identity 1 5))
(format t "The sum of numbers from 1 to 4 is ~a~%" (utilities:big-sigma #'identity 1 5))
```

**Output:**
```
The product of numbers from 1 to 4 is 24
The sum of numbers from 1 to 4 is 10
```

### 10. `cross-ratio`
Calculates the cross-ratio of four numbers.

**Usage:**
```lisp
(format t "The cross-ratio of 1, 2, 3, 4 is ~a~%" (utilities:cross-ratio 1 2 3 4))
```

**Output:**
```
The cross-ratio of 1, 2, 3, 4 is 4/3
```

### 11. `square` and `cube`
Calculates the square or cube of a number.

**Usage:**
```lisp
(format t "The square of 3 is ~a~%" (utilities:square 3))
(format t "The cube of 3 is ~a~%" (utilities:cube 3))
```

**Output:**
```
The square of 3 is 9
The cube of 3 is 27
```

### 12. `rising-factorial` and `falling-factorial`
Calculates the rising or falling factorial of a number.

**Usage:**
```lisp
(format t "The rising factorial of 3 with rise 2 is ~a~%" (utilities:rising-factorial 3 2))
(format t "The falling factorial of 5 with fall 3 is ~a~%" (utilities:falling-factorial 5 3))
```

**Output:**
```
The rising factorial of 3 with rise 2 is 12
The falling factorial of 5 with fall 3 is 60
```

### 13. `least-squares`
Calculates the slope and intercept of the least-squares regression line for a set of points.

**Usage:**
```lisp
(multiple-value-bind (m b) (utilities:least-squares '((1 . 2) (2 . 3) (3 . 5)))
  (format t "The slope is ~a and the intercept is ~a~%" m b))
```

**Output:**
```
The slope is 3/2 and the intercept is 1/3
```

### 14. `iota`
Generates a list of integers from 0 through n-1.

**Usage:**
```lisp
(format t "The list generated is ~a~%" (utilities:iota 5))
```

**Output:**
```
The list generated is (0 1 2 3 4)
```

### 15. `integer-log` and `leftmost-digit`
Calculates the integer logarithm and leftmost digit of a number in a given base.

**Usage:**
```lisp
(multiple-value-bind (log leftmost) (utilities:integer-log 100 10)
  (format t "The integer log is ~a and the leftmost digit is ~a~%" log leftmost))
```

**Output:**
```
The integer log is 2 and the leftmost digit is 1
```

