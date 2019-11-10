# Altest

Unit test for autolisp



# Install

## Using [Ezlisp](https://github.com/chenbotao828/Ezlisp)

`(install chenbotao828/Altest)`

# Usage

## In "altest/test.lsp" 

```commonlisp
(assert 't)
(assert 'nil)
(deftest "Assert")
(assert '(= 1 1))
(assert '(/= 1 1))
(assert '(/ 1 0))
(deftest "Assert-eq")
(assert-eq '1 '(+ 1 0))
(assert-eq '1 '(+ 1 1))
(assert-eq '1 '(/ 1 0))
(deftest "Assert-error")
(assert-error '(/ 1 0))
(assert-error '(/ 1 1))
(deftest "All-pass")
(assert '(= 1 1))
(assert-eq '1 '(+ 1 0))
(assert-error '(/ 1 0))
```

## In CAD

`(altest "altest/test")` or `(altest "altest")` 

"test" (end with ".lsp") is default test file in package

```commonlisp
--------- AlTest Start: altest/test ---------
.!.!!.!!.!...
Test:
  AssertionError: (assert 'nil)
Assert:
  AssertionError: (assert '(/= 1 1))
  Error "divide by zero": (assert '(/ 1 0) )
Assert-eq:
  EqualAssertionError: (assert-eq '1 '(+ 1 1))
  Error "divide by zero": '(/ 1 0) <-- (assert-eq '1 '(/ 1 0))
Assert-error:
  NoError: (assert-error '(/ 1 1))
Passed 7, Failed 6, Time 15.01 ms
--------------------- End ---------------------
```
