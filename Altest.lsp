;; error handling
(defun instance_error (msg)
  (list (cons 'class 'error)
    (cons 'parent 'base_error)
    (cons 'msg msg)))

(defun error? (x / al? list? func? all)

  (defun list? (x)
   (if (= 'list (type x))
     t nil))

  (defun func? (x)
   (if (= 'SUBR (type x))
     t nil))

  (defun all (l f / ret i n)
    (setq ret T n 0)
    (if (not (func? f)) (setq f (eval f)))
    (while (and ret (< n (length l)))
      (setq i (nth n l))
      (setq ret (if (apply 'f (list i)) t nil))
      (setq n (+ 1 n)))
    ret)

  (defun al? (l)
    (if (and 
      (list? l)
      (all l (lambda (x) (and 
        (list? x)
        (atom (car x))))))
    t nil))
  (if (and
    (al? x)
    (= (cdr (assoc 'class x)) 'error))
  
  t nil))

(defun try (x / catchit)
  ;; execute expression get result or error
  (setq {altest_error_occured} nil)
  (setq catchit (vl-catch-all-apply 'eval (list x)))
  (if (vl-catch-all-error-p catchit)
    (progn
      (setq catchit 
       (instance_error (vl-catch-all-error-message catchit)))
      (setq {altest_error_occured} catchit)))
  (if {altest_error_occured}
    {altest_error_occured}
    catchit))

(defun *error* (msg)
  (setq {altest_error_occured} (instance_error msg))
  (if (not {altest_start_at})
      (progn
        (princ (strcat "\n;; " msg "\n"))
        (vl-exit-with-error msg)
        )
      )
  )

;; AlTest
(defun altest (file / f start end time output passed failed file?)

  ;; (defun start (x)
  ;;   (princ (strcat
  ;;           "\n--------- AlTest Start: "
  ;;           x
  ;;           " ---------\n")))
  (defun start (x / l -l -- s)
    (setq  l (strlen x)
      -l (/ (- 80 15 l ) 2)
      -- "")
    (repeat (- -l 1) (setq -- (strcat -- "-")))
    (setq s (strcat "\n" -- " AlTest Start: " x " " --))
    (repeat (- 81 (strlen s)) (setq s (strcat s "-")))
    (setq s (strcat s "\n"))
    (princ s))
  
  (defun end ()
    (princ (strcat
      "\n-------------------------------------- End"
      " -------------------------------------\n")))

  (defun time ()
    (princ
      (strcat
        "Time "
        (rtos
          (* 86400000
            (- (getvar "date") {altest_start_at}))
          2 2)
        " ms")))

  (defun passed ()
    (princ
      (strcat
        "Passed "
        (rtos {altest_passed})
        ", ")))

  (defun failed ()
    (princ
      (strcat
        "Failed "
        (rtos {altest_failed})
        ", ")))

  (defun output ()

    (foreach unit (reverse {altest_results})
      (foreach ret (reverse (cdr unit))
        (if (= ret nil) (princ ".")
          (princ "!"))))
    
    

    (foreach unit (reverse {altest_results})
      (if (apply 'or (cdr unit))
        (progn
          (princ (strcat "\n" (car unit) ": "))
          (foreach ret (reverse (cdr unit))
            (if (/= ret nil) (princ (strcat "\n  " ret))))))))
  

  (defun file? (f)
    (or 
     (findfile f)
     (findfile (strcat f ".lsp"))
     (findfile (strcat f ".fas"))
     (findfile (strcat f ".vlx"))))
  
  (setq f (strcat file "/test"))
  
  (if (not (file? f))
    (setq f file))

  (if (file? f)
    (progn
      ;; start
      (setq {altest_start_at} (getvar "date"))
      (setq {altest_results} nil)
      (setq {altest_passed} 0)
      (setq {altest_failed} 0)
      (setq {altest_testing} "Test")
      (start f)
      ;; test
      (load f)
      ;; end
      (output)
      (princ "\n \n")
      (passed)
      (Failed)
      (time)
      (end)
      (setq {altest_start_at} nil)
      (setq {altest_results} nil)
      (setq {altest_passed} nil)
      (setq {altest_failed} nil)
      (setq {altest_testing} nil))
    
    (*error* (strcat "FileNotExist: " f)))
  
  (princ))


(defun == (x y)
  (cond 
    ((= x y) T)
    ((eq x y) T)
    ((if (and (numberp x) (numberp y))
     (equal x y 0.0000000000000001)
     (equal x y)) T)
    (t nil)))


(setq str vl-prin1-to-string)

(defun deftest (testname)
  (setq {altest_testing} testname)
  (princ))


(defun altest_add_result (ret)
  (if (= {altest_failed} nil) (setq {altest_failed} 0))
  (if (= {altest_passed} nil) (setq {altest_passed} 0))
  (cond 
   ((== nil {altest_results})
    (set '{altest_results} (list (cons {altest_testing} (list ret)))))
   
   ((== {altest_testing} (caar {altest_results}))
    (set '{altest_results} 
     (cons (cons {altest_testing} (cons ret (cdar {altest_results}))) (cdr {altest_results}))))
   (t
    (set '{altest_results} (cons (list {altest_testing} ret) {altest_results}))))
  (if ret (setq {altest_failed} (+ 1 {altest_failed}))
    (setq {altest_passed} (+ 1 {altest_passed})))
  (princ))

(defun assert (statement / catchit ret)
  (setq catchit (try statement))
  (setq ret nil)
  (if {altest_error_occured}
    (setq ret (strcat "Error \""
               (cdr (assoc 'msg catchit))
               "\": (assert '"
               (str statement) 
               " )"))
    (if (== catchit nil)
      (setq ret (strcat "AssertionError: (assert '" (str statement) ")"))
      nil))
  (if {altest_testing} (altest_add_result ret)
    (princ ret))
  (princ))


(defun assert-eq (sa sb / statement ca cb ret)
  (setq statement 
   (read (strcat "'(== "
          (str sa)
          " "
          (str sb)
          ")")))
  (setq ret nil)
  (setq ca (try sa))
  (setq cb (try sb))
  (cond ((error? ca) ; Error from A 
         (setq ret (strcat "Error \""
                    (cdr (assoc 'msg ca))
                    "\": '"
                    (str sa)
                    " <-- "
                    "(assert-eq '"
                    (str sa) 
                    " '"
                    (str sb)
                    ")")))
   ((error? cb) ; Error from B
    (setq ret (strcat "Error \""
               (cdr (assoc 'msg cb))
               "\": '"
               (str sb)
               " <-- "
               "(assert-eq '"
               (str sa) 
               " '"
               (str sb)
               ")")))
   ((not (== ca cb)) ; Assert-eq Error
    (setq ret (strcat "EqualAssertionError: "
               "(assert-eq '"
               (str sa) 
               " '"
               (str sb)
               ")")))
   (t nil))
  (if {altest_testing} (altest_add_result ret)
    (princ ret))
  (princ))

(defun assert-error (statement / ret)
  (try statement)
  (if (not {altest_error_occured})
    (setq ret (strcat "No Error: (assert-error '"
               (str statement) ")")))
  (if {altest_testing} (altest_add_result ret)
    (princ ret))
  (princ))
