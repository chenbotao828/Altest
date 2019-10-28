(defun altest (file / start end time output passed failed)

  (defun start (x)
    (princ (strcat
            "\n--------- AlTest Start: "
            x
            " ---------\n"
            )))

  (defun end ()
    (princ (strcat
            "\n--------------------- End"
            " ---------------------\n"
            )))

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
            (princ "!"))
       )
     )

    (foreach unit (reverse {altest_results})
      (if (apply 'or (cdr unit))
        (progn
          (princ (strcat "\n" (car unit) ": "))
          (foreach ret (reverse (cdr unit))
            (if (/= ret nil) (princ (strcat "\n  " ret)))
           )))))


  ;; start
  (setq {altest_results} nil)
  (setq {altest_passed} 0)
  (setq {altest_failed} 0)
  (setq {altest_testing} "Test")
  (start file)
  (setq {altest_start_at} (getvar "date"))
  ;; test
  (load file)
  ;; end
  (output)
  (princ "\n \n")
  (passed)
  (Failed)
  (time)
  (end)
  (princ))

(defun == (x y)
  (cond 
        ((= x y) T)
        ((eq x y) T)
        ((equal x y) T)
        (t nil))
  )

(setq s2str vl-princ-to-string)

(defun deftest (testname)
  (setq {altest_testing} testname)
  (princ)
 )

(defun add_result (ret)
  (cond 
   ((== nil {altest_results})
    (set '{altest_results} (list (cons {altest_testing} (list ret))))
    )
   ((== {altest_testing} (caar {altest_results}))
    (set '{altest_results} 
     (cons (cons {altest_testing} (cons ret (cdar {altest_results}))) (cdr {altest_results}))
     )
    )
   (t
    (set '{altest_results} (cons (list {altest_testing} ret) {altest_results}))
    )
  )
  (if ret (setq {altest_failed} (+ 1 {altest_failed}))
          (setq {altest_passed} (+ 1 {altest_passed}))
   )
  (princ)
)

(defun assert (statement / catchit ret)
  (setq catchit (vl-catch-all-apply 'eval (list statement)))
  (setq ret nil)
  (if (vl-catch-all-error-p catchit)
      (setq ret (strcat "Error \""
                     (vl-catch-all-error-message catchit)
                     "\": (assert '"
                     (s2str statement) 
                     " )"))
      (if (== catchit nil)
          (setq ret (strcat "AssertionError: (assert '" (s2str statement) ")"))
          nil))
  (add_result ret)
  (princ))

(defun assert-eq (sa sb / statement catchit ca cb es ea eb ret)
  (setq statement 
           (read (strcat "'(== "
                         (s2str sa)
                         " "
                         (s2str sb)
                         ")")))
  (setq ret nil)
  ;; (setq catchit (vl-catch-all-apply 'eval (list statement)))
  (setq ca (vl-catch-all-apply 'eval (list sa)))
  (setq cb (vl-catch-all-apply 'eval (list sb)))
  ;; (setq es (vl-catch-all-error-p catchit))
  (setq ea (vl-catch-all-error-p ca))
  (setq eb (vl-catch-all-error-p cb))
  (cond (ea ; Error from A 
          (setq ret (strcat "Error \""
                         (vl-catch-all-error-message ca)
                         "\": '"
                         (s2str sa)
                         " <-- "
                         "(assert-eq '"
                         (s2str sa) 
                         " '"
                         (s2str sb)
                         ")")))
        (eb ; Error from B
          (setq ret (strcat "Error \""
                         (vl-catch-all-error-message cb)
                         "\": '"
                         (s2str sb)
                         " <-- "
                         "(assert-eq '"
                         (s2str sa) 
                         " '"
                         (s2str sb)
                         ")")))
        ((not (== ca cb)) ; Assert-eq Error
         (setq ret (strcat "EqualAssertionError: "
                        "(assert-eq '"
                        (s2str sa) 
                        " '"
                        (s2str sb)
                        ")")))
        (t nil))
  (add_result ret)
  (princ))

(defun assert-error (statement / ret)
  (setq ret nil)
  (setq catchit (vl-catch-all-apply 'eval (list statement)))
  (if (not (vl-catch-all-error-p catchit))
      (setq ret (strcat "NoError: (assert-error '"
                        (s2str statement) ")")))
  (add_result ret)
  (princ))
