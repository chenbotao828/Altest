;; error handling
; (c:ll)
(setq {ver_year} (atoi (substr (VER) 13 4)))

(defun time_ms () 
  (if (>= {ver_year} 2017) 
    (getvar "MILLISECS")
    (* (rem (getvar "DATE") 1) 86400000)
  )
)

(defun instance_error (msg) 
  (list (cons 'parent 'error) 
        (cons 'msg msg)
  )
)
(defun error? (x) 
  (if 
    (and 
      (al? x)
      (= (cdr (assoc 'parent x)) 'error)
    )
    t
    nil
  )
)

(defun try (x / catchit) 
  ;; execute expression get result or error
  (setq {altest_error_occured} nil)
  (setq catchit (vl-catch-all-apply 'eval (list x)))
  (if (vl-catch-all-error-p catchit) 
    (progn 
      (setq catchit (instance_error (vl-catch-all-error-message catchit)))
      (setq {altest_error_occured} catchit)
    )
  )
  (if {altest_error_occured} 
    {altest_error_occured}
    catchit
  )
)

(defun *error* (msg) 
  (setq {altest_error_occured} (instance_error msg)
        msg                    (str msg)
  )
  (if (not {altest_start_at}) 
    (progn 
      (princ (strcat "\n;; " msg "\n"))
      (vl-exit-with-error msg)
      nil
    )
  )
)

(defun check_off () 
  (setq {check_off} t)
  (princ "\n;; Type check off")
  (princ)
)
(defun check_on () 
  (setq {check_off} nil)
  (princ "\n;; Type check on")
  (princ)
)

;; AlTest
(defun altest (file / f start ed time output passed failed old_check) 
  (setq old_check   {check_off}
        {check_off} nil
  )
  ;; (defun start (x)
  ;;   (princ (strcat
  ;;           "\n--------- AlTest Start: "
  ;;           x
  ;;           " ---------\n")))
  (defun start (x / l -l -- s) 
    (setq l  (strlen x)
          -l (/ (- 80 15 l) 2)
          -- ""
    )
    (repeat (- -l 1) (setq -- (strcat -- "-")))
    (setq s (strcat "\n" -- " AlTest Start: " x " " --))
    (repeat (- 81 (strlen s)) (setq s (strcat s "-")))
    (setq s (strcat s "\n"))
    (princ s)
  )

  (defun ed () 
    (princ 
      (strcat 
        "\n-------------------------------------- End"
        " -------------------------------------\n"
      )
    )
  )

  (defun time () 
    (princ 
      (strcat 
        "Time "
        (rtos 
          (- (time_ms) {altest_start_at})
          2
          2
        )
        " ms"
      )
    )
  )

  (defun passed () 
    (princ 
      (strcat 
        "Passed "
        (rtos {altest_passed})
        ", "
      )
    )
  )

  (defun failed () 
    (princ 
      (strcat 
        "Failed "
        (rtos {altest_failed})
        ", "
      )
    )
  )

  (defun output () 

    (foreach unit (reverse {altest_results}) 
      (foreach ret (reverse (cdr unit)) 
        (if (= ret nil) 
          (princ ".")
          (princ "!")
        )
      )
    )



    (foreach unit (reverse {altest_results}) 
      (if (apply 'or (cdr unit)) 
        (progn 
          (princ (strcat "\n" (car unit) ": "))
          (foreach ret (reverse (cdr unit)) 
            (if (/= ret nil) (princ (strcat "\n  " ret)))
          )
        )
      )
    )
  )

  (setq f (strcat file "/test"))

  (if (not (file? f)) 
    (setq f file)
  )

  (if (file? f) 
    (progn 
      ;; start
      (setq {altest_start_at} (time_ms))
      (setq {altest_results} nil)
      (setq {altest_passed} 0)
      (setq {altest_failed} 0)
      (setq {altest_testing} "Test")
      (start f)
      ;; test
      (load f)
      ;; ed
      (output)
      (princ "\n \n")
      (passed)
      (Failed)
      (time)
      (ed)
      (setq {check_off} old_check)
      (setq {altest_start_at} nil)
      (setq {altest_results} nil)
      (setq {altest_passed} nil)
      (setq {altest_failed} nil)
      (setq {altest_testing} nil)
    )

    (*error* (strcat "FileNotExist: " f))
  )

  (princ)
)

(defun deftest (testname) 
  (setq {altest_testing} testname)
  (princ)
)


(defun altest_add_result (ret) 
  (if (= {altest_failed} nil) (setq {altest_failed} 0))
  (if (= {altest_passed} nil) (setq {altest_passed} 0))
  (cond 
    ((== nil {altest_results})
     (set '{altest_results} (list (cons {altest_testing} (list ret))))
    )

    ((== {altest_testing} (caar {altest_results}))
     (set '{altest_results} 
          (cons (cons {altest_testing} (cons ret (cdar {altest_results}))) 
                (cdr {altest_results})
          )
     )
    )
    (t
     (set '{altest_results} 
          (cons (list {altest_testing} ret) {altest_results})
     )
    )
  )
  (if ret 
    (setq {altest_failed} (+ 1 {altest_failed}))
    (setq {altest_passed} (+ 1 {altest_passed}))
  )
  (princ)
)

(defun assert (statement / catchit ret) 
  (setq catchit (try statement))
  (setq ret nil)
  (if {altest_error_occured} 
    (setq ret (strcat "Error \"" 
                      (cdr (assoc 'msg catchit))
                      "\": (assert '"
                      (str statement)
                      " )"
              )
    )
    (if (== catchit nil) 
      (setq ret (strcat "AssertionError: (assert '" (str statement) ")"))
      nil
    )
  )
  (if {altest_testing} 
    (altest_add_result ret)
    (princ ret)
  )
  (princ)
)


(defun assert-eq (sa sb / ca cb ret) 
  (setq ret nil)
  (setq ca (try sa))
  (setq cb (try sb))
  (cond 
    ((error? ca) ; Error from A
     (setq ret (strcat "Error \"" 
                       (cdr (assoc 'msg ca))
                       "\": '"
                       (str sa)
                       " <-- "
                       "(assert-eq '"
                       (str sa)
                       " '"
                       (str sb)
                       ")"
               )
     )
    )
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
                       ")"
               )
     )
    )
    ((not (== ca cb)) ; Assert-eq Error
     (setq ret (strcat "EqualAssertionError: " 
                       "(assert-eq '"
                       (str sa)
                       " '"
                       (str sb)
                       ")"
               )
     )
    )
    (t nil)
  )
  (if {altest_testing} 
    (altest_add_result ret)
    (princ ret)
  )
  (princ)
)

(defun assert-error (statement / ret) 
  (if (getvar "VERNUM") 
    nil
    ;; (altest_add_result (strcat "assert-error not working in GstarCAD: (assert-error '"
    ;;                         (str statement) ")"))
    (progn 
      (try statement)
      (if (nil? {altest_error_occured}) 
        (setq ret (strcat "No Error: (assert-error '" 
                          (str statement)
                          ")"
                  )
        )
      )
      (if {altest_testing} 
        (altest_add_result ret)
        (princ ret)
      )
      (princ)
    )
  )
)

;; %timeit 123**1231 
;; 19.1 s +- 63.9 ns per loop (mean +- std. dev. of 7 runs, 100000 loops each) 
;; total timeit should below 7 seconds
;; 1st round eval expr for 1, 10, 100 ... times until time is above 10 ms

(defun timeit (expr / start ed mms dt loop dtl ft n sort_list) 
  (if (<= {ver_year} 2020)
   (setq mms (strcat (vl-list->string '(166 204)) "s")
        +-  (vl-list->string '(161 192))
  )
  (setq mms "¦Ìs"
        +-  "¡À"
  ))
  (defun ft (x) 
    (cond 
      ((>= x 1000) (strcat (rtos (/ x 1000.0) 2 2) " s"))
      ((<= 1 x 1000) (strcat (rtos x 2 2) " ms"))
      ((<= 0.001 x 1) (strcat (rtos (* 1000 x) 2 2) " " mms))
      ((<= x 0.001) (strcat (rtos (* 1000000 x) 2 2) " ns"))
    )
  )
  (defun sort_list (lst func) 
    (mapcar '(lambda (x) (nth x lst)) (vl-sort-i lst 'func))
  )
  (setq start (time_ms))
  (eval expr)
  (setq ed (time_ms))
  (setq dt   (- ed start)
        loop 10
  )
  (while (<= dt 10) 
    (setq start (time_ms))
    (repeat loop (eval expr))
    (setq ed (time_ms))
    (setq dt   (- ed start)
          loop (* loop 10)
    )
  )
  (repeat 9 
    (progn 
      (setq start (time_ms))
      (repeat loop (eval expr))
      (setq ed (time_ms))
      (setq dt  (- ed start)
            dtl (cons dt dtl)
      )
    )
  )
  ;; (setq dtl (vl-remove (apply 'max dtl) dtl))
  ;; (setq dtl (vl-remove (apply 'min dtl) dtl))
  (setq dtl (cdr (reverse (cdr (sort_list dtl <))))
        n   (length dtl)
        t1  (/ (eval (apply '+ dtl)) (float n) loop)
        t2  (max (- (/ (apply 'max dtl) (float loop)) t1) 
                 (- t1 (/ (apply 'min dtl) (float loop)))
            )
  )
  (princ 
    (strcat "\n;; " 
            (ft t1)
            " "
            +-
            " "
            (ft t2)
            " per loop, (mean of "
            (rtos n)
            " runs, "
            (rtos loop)
            " loops each)\n"
    )
  )
  (princ)
)


;; in expr
;; 