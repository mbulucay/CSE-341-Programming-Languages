(load "gpp_lexer.lisp")

(setq variables '())
(setq opStack '())
(setq stack '())
(setq tmp '())
(setq results '())

(defun getTokenID (tkname operators)
    "Returns the token ID of the given token names"
    (when (not (equal (length operators) 0))
        (if (equal tkname (car (car operators)))
            (return-from  getTokenID (cdr (car operators)))
            (getTokenID tkname (cdr operators))
        )
    )
)

(defun isParanthesis(token)
    "Returns true if the given token is a paranthesis"
    (or (string= "OP_CP" token)
        (string= "OP_OP" token))
)

(defun isOperator (token)
    "returns true if token is an operator"
	(or (string= "OP_PLUS" token)
		(string= "OP_MINUS" token)
		(string= "OP_DIV" token)
		(string= "OP_MULT"token)
        (string= "OP_DBLMULT" token))
)

(defun isLogic (token)
    "returns true if token is a logic operator"
	(or (string= "KW_AND" token)
		(string= "KW_OR" token)
		(string= "KW_EQUAL" token)
		(string= "KW_NOT" token)
        (string= "KW_TRUE" token)
        (string= "KW_FALSE" token))
)

(defun removeLast(l)
    "Remove last element from list"
    (reverse (cdr (reverse l)))
)

(defun makeOp (token val1 val2)
    "making + - * / ** opeators in the stack last in two elements and save it in the stack"
    (let ((opToken token) (val_1 val1) (val_2 val2))
        
        (setq stack (removelast (removelast stack)))
        
        (cond  
                ( (string= "OP_MULT" optoken) 
                    (setq r (* (parse-integer (car val_1)) (parse-integer (car val_2)))) )
                ( (string= "OP_MINUS" optoken)
                    (setq r (- (parse-integer (car val_1)) (parse-integer (car val_2)))) )
                ( (string= "OP_PLUS" optoken) 
                    (setq r (+ (parse-integer (car val_1)) (parse-integer (car val_2)))) )
                ( (string= "OP_DIV" optoken) 
                    (setq r (/ (parse-integer (car val_1)) (parse-integer (car val_2)))) )
                ( (string= "OP_DBLMULT" optoken) 
                    (setq r (expt (parse-integer (car val_1)) (parse-integer (car val_2)))) )
        )
        (setq stack (append stack (list  (write-to-string r))))

    )
)

(defun isInFalse(lst)
    "returns true if the list contains a false value"
    (when (not (equal (length lst) 0))
        (if (equal (car lst) "false")
            (return-from isInFalse T)
            (isInFalse (cdr lst))
        )
    )
)

(defun isInTrue(lst)
    "returns true if the list contains true"
    (when (not (equal (length lst) 0))
        (if (equal (car lst) "true")
            (return-from  isInTrue T)
            (isInTrue (cdr lst))
        )
    )
)

(defun makeLogic (token)
    "Making logic operation which we defined in the grammar"
    (let ((logToken token))

        (cond  
                ( (string= "KW_AND" logToken) 
                    (setq r (not (isInFalse stack))) )
                ( (string= "KW_OR" logToken)
                    (setq r (isInTrue stack)) )
                ( (string= "KW_EQUAL" logToken) 
                    (setq r (equal (car (last stack)) (car (last (removelast stack))) )) )
                ( (string= "KW_NOT" logToken) 
                    (if (equal (car (last stack)) "true") 
                        (setq r nil)
                        (setq r T)
                    ))
        )
        (setq stack '())
        (if r
            (setq results (append results (list  "true")))
            (setq results (append results (list  "false")))
        )
    )
)

(defun getUntilOperand(stackList)
    "get the values from the stack until an operator is found"
    (let ((lst stackList)
            (bool (and (null (getTokenID (car (last stackList)) (getTokens))) (/= 0 (length stackList)))))

        (if bool 
            (let ()
                (setq tmp (append tmp (list (car (last lst))))) 
                (setq stack (removelast stack)) 
                (getUntilOperand (removelast lst)))
            (let () 
                (setq stack (list (append stack tmp))) 
                (setq tmp '()))
        )
    )
)

(defun isLess (val1 val2)
    "returns true if val1 is less than val2"
    (if (< val1 val2)
        (setq results (append results (list  "true")))
        (setq results (append results (list  "false")))
    )
    (setq stack '())
)    

(defun dispVar(identifier lst)
    "Controlling the variable is exist or not"   
    (when (not (equal (length lst) 0))
        (if (equal identifier (car (car lst)))
            (return-from dispVar (cdr (car lst)))
            (dispVar identifier (cdr lst))
        )
    )
)

(defun displayResult()
    "print the result of a variable"
    (if (/= (length stack) 0)
        (let () 
            (format t "Result: ~{~A~}~%"  (dispVar (car (last stack)) variables)))
        (let ()
            (format t "Result: ( ")
            (loop for x in(car (last results)) do (format t "~d "  (dispVar x variables)))
            (format t ")~%")
        )
    ) 
)

(defun reAssignVar(identifier variables)
    "reassign the value of a variable"
)

(defun getVar (identifier)
    "
    get the value of a variable
    "
    (let ((lst variables))
        (when (not (equal (length lst) 0))
            (if (equal identifier (car (car lst)))
                (return-from getVar (car lst)) 
                (getVar identifier (cdr lst))
            )
        )
    )
)

(defun setVar (stackList)
    "Setting a variable in the variables list"
    (let ((var1 (car (last stackList))))
        
        (if (equal (length stackList) 1)
            ; if the value is list
            (let ((var2 (list (list var1 (car (last results)))))) 
                (setq variables (append variables  var2))
                (setq results (append results var2))
                (setq stack (removelast stack))
                (setq results (removelast results))
                ; (setq results (removelast results))
                ; (format t "Result: ~{~A~}~%"  (last (getVar var1)))
            )
            ; if the value is value
            (let ((var2 (car (last (removelast stackList)))) )
                (setq variables (append variables (list (list var1 var2))))
                (setq stack  (removelast stack))
                ; (format t "Result: ~{~A~}~%"  (last (getVar var1)))
            )
        ) 
    )
)

(defun appendVar(stackList) 
    "
    Append a new value to getting list
    "
    (let ((lst (car (last results))) (value (car (last stackList))) )
        (setq results (append results (list (append lst (list value)))))
        (setq stack '())
    )
)

(defun concatenateVar (stackList)
    "
    Concatenate the two list with theirselves
    And the result is storing in the results list
    "
    (let ((var1 (car (last results)))
            (var2 (car (last (removelast results)))))
        
        (setq results (removelast results))
        (setq results (removelast results))
        (setq results (append results (list (append var1 var2))))
    )
)

(defun cleanWorld()
    "
    append the result in results 
    and cleaning stack variable
    "
    (setq results (append results stack))
    (setq stack '())
)

(defun addStackOperations(stackList)
    "
        Main function for the interpreter operations
        Using the stack to control the operations if the next element is value store in stack
        until the next element is an operator
        then the according to the operator the returned result is calculated
        and stored in the results list
        then calculated values removed from the stack

        We are using stack therefore we can handle the operations in the order they are written in the grammar
        it allow us to nested operations.

        Function explained in report with draws  
    "
    (when (and (>= (length (car stackList)) 2) 
			(string= (subseq (car stackList) 0 2) ";;"))(return-from addStackOperations nil) )
    
    (if (null stackList) 
        (cleanWorld)
        (let ((var (last stackList)) (tokenID (getTokenID (car (last stackList)) (getTokens))) )
            
            (connetWithToken (car (last stackList)) (getTokens))
            (when (not (or (equal ")" (car var)) (equal "(" (car var)) (equal "," (car var))))

                (if tokenID
                    
                    (cond 
                            ((isOperator tokenID) 
                                (let () (makeOp tokenID (last stack) (last (removelast stack)) ) ))
                            ((isLogic tokenID)
                                (let () 
                                    (if (or (string= tokenID "KW_FALSE") (string= tokenID "KW_TRUE"))
                                        (setq stack (append stack var))
                                        (makeLogic tokenID)
                                    )
                                ))
                            ((equal "KW_LIST" tokenID) 
                                (let () (getUntilOperand stack) (cleanWorld)))
                            ((equal "KW_SET" tokenID)
                                (let () (setVar stack) (cleanWorld)))
                            ((equal "KW_CONCAT" tokenID)
                                (let ()(concatenateVar stack) (cleanWorld)))
                            ((equal "KW_APPEND" tokenID)
                                 (appendVar stack))
                            ((equal "KW_DISP" tokenID)
                                (let () (displayResult) (cleanWorld)))
                            ((equal "KW_LESS" tokenID)
                                (isLess (parse-integer (car (last stack))) (parse-integer (car (last (removelast stack))))))
                    )
                    (setq stack (append stack var))         ;if next element is not operator add to stack
                )    
            )
            (addStackOperations (removeLast stackList))
        )
    )
)

(defun interpreterShell2()
    "   
    Start the interpreter shell
    basically read line split it and print the result
    "
	(format t "$> ") 
	(setf line (read-line))

	(if (string= (string line) "(exit)") 
		(bye)
		(addStackOperations (splitSeq line)))
    
    (when (not (and (>= (length line) 2) 
			(string= (subseq line 0 2) ";;")))
        (let()
            (format t "Syntax OK:~%" )
            (format t "Result: ~{~A~}~%" (last results))
        ))
	(interpreterShell2)
)

(defun parseLine (line)
    "
    parse the line in getting line from the file
    "
    (if (string= (string line) "(exit)") 
		(bye)
		(addStackOperations (splitSeq line)))
    (when (not (and (>= (length line) 2) 
			(string= (subseq line 0 2) ";;")))
        (let()
            (format t "Syntax OK:~%" )
            (format t "Result: ~{~A~}~%" (last results))
        ))
)

(defun interpreterFile2 (fname)
    "
    reding line from file then parse it line by line
    "
	(let 
		((ret (open fname :if-does-not-exist nil)))		
	  	(when ret (loop for line = (read-line ret nil)
	        while line do (parseLine line)) (close ret)))
)

(defun gppinterpreter2 (&optional fname)
	(if fname (interpreterFile2 fname) (interpreterShell2))
)

(defun main2()
	 (if (null *args*)
        (gppinterpreter2)
        (gppinterpreter2(car *args*))
    )
)

(main2)