; keyword tokens
(defun getTokens ()
	(concatenate 'list (getKeys) (getOperators)))

(defun getKeys ()
	(pairlis '("false" "true" "disp"  "load" "exit" "if" "for" "deffun" "set" "concat" "append" "list" "nil" "less" "equal" "not" "or" "and")
		'("KW_FALSE" "KW_TRUE" "KW_DISP" "KW_LOAD" "KW_EXIT" "KW_IF" "KW_FOR" "KW_DEFFUN" "KW_SET" "KW_CONCAT" "KW_APPEND" "KW_LIST" "KW_NIL" "KW_LESS" "KW_EQUAL" "KW_NOT" "KW_OR" "KW_AND")))

(defun getOperators ()
	(pairlis '("," "\"" "\"" "**" ")" "(" "*" "/" "-" "+")
		'("OP_COMMA" "OP_CC" "OP_OC" "OP_DBLMULT" "OP_CP" "OP_OP" "OP_MULT" "OP_DIV" "OP_MINUS" "OP_PLUS")))

; Trim values
(setq trimCharacters '(#\Space #\Newline #\Backspace #\Tab #\Return ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; DFA for identidier [a-zA-Z][a-zA-Z0-9]*
; it is accepting firstly alpha character then alpha and numeric character
(defun Tidentifier (tknid lst)
	(assert (isAlphabetic (subseq tknid 0 1)))

	(loop for c across tknid 
		do (if 
		(not (or (isAlphabetic c) (isNumeric c))) 
		(return-from Tidentifier (printError tknid c))))

	(let ((keyword (Tkeyword tknid lst)))
		(if (null keyword) (format nil "IDENTIFIER") keyword))
)
	
; DFA for value [0-9]|[-]?[1-9][0-9]*
; It is accepting positive integer just digit or any number which not start with zero   
(defun Tvalue (tknid)
	(assert (isNumeric (subseq tknid 0 1)))

	(setf tleng (length tknid))
	(cond 
		((> tleng 1) (isZero (subseq tknid 0 1)))
		((< tleng 1) (return-from Tvalue (printError tknid (subseq tknid 0 1))))
	)

	(loop for val across tknid 
		do (if (not (isNumeric val))
		(return-from Tvalue (printError tknid val))))

	(format nil "VALUE")
)

; defined token in gpp syntax
(defun Tkeyword (token lst)
	(setf value (assoc token lst :test #'equal))
	(if value (format nil "~a" (cdr value)) 
		nil)
)

; defined operators in gpp syntax
(defun Toperator (token lst)
	(setf value (assoc token lst :test #'equal))
	(if value (format nil "~a" (cdr value)) 
		nil)
)

; string token
(defun Tstring (token)
	(assert (isQuates (subseq token 0 1)))
	(format t "STRING")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; If there will be an undefined character print error
(defun printError (token c)
	(format t "SYNTAX ERROR: '~a' : '~a'" token c))
; print the token list
(defun prList (lst)
    (format nil "~{~A~}" lst))

; checking character for ascii 
(defun isBrackets (value)
	(let ((v (char-int (coerce value 'character))))
		(or (= v 40) (= v 41)))) ; 40:(  41:))

(defun isSpace (v) (char= v #\Space))


(defun isZero (value)
	(equal value "0"))	; ->0


(defun isSemicolon (value)
	(equal value ";"))	; ->;


(defun isQuates (value)
	(equal value "\"" ))


(defun isNumeric (value)
	(setf value (coerce value 'character))
	(and (char>= value #\0) (char<= value #\9)))
	

(defun isAlphabetic (value)
	(setf value (coerce value 'character))
	(and (char>= value #\A)  (char<= value #\z)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; triming non required and separeted character
(defun TrimNonRequiredChr (str)
	(string-trim trimCharacters str)
)

; split the values according to the space
(defun split (string &key (isSpace #'isSpace))

  (loop :for start = (position-if-not isSpace string)
    :then (position-if-not isSpace string :start (1+ end))
    :for end = (and start (position-if isSpace string :start start))
    :when start :collect (subseq string start end):while end)
)

; 
(defun splitSeq (seq)
	(setq seq (prList (map 'list 
		#'(lambda (c) 
		(if (isBrackets c) (concatenate 'string " " (string c) " ")
			(string c))) 
		(TrimNonRequiredChr seq))))

	;; in sequence, find the indices of spaces in between quo-mark
	(setf lenString (- (length seq) 1))
	(let ((tlist (loop for index from 0 to lenString
			when (eq (aref seq index) #\") collect index))
		  	(index1 '()) (index2 '()) (spaceIndex '()))
		(loop while tlist 
			do (setq index1 (car tlist)) 
			do (setq index2 (car (cdr tlist)))
			do (setq tlist (cdr (cdr tlist)))
			do (setq spaceIndex
			 	(loop for index from 0 to lenString
					when (and 
					(< index1 index) (> index2 index) 
					(eq (aref seq index) #\Space)) collect index)))

		(split (prList (loop for index from 0 to lenString
			if (member index spaceIndex) collect #\. else collect (aref seq index))))
	)
)

; pair the string keyword and token according to the first character
(defun connetWithToken (tknid lst)

	(let ((c (subseq tknid 0 1))) 
	 	(cond 	
	 		((isQuates c) (Tstring tknid))	
	 		((isNumeric c) (Tvalue tknid))
	 		((isAlphabetic c) (Tidentifier tknid lst))  
	 	   	(t (if (Toperator tknid lst) 					
	 	   	(Toperator tknid lst) (printError tknid c))))	
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; live interpreter shell for gpp lexer
(defun interpreterShell()
	(format t "$> ") 
	 (setf line (read-line))

	(if (string= (string line) "(exit)") 
		(bye)
		(interpreter line))
	(interpreterShell)
)

; for reading file and lex it 
(defun interpreterFile (fname)
	(let 
		((ret (open fname :if-does-not-exist nil)))		
	  	(when ret (loop for line = (read-line ret nil)
	        while line do (interpreter line)) (close ret)))
)


(defun interpreter(seq)
	(setf lst (splitSeq seq))

	(if (and (>= (length seq) 2) 
			(string= (subseq seq 0 2) ";;")) (format t "\"COMMENT\"~%")
		(map nil #'(lambda (token) (format t "~s~%" (connetWithToken token (getTokens)))) lst))
)

(defun gppinterpreter (&optional fname)
	(if fname (interpreterFile fname) (interpreterShell)))

(defun main ()
	 (if (null *args*)
        (gppinterpreter)
        (gppinterpreter (car *args*))
    )
)

; (main)

; (setf seq ";; helloworld.g++
; (deffun sumup (x)
; 	(if (equal x 0)
; 		1 
; 		(+ x (sumup (- x 1)))
; 	)
; )")

; (loop for x in (splitSeq seq) do (print x)) 

