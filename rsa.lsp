; ==================================
; 			GENERATE PRIME
; ==================================
; ******** LOOP version ********
; (defun divisible-by (number list)
; 	"return true if divisible by an element in the list"
; 	(loop for i in list
; 		thereis (= (mod number i) 0)
; 	)
; )
; ***** FUNCTIONAL version *****
(defun divisible-by(number lst)
	"return true if divisible by an element in the list"
	(> 	(count 
			'0
			(mapcar #'(lambda(i) (mod number i)) lst)
		)
		0
	)
)

; DETERMINE all primes in range [current..max-value]
(defun prime-list-generator (current max-value prime-list)
	"business end of gen"
	(if (= current max-value)
		prime-list
		(if (divisible-by current prime-list)
			(prime-list-generator (+ current 1) max-value prime-list)
			(prime-list-generator (+ current 1) max-value (nconc prime-list (list current)))
		)
	)
)

; GENERATE prime in range [1..n]
(defun gen-prime(n)
	; "Find the sum of all the primes below n"
	; (time (reduce #'+ (prime-list-generator 2 n '())))
	(setq lst (prime-list-generator 2 n '()))
	(nth (random (length lst)) lst)
)

; ==================================
; 		GET coprime value of N
; ==================================
; ******** LOOP version ********
; (defun get-coprime(n)
; 	(setq prime-list (reverse (prime-list-generator 2 n '())))	; HERE <- cdr (edited)
; 	(loop for p in prime-list
;         when (= (gcd p n) 1)
;         return p
; 	)
; )
; ***** FUNCTIONAL version *****
(defun get-coprime(n)
	(find-if 
		#'(lambda(p) (= (gcd p n) 1)) 
		(reverse (prime-list-generator 2 n '()))
	)
)

; ==============================================================
; DETERMINE the modular multiplicative inverse of e in modulo n
; ==============================================================
; ******** LOOP version ********
; (defun inverse-modulo(e n)
; 	(loop for d from 1 to n
; 		when (= (mod (* d e) n) 1)
; 		return d
; 	)
; )
; ***** FUNCTIONAL version *****
(defun gen-range-help(i N result)
	(if (<= i N)
		(gen-range-help (+ i 1) N (nconc result (cons i Nil)))
		result
	)
)

(defun gen-range(n)
	(gen-range-help 1 n '())
)

(defun inverse-modulo(e n)
	(find-if 
		#'(lambda(x) (= (mod (* x e) n) 1))
		(gen-range n)
	)
)

; ========================================================
"		 			RSA algorithm						 "
; Source: https://en.wikipedia.org/wiki/RSA_(cryptosystem)
; Author: Binh D. Nguyen
; ========================================================
(defun rsa(range)
	; First step: Generate 2 primes (p, q).
	(setq p (gen-prime range))
	(setq q (gen-prime range))
	
	; Second step: Calculate N = p*q - the KEY length.
	(setq n (* p q))
	
	; Third step: Calculate totient of N - λ(N). This value is kept private!
	(setq totient (lcm (- p 1) (- q 1)))
	
	; Fourth step: Choose number e in order that e and λ(N) are coprime.
	(setq ke (get-coprime totient))
	
	; Fifth step: Determine d - the modular multiplicative inverse of e in modulo λ(N)
	(setq kd (inverse-modulo ke totient))
	
	; Return keys
	; The public key consists of the modulus N and the public (or encryption) exponent E. 
	; The private key consists of the modulus n and the private (or decryption) exponent D, which must be kept secret. 
	; P, Q, and λ(N) must also be kept secret because they can be used to calculate D.
	(format T "+ The public  key is (n = ~D, e = ~D).~%" n ke)
	(format T "+ The private key is (n = ~D, d = ~D).~%" n kd)
)

;  The encryption function c(m)
(defun encrypt(m ke kn)
	(mod (expt m ke) kn)
)

; The decryption function m(c)
(defun decrypt(c kd kn)
	(mod (expt c kd) kn)
)