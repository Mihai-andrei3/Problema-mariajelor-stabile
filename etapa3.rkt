  #lang racket
(require ( lib "trace.ss"))
(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.


(define (get-unstable-couples engagements mpref wpref)
  (let iteration ((it-eng engagements) (res '())) ;iteram prin logodne
    (if (null? it-eng) res ;daca am trecut prin toate intoarcem rezultatul
        (let* ((eng (car it-eng))
               (fem (car eng)) ;femeia din logodna curenta
               (masc (cdr eng)) ;barbatul din logodna curenta
               (current-res (if (or (better-match-exists? fem masc (get-pref-list wpref fem) mpref (map (lambda (pair) (cons (cdr pair) (car pair))) engagements)) 
                                (better-match-exists? masc fem (get-pref-list mpref masc) wpref engagements))
                            (cons (cons fem masc) res) ;daca unul dintre parteneri prefera pe altcineva atunci adaugam in cuplul in rezultaul final
                            res) ;altfel rezultaul ramane la fel
                        )
               )
          (iteration (cdr it-eng) current-res))))) ;trecem la urmatoarea logodna si salvam rezultatul




; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.

(define (engage free-men engagements mpref wpref)
  (let men_iteration ( (free free-men) (res  engagements)) ;iteram prin barbatii logoditi
    (if (null? free) res ;am repartizat toti barbatii si intoarcem rezultatul
        (let* ((single (car free)) ;barbatul curent
               (pref (get-pref-list mpref single))) ;lista lui de preferinte
          (let fem_iteration ( (it-pref pref) )  ;iteram prin lista de preferinte a barbatului
            (if (null? it-pref) (men_iteration (cdr free) res) ;trecem la barbatul urmator daca s au parcurs toate femeile
                (let* ( (fem (car it-pref)) ;femeia curenta
                        (fem-pref (get-pref-list wpref fem)) ;lista ei de preferinte
                        (partener (get-partner res fem)) ;partenerul femeii
                       )
                  (cond
                    ((equal? partener #f) (men_iteration  (cdr free) (cons (cons fem single) res) ))
                    ((preferable? fem-pref single partener ) (men_iteration  (cons partener (cdr free)) (update-engagements res fem single)))
                    (else (fem_iteration (cdr it-pref) ))))))))))
 




; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men mpref) '() mpref wpref))


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
   (foldl (lambda (pair acc) (append (list (car pair)) (list (cdr pair)) acc ) ) '() pair-list))
