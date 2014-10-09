(defparameter *grammar*
  '((sentence -> (noun-phrase verb-phrasePast) (noun-phrase verb-phrase) (noun-phrase verb-phrase prep-phrase) (noun-phrase prep-phrase relative-clause infinitive-phrase prep-phrase) (noun-phrase verb-phrase infinitive-phrase prep-phrase) (sentence Conjunction sentence) (SubordinatingConj sentence verb-phrase prep-phrase))
    (noun-phrase -> (Article Noun) (Noun) (Pronoun) (Adj Noun) (Adj NounPlural) (Adj noun-phrase) (Noun noun-phrase) (Article noun-phrase) (Article Adj Noun) (Determiner Adj Noun) (Determiner VerbPast noun-phrase) (Article Quantifier NounPlural) (Quantifier NounPlural) (noun-phrase Conjunction noun-phrase) (NounPlural))
    (verb-phrase -> (TransVerb noun-phrase) (AuxVerb TransVerb noun-phrase) (AuxVerb Adverb TransVerb noun-phrase) (AuxVerb Adverb LinkVerb Adj) (AuxVerb LinkVerb Adj))
    (verb-phrasePast -> (VerbPast noun-phrase))
    (prep-phrase -> (Prep noun-phrase) (Determiner Prep noun-phrase))
    (infinitive-phrase -> (InfParticle IntransVerb Adj) (InfParticle TransVerb noun-phrase))
    (relative-clause -> (relativePronoun IntransVerb))
    (Article -> the a)
    (Determiner -> this there such)
    (Noun -> man ball woman table education narrative polarization strategy intent job business health care asteroid eye prosecution death penalty case)
    (NounPlural -> gains industries services astronomers telescopes weeks)
    (Pronoun -> it)
    (relativePronoun -> that)
    (TransVerb -> counter achieve see seek)
    (IntransVerb -> tend pay)
    (VerbPast -> hit took saw liked were)
    (AuxVerb -> must will should)
    (LinkVerb -> be)
    (InfParticle -> to)
    (Adj -> higher prevailing flawed desired solid well professional visible naked amateur able decided)
    (Prep -> of as with in to)
    (Adverb -> hardly not)
    (Quantifier -> several few)
    (Conjunction -> and but)
    (SubordinatingConj -> whether))
  "A grammar for a trivial subset of English.")

(defun targeted-sentence (rules)
  (apply-rules rules nil)
)

;list of rules using DFS order
;(THE MAN LIKED A WOMAN)
(defparameter rules0 '((sentence 0) (noun-phrase 0) (Article 0) (Noun 0) (verb-phrasePast 0) (VerbPast 3) (noun-phrase 0) (Article 1) (Noun 2)))
;for sentence 1 of corpus.txt
(defparameter rules1 '((sentence 2) (noun-phrase 3) (Adj 0) (Noun 4) (verb-phrase 1) (AuxVerb 0) (TransVerb 0) (noun-phrase 8) (Article 0) (Adj 1) (Noun 5) (prep-phrase 0) (Prep 0) (noun-phrase 1) (Noun 6)))
;for sentence 2 of corpus.txt
(defparameter rules2 '((sentence 1) (noun-phrase 9) (Determiner 0) (Adj 2) (Noun 7) (verb-phrase 2) (AuxVerb 1) (Adverb 0) (TransVerb 1) (noun-phrase 8) (Article 0) (Adj 3) (Noun 8)))
;for sentence 3 of corpus.txt
(defparameter rules3 '((sentence 3) (noun-phrase 10) (Determiner 1) (VerbPast 4) (noun-phrase 5) (Adj 4) (noun-phrase 6) (Noun 9) (noun-phrase 14) (NounPlural 0)
  (prep-phrase 0) (Prep 3) (noun-phrase 12) (Quantifier 0) (NounPlural 1) (relative-clause 0) (relativePronoun 0) (IntransVerb 0) (infinitive-phrase 0) (InfParticle 0) (IntransVerb 1) (Adj 5)
  (prep-phrase 1) (Determiner 2) (Prep 1) (noun-phrase 13) (noun-phrase 1) (Noun 10) (Conjunction 0) (noun-phrase 13) (noun-phrase 4) (Adj 6) (NounPlural 2) (Conjunction 0) (noun-phrase 6) (Noun 11) (noun-phrase 1) (Noun 12)))
;for sentence 4 of corpus.txt
(defparameter rules4 '((sentence 5) (sentence 2) (noun-phrase 0) (Article 0) (Noun 13) (verb-phrase 3) (AuxVerb 1) (Adverb 1) (LinkVerb 0) (Adj 7) (prep-phrase 0) (Prep 4) (noun-phrase 8) (Article 0) (Adj 8) (Noun 14)
  (Conjunction 1) (sentence 4) (noun-phrase 4) (Adj 9) (NounPlural 3) (verb-phrase 4) (AuxVerb 2) (LinkVerb 0) (Adj 10) (infinitive-phrase 1) (InfParticle 0) (verb-phrase 0) (TransVerb 2) (noun-phrase 2) (Pronoun 0)
  (prep-phrase 0) (Prep 2) (noun-phrase 14) (NounPlural 4)))
;for sentence 5 of corpus.txt
(defparameter rules5 '((sentence 6) (SubordinatingConj 0) (sentence 2) (noun-phrase 0) (Article 0) (Noun 15) (verb-phrase 1) (AuxVerb 1) (TransVerb 3) (noun-phrase 7) (Article 0) (noun-phrase 6) (Noun 16) (noun-phrase 1) (Noun 17)
  (prep-phrase 0) (Prep 3) (noun-phrase 0) (Article 0) (Noun 18) (verb-phrase 4) (AuxVerb 1) (LinkVerb 0) (Adj 11) (prep-phrase 0) (Prep 3) (noun-phrase 11) (Article 1) (Quantifier 1) (NounPlural 5)))


;take a set of rules and the current sentence that has been generated so far
;here is what happens for the above example:
;when the first rule is called, the current sentence is empty and is rewritten to (noun-phase verb-phrase)
;second rule: (Article Noun verb-phrase)
;3: (THE Noun verb-phrase)
;4: (THE MAN verb-phrase)
;5: (THE MAN Verb noun-phrase)
;6: (THE MAN LIKED noun-phrase)
;7: (THE MAN LIKED Article Noun)
;8: (THE MAN LIKED A Noun)
;9: (THE MAN LIKED A WOMAN)


;ok to repeat InfParticle, Preopositions, Articles, Determiners, and Conjunctions
(setq OKtoRepeat '(to of as with in to the a this there such and but))
(setq onlyFive '(man ball woman table education narrative polarization strategy intent job business health care asteroid eye prosecution death penalty case gains industries services astronomers telescopes weeks it)) ;list of nouns, pluralNouns, and pronouns that should only be used 5 times

(defun apply-rules (rules sentence)
  (cond
    ((null rules) sentence)
    ((null sentence) (apply-rules (rest rules) (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
    (t (let ((rule-to-rewrite (car (car rules))) (new-rule (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
      (apply-rules (rest rules) (rewrite-sentence nil sentence rule-to-rewrite new-rule))))))

;simply rewrites a sentence replacing the first occurence of the variable "rule-to-rewrite" in "sentence-next" by the symbols in "new-rule" 
;example: (rewrite-sentence nil '(THE MAN verb-phrase) 'verb-phrase '(Verb noun-phrase))
;returns (THE MAN Verb noun-phrase)
(defun rewrite-sentence (sentence-pre sentence-next rule-to-rewrite new-rule)
    (cond ((null sentence-next) sentence-pre)
    (t
      (if (equal (car sentence-next) rule-to-rewrite)
      (append (append sentence-pre (if (listp new-rule) new-rule (list new-rule))) (rest sentence-next))
      (rewrite-sentence (append sentence-pre (list (car sentence-next))) (rest sentence-next) rule-to-rewrite new-rule)))))


(defun random-elt (list)
  (elt list
       (random (length list))))

(setq n 0) ;initialize tree depth
(setq maxn 0) ;initialize overall tree_depth
(setq sentcount 0) ;going to keep track of how many times the non-terminal 'sentence' is used(
(setq sentn '(sentence placeholder))

(defun random-sentence (phrase)
  "Generate a random sentence or phrase"
  (setq n (+ n 1)) ;Add 1 to tree depth every time you get here
  (setq phraseB phrase)
  (if (equal phrase (car sentn)) (setq sentcount (+ sentcount 1)))
  (cond ((listp phrase)
         (mappend #'random-sentence phrase))
        ((rewrites phrase) ;rewrite left-hand side as right-hand side
         (random-sentence (random-elt (rewrites phrase))))
        (t
          (if (> n maxn)
            (setq maxn n))
          (setq n 0)
          (list phrase))));adds n to n_lst

(setq repeated '());initialize list for any repeated word in the phrase
(setq badRepeats '());initialize the list where the wrongly repeated words will be stored
(setq nouncount 0)
(defun repeatedWords (phrase)
  "Determines if there are any wrongly repeated words"
  (dotimes (m (length phrase))
    (setq word (car phrase));first word
    (setq phrase (cdr phrase));rest of phrase
    (if (not (eql (member word phrase) NIL)) (push word repeated)));stores the repeated words
    (if (not (eql (member word onlyFive) NIL)) (setq nouncount (+ nouncount 1))) ;if word is a noun, simultaneously increase nouncount
  (dotimes (m (length repeated))
    (setq rword (car repeated));first word of repeated list
    (setq repeated (cdr repeated))
    (if (eql (member rword OKtoRepeat) NIL) (push rword badRepeats))));compares repeated words to those that are ok to repeat

;setting variables for generateValid
(setq N1 40) ;max number of words
(setq N2 10) ;max tree depth

(defun validp (phrase)
  "Tests if a phrase is valid"
  (repeatedWords phrase)
  (if (< (length phrase) N1)
    (if (< maxn N2)
      (if (eql badRepeats NIL)
        (if (<= nouncount 5)
          (if (<= sentcount 3)
            (list phrase)))))))

(defun generateValid (phrase)
  "Generate a valid random sentence or phrase"
  (setq maxn 0);clear variables if there is anything here from the last time random-sentence ran
  (setq badRepeats '())
  (setq repeatedWords '())
  (setq nouncount 0)
  (setq sentcount 0)
  (setq sent (random-sentence 'sentence))
  (if (not (eql (validp sent) NIL)) (list sent) (resetGV phrase))) ;if phrase is valid, return the phrase - if not, reset and repeat.

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(random-sentence 'sentence)
