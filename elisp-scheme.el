;;; elisp-scheme.el --- Interpreter in elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Michael Hoffmann

;; Author: Michael Hoffmann
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Rudimentary interpreter based on the one taught in the book The
;; Little Schemer.

;;; Code:

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name (first entry) (second entry) entry-f))

(defun lookup-in-entry-help (name names values entry-f)
  (cond ((null names) (funcall entry-f name))
        ((eq (car names) name) (car values))
        (t (lookup-in-entry-help name (cdr names) (cdr values) entry-f))))

(defalias #'new-entry #'list)
(defalias #'extend-table #'cons)

(defun lookup-in-table (name table table-f)
  (cond ((null table) (funcall table-f name))
        (t (lookup-in-entry name (car table)
                            (lambda (name)
                              (lookup-in-table
                               name (cdr table) table-f))))))

(defun value (e)
  (meaning e nil))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(defun expression-to-action (e)
  (cond ((atom e) (atom-to-action e))
        (t (list-to-action e))))

(defun atom-to-action (e)
  (cond ((numberp e) #'*const)
        ((eq e t) #'*const)
        ((eq e nil) #'*const)
        ((eq e (quote cons)) #'*const)
        ((eq e (quote car)) #'*const)
        ((eq e (quote cdr)) #'*const)
        ((eq e (quote eq?)) #'*const)
        ((eq e (quote null?)) #'*const)
        (t #'*identifier)))

(defun list-to-action (e)
  (cond ((atom (car e))
         (cond ((eq (car e) (quote quote)) #'*quote)
               ((eq (car e) (quote lambda)) #'*lambda)
               ((eq (car e) (quote cond)) #'*cond)
               (t #'*application)))
        (t #'*application)))

(defun *const (e table)
  (cond ((numberp e) e)
        ((eq e t) t)
        ((eq e nil) nil)
        (t (list (quote primitive) e))))

(defun *identifier (e table)
  (lookup-in-table e table #'initial-table))

(defun *quote (e table)
  (text-of e))

(defalias #'text-of #'second)

(defun *lambda (e table)
  (list (quote non-primitive)
        (cons table (cdr e))))

(defun *cond (e table)
  (evcon (cond-lines-of e) table))

(defalias #'cond-lines-of #'cdr)

(defun evcon (lines table)
  (cond ((else? (question-of (car lines)))
         (meaning (answer-of (car lines)) table))
        ((meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table))
        (t (evcon (cdr lines) table))))

(defun else? (x)
  (and (atom x) (eq x (quote else))))

(defalias #'question-of #'first)
(defalias #'answer-of #'second)

(defun *application (e table)
  (:apply
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(defalias #'function-of #'car)
(defalias #'arguments-of #'cdr)

(defun evlis (args table)
  (cond ((null args) ())
        (t (cons (meaning (car args) table)
                 (evlis (cdr args) table)))))

(defun :apply (fun vals)
  (cond ((primitive? fun) (apply-primitive (second fun) vals))
        ((non-primitive? fun) (apply-closure (second fun) vals))))

(defun primitive? (l)
  (eq (first l) (quote primitive)))

(defun non-primitive? (l)
  (eq (first l) (quote non-primitive)))

(defun apply-primitive (name vals)
  (cond ((eq name (quote cons)) (cons (first vals) (second vals)))
        ((eq name (quote car)) (car (first vals)))
        ((eq name (quote cdr)) (cdr (first vals)))
        ((eq name (quote eq?)) (eq (first vals) (second vals)))
        ((eq name (quote null?)) (null (first vals)))))

(defun apply-closure (closure vals)
  (meaning (body-of closure)
           (extend-table
            (new-entry
             (formals-of closure) vals)
            (table-of closure))))

(defalias #'table-of #'first)
(defalias #'formals-of #'second)
(defalias #'body-of #'third)

(value '(eq? nil nil))

(provide 'elisp-scheme)
;;; elisp-scheme.el ends here
