;; Documentation for the module.
(defmodule newlfe
  (export (example_square 1)
          (foo 0)))

;; Documentation for the `example_square` function.
(defun example_square (x) (* x x))

(defun foo ()
  (Elixir.Game:hello))
