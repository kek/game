(defmodule newlfe-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "./deps/ltest/include/ltest-macros.lfe")

(deftest example_square-test
  (is-equal 64 (newlfe:example_square 8)))

 (deftest example_square-test2
    (is-equal 'world (Elixir.Game:hello)))

 (deftest example_square-test3
   (is-equal 'world (newlfe:foo)))
