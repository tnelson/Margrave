#lang s-exp "../policy-p4p.rkt"

define( foo(n), n)

foo(1)

foo(2)

Policy( uses, myvoc,
            Variables(s(Subject), a(Action)),
            Rules())