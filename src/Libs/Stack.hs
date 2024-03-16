module Libs.Stack where

type Stack t = [t]

push :: Stack t -> t -> Stack t
push stack element = element : stack

peek :: Stack t -> t
peek = head

pop :: Stack t -> Stack t
pop = tail

emptyStack :: Stack t
emptyStack = []