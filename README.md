# wordbuck
An interpreter for a Javaish language

## About the project
I am creating an interpreter written in the Scheme programming language. It is inspired by an interpreter for a Java like language that I did a few months ago, but the language is going to be different. I'm calling it Javaish for now, mostly because it is going to be object oriented, but I plan to change it a lot.

## The `wordbuck` language
The `wordbuck` language is an mashup of a lot of language concepts that I've come across in the past year or so. I've used Java quite a lot in my life, so when I think of an object-oriented language, that is usually where I start, but I want to do things differently, mostly just to see how things turn out.

Features:
- Everything is an object
- Every object has a `_value_()` method that returns the value of the object
- unless otherwise noted, object `a` is evaluated as `a._value_()`
- Functions are objects which return a function when their `_value_()` is called
- Numbers/primitives are objects which only have a `_value_()`. The interpreter will use normal numbers, but if the call is used in a program, it will return the object.
