# The Power of Prolog

## Introduction to modern Prolog

Prolog is a **programming language** that is rooted in
formal&nbsp;logic. It supports *backtracking* and *unification*
as built-in features. Prolog allows us to elegantly&nbsp;solve
many&nbsp;tasks with short and general programs.

The goal of this material is to *bridge the&nbsp;gap* between the
great traditional Prolog textbooks of the&nbsp;past and the language
*as it currently is*, several decades after these books
were&nbsp;written. You will see that many limitations of the past are
no longer relevant, while several new constructs are now of great
importance even though they are not yet covered in any available
Prolog&nbsp;book.

## Reading this book

This books is *self-hosting*: It includes an HTTP&nbsp;server that
lets you browse most of the content locally.

To run it, you need SWI-Prolog&ge;**7.5.10**. To spawn the server on
port&nbsp;5053, use for example:

    $ swipl main.pl --port=5053 --interactive

Then, direct your browser to:

   http://localhost:5053/prolog

If you are new to Prolog, read the chapters in&nbsp;order for a
self-contained exposition of many important language features. If you
already have some experience with Prolog and would like to learn more
about more recent aspects, I recommend you start with the chapter on
integer&nbsp;arithmetic and proceed with the chapters it
links&nbsp;to.

## About this book

The latest version of this book is always available from:

**https://www.metalevel.at/prolog**

I will periodically synch this repository with new material as it
becomes available.

I started writing this book in 2005, when I was a student of
Ulrich&nbsp;Neumerkel in Vienna. Meta-interpreters formed the first
chapter.

You can run most of the examples with any popular Prolog system, such
as GNU&nbsp;Prolog, SICStus&nbsp;Prolog, YAP and&nbsp;SWI.
