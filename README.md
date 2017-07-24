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

This book is *self-hosting*: It includes an HTTP&nbsp;server that lets
you browse most of the content locally. When you access files that are
*not* locally available, it redirects to their online&nbsp;versions.

To run it, you need SWI-Prolog&ge;**7.5.10**. To spawn the server on
port&nbsp;5053, use for example:

    $ swipl main.pl --port=5053 --interactive

Then, direct your browser to:

   http://localhost:5053/prolog

If you are new to Prolog, read the chapters in&nbsp;order for a
self-contained exposition of many important language features. If you
already have some experience with Prolog and would like to learn more
about more recent aspects, I recommend you start with the chapter on
[integer&nbsp;arithmetic](https://www.metalevel.at/prolog/clpfd) and
proceed with the chapters it links&nbsp;to.

## About this book

The latest version of this book is always available from:

**https://www.metalevel.at/prolog**

I will periodically synch this repository with new material as it
becomes available.

I started writing this book in 2005, when I was a student of
Ulrich&nbsp;Neumerkel in Vienna. A collection of
[Prolog&nbsp;meta-interpreters](https://www.metalevel.at/acomip/)
formed the first chapter.

You can run most of the examples with any popular Prolog system, such
as GNU&nbsp;Prolog, SICStus&nbsp;Prolog, YAP and&nbsp;SWI. In some
cases, slight modifications may be necessary to adjust for different
libraries or small variations. See your Prolog system's manual, or ask
on&nbsp;Stackoverflow and `comp.lang.prolog` for more information.

## On teaching Prolog

Prolog is an interesting programming language: It has a *pure*
monotonic core, and it also has features that are
called&nbsp;*impure*. You, the programmer, must decide in which subset
of the language you want to program.

In a sense, programming in Prolog is like writing a&nbsp;poem: You can
achieve great effects by writing under stringent constraints.
In&nbsp;Prolog, you often get the best results by restricting
your&nbsp;work to the pure&nbsp;core of the&nbsp;language. As long as
you do this, you can benefit from strong logical properties which let
you reason about your programs in systematic ways that are not
available in most other programming&nbsp;languages.

In the past decades, many pure features have become widely available
in Prolog&nbsp;systems. This book explains many of these new features,
and shows you how you can use them to achieve general and efficient
Prolog&nbsp;programs.

<pre>
Le message dur:
<b>Restez purs!</b>
</pre>
