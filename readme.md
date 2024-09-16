# Forther


A forth *like* langauge.
When I started working on this I had no idea about Forth.
The only thing I knew was that I knew that it was stack-based.

This is why many of the implementations are much different than what would you
expect from a forth implementation.
This is also one reason as to why we have different types on the stack 
  (such as stacks, strings, words, exact (int) and inexact (double) numbers).

## Syntax

### Values

You can add any values to the stack like this:

```
1 2.3 "4" five false { 6 7.8 "9" ten }
```

This will add:

1. an `Exact` with value `1`.
2. an `Inexact` with value `2.3`.
3. a `String` with value `"4"`.
4. a `Word` with value `:five`.
5. a `Boolean` with value `false`.
6. a `List` / Stack with value `{ 6 7.8 "9" ten }`.


### If 

To check if a number is even or not you could for example use this:
```
{ "even" println . } { "odd" println . } x 2 | if
```

The general syntax follows this:
```
{ if-true } { if-false } <cond> if
```

### For

A word `:I` is inserted into the dictionary for each iteration
with the current iteration value. 

This:
```
{ I print ". line" println .  } 1 10 for
```
would print:
```
1. line
2. line
3. line
4. line
5. line
6. line
7. line
8. line
9. line
10. line
```

The general syntax follows this:
```
{ for-block (with :I in the dict) } from to for
```

### Built-In words

Use the `:words` word to see all words.
You can override any word at any time, even built-in ones.

## Examples

See `./examples/*.forther` for examples.

> [!NOTE]
> Right now forther cannot read from a file, thus
> the shebang does not actually work yet.<br>
> Run the files like this instead:
>
> `cat ./examples/fizzbuzz.forther | forther`

## Current Status (incomplete)

- ✅ A basic repl
  - ✅ If
  - ✅ For
  - ✅ Basic stack operations
  - ❌ While (to have conditional looping)
- 🚧 `ErrorCall` free (better error handling)
  - Error handling is done in multiple ways (through exceptions and the Result type).
    It would be nicer, to handle all internal (meaning all errors that I have control over)
    in the same way. This would also lead to better error messages.
- ✅ reading from a file
- 🚧 Compile mode 
- ❌ Dumping the current word list to a file
- ❌ JVM backend
- (❌) Maybe a cuomst vm?
- ❌ IO Operations / Graphs stuff for Advent of Code.

### Goal

I want to solve one day of Advent of Code with this.

