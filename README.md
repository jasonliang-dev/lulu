# lulu

lulu is a Lisp-like programming language that compiles to JavaScript.

## Examples

### Hello World!

```lisp
(console.log "Hello World!")
```

```
Hello World!
```

### Conditionals

```lisp
(let coinToss (gt (Math.random) 0.5))
(console.log (if coinToss "Heads" "Tails"))
```

```
Heads
```

### User Input (Browser)

```lisp
(let name (prompt "What's your name?"))
(console.log (concat "Hello " name "!"))
```

```
What's your name? Jason
Hello Jason!
```

<details>
  <summary>More examples</summary>

### User Input (Node)

```lisp
(let readline (require "readline"))
(let rl (readline.createInterface
         (dict
          (list "input" process.stdin)
          (list "output" process.stdout))))

(rl.question "What's your name? "
             (lambda (name)
               (console.log (concat "Hello " name "!"))
               (rl.close)))
```

```
What's your name? Jason
Hello Jason!
```

### setTimeout

```lisp
(console.log
 (map (lambda (str i)
        (let factor (mul i 800))
        (setTimeout (lambda () (console.log str))
                    factor)
        factor)
      (list "What if we put our minecraft"
            "beds next to each other?"
            "haha jk"
            "unless? (〃∀〃)ゞ")))
```

```
[ 0, 800, 1600, 2400 ]
What if we put our minecraft
beds next to each other?
haha jk
unless? (〃∀〃)ゞ
```

### Functions

```lisp
(defun factorial (n)
  (if (eq n 0)
      1
    (mul n (factorial (dec n)))))

(console.log (factorial 5))
```

```
120
```

### Constructor

```lisp
(defun Person (name age)
  (let this.name name)
  (let this.age age)
  ())

(console.log (new Person "Kiki" 12))
```

```
Person { name: 'Kiki', age: 12 }
```

</details>

## Building from source

```
stack build
stack run input.lulu output.js
node output.js
```

## Function List

Functions are defined in `src/LuluInclude.hs`.

| Function | Description                                                        |
| -------- | ------------------------------------------------------------------ |
| add      | Adds two numbers                                                   |
| sub      | Subtracts two numbers                                              |
| mul      | Multiplies two numbers                                             |
| div      | Divides two numbers                                                |
| mod      | Returns remainder after division of two numbers                    |
| inc      | Add 1                                                              |
| dec      | Subtract 1                                                         |
| eq       | Loose equality                                                     |
| lt       | True if left hand side is less than right hand side                |
| lte      | True if left hand side is less than or equal to right hand side    |
| gt       | True if left hand side is greater than right hand side             |
| gte      | True if left hand side is greater than or equal to right hand side |
| and      | Logical and                                                        |
| or       | Logical or                                                         |
| xor      | Logical xor                                                        |
| list     | Constructs an array                                                |
| range    | Create an array of numbers within a range. (last number excluded)  |
| length   | Get length of string, array, what have you                         |
| concat   | Concatenate strings, arrays, what have you                         |
| join     | Join an array (or array like) with a given separator               |
| fromList | Creates an object given a list of key value pairs                  |
| dict     | fromList, except with a variable argument list                     |
| assoc    | Add or update an entry in an object                                |
| dissoc   | Unset an entry in an object                                        |
| map      | Transforms each element in an array with a given callback function |
| filter   | Creates an array with elements that pass a predicate               |
| reduce   | Execute a reducer function for each element in an array            |

## BNF Grammar

```
         <program> ::= <opt-whitespace> | <sexpr> <opt-whitespace> <program>
           <sexpr> ::= <atom> | "(" <whitespace> <args> <whitespace> ")"
            <args> ::= <sexpr> | <sexpr> <whitespace> <sexpr>
            <atom> ::= "nil" | "t" | <number> | <name> | <string>
 
          <number> ::= <digit> | <digit> <number> | <number> "." <number>
            <name> ::= <alpha> | "_" | <name> <alphanumeric> | <name> "_" | <name> "." <name>
          <string> ::= '"' <text> '"'
            <text> ::= "" | <normal-char> <text> | <escape-char> <text>
     <escape-char> ::= "\"" | '\\' | '\b' | '\f' | '\n' | '\r' | '\t'
     <normal-char> ::= any character that isn't '"' or '\\'

  <opt-whitespace> ::= "" | <whitespace>
      <whitespace> ::= <whitespace-chars> | <whitespace-chars> <whitespace>
<whitespace-chars> ::= " " | "\n" | "\r"
    <alphanumeric> ::= <alpha> | <digit>
           <alpha> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
           <digit> ::= "0" | "1" | ... | "9"
```
