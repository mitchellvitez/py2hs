# py2hs

Hacky Python-to-Haskell transpiler for a tiny subset of Python

The transpiler only works on pure functions at the moment, and doesn't even support the entire expression subset of Python. All it does is generate code strings directly from a Python AST.

## Usage

`stack run` will convert `examples/example.py` into Haskell and pipe it to stdout.

## Examples

In Haskell, every `then` has a corresponding `else`. This isn't true in Python, meaning sometimes it can be tricky to correctly generate an `if` expression. However, if your function is of this fairly common form...

```python
def lessThanOne(x):
    if x < 1:
        return True
    return False
```

...then the transpiler can do the right thing and moves the returned expression into the `if`:

```haskell
lessThanOne x =
  if x < 1
  then True
  else False
```

For similar reasons, both of these Fibonacci examples transpile to the same function body:

```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def fibonacci2(n):
    if n <= 1:
        return n
    else:
        return fibonacci2(n - 1) + fibonacci2(n - 2)
```

The `else` and `return` collapse into the same expression in both cases.

```haskell
fibonacci n =
  if n <= 1
  then n
  else fibonacci (n - 1) + fibonacci (n - 2)
```

Although all types are left to be inferred at a later time, py2hs respects the difference between a function call and a lambda.

```python
def plusOne(x):
    return x + 1

def plusOneLambda():
    return lambda x: x + 1
```

```haskell
plusOne x =
  x + 1

plusOneLambda  =
  \x -> x + 1
```

Besides basic operators and function calls, py2hs can transpile list comprehensions. Generated code currently comes with a couple functions to define slicing and list ranges, but set/dict/generator comprehensions are not supported like lists are.

```python
def allPairs(arrA, arrB):
    return [(a, b) for a in arrA for b in arrB]

def squares(n):
    return [x**2 for x in range(1, n)]

def truples(n):
    return [(a, b, c) for a in range(1, n) for b in range(1, n) for c in range(1, n)]
```

```haskell
allPairs arrA arrB =
  [ (a, b) | a <- arrA, b <- arrB ]

squares n =
  [ x ^ 2 | x <- range 1 n ]

truples n =
  [ (a, b, c) | a <- range 1 n, b <- range 1 n, c <- range 1 n ]
```

Finally, py2hs can transform simple assignments into let expressions, so it's good enough for most basic math calcuations. However, it doesn't know anything about let block formatting so the generated code is not very idiomatic.

```python
def quadraticFormula(a, b, c):
    d = b ** 2 - 4 * a * c

    ans1 = (-b - sqrt(d)) / (2 * a)
    ans2 = (-b + sqrt(d)) / (2 * a)

    return ans1, ans2
```

```haskell
quadraticFormula a b c =
  let d = b ^ 2 - 4 * a * c in
  let ans1 = (-b - sqrt d) / (2 * a) in
  let ans2 = (-b + sqrt d) / (2 * a) in
  (ans1, ans2)
```

If you try to use something that isn't supported, partial code will be generated, and then you'll (hopefully) see an error in the generated output.

```python
# unsupported
def matmul(a, b):
    return a @ b
```

```haskell
Error transpiling example:39:14
@ is not supported
```

##  Future Ideas

Some ideas, in case I pick up this project again. The most obvious hurdle to making this project more useful is deciding how to handle IO.

For example, given this Python code

```python
def divide(x, y):
    count = 1
    while x > 0:
        x -= y
        count += 1
    return count
```

it might be easiest to translate it directly using `IORef` for variables, and monadic control functions (e.g. `whileM`). A reasonably direct Haskell translation might be this:

```haskell
divide x y = do
  x <- newIORef x
  y <- newIORef y

  count <- newIORef 1

  whileM (fmap (> 0) (readIORef x)) $ do
    modifyIORef x (-y)
    modifyIORef count (+1)

  readIORef count
```

However, we'd need a way to decide which functions need IO and which don't, otherwise the currently-supported pure functions would become really strange and unidiomatic. Supporting idiomatic code seems tough in general, since the languages are so different. In fact, a much more idiomatic Haskell version of `divide` might be this:

```haskell
divide2 x y =
  go x 1
    where
      go x count =
        if x > 0
        then go (count + 1) (x - y)
        else count
```

This would be harder to generate, and in general it may be very difficult to transpile arbitrary functions this nicely by merely generating strings of code directly from an AST.
