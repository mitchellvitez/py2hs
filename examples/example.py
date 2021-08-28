def lessThanOne(x):
    if x < 1:
        return True
    return False

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def fibonacci2(n):
    if n <= 1:
        return n
    else:
        return fibonacci2(n - 1) + fibonacci2(n - 2)

def allPairs(arrA, arrB):
    return [(a, b) for a in arrA for b in arrB]

def plusOne(x):
    return x + 1

def plusOneLambda():
    return lambda x: x + 1

def factorial(n):
    if n == 0:
        return 1
    return n * factorial(n - 1)

def squares(n):
    return [x**2 for x in range(1, n)]

def pythagoreanTriples(n):
    return [(a, b, c) for a in range(1, n) for b in range(1, n) for c in range(1, n)]

# unsupported
# def matmul(a, b):
#     return a @ b

def quadraticFormula(a, b, c):
    d = b ** 2 - 4 * a * c

    ans1 = (-b - sqrt(d)) / (2 * a)
    ans2 = (-b + sqrt(d)) / (2 * a)

    return ans1, ans2

# ideas for handling while loops, variable assignment, etc. either purely or via IO

# def divide(x, y):
#     count = 1
#     while x > 0:
#         x -= y
#         count += 1
#     return count

# divide x y = do
#   x <- newIORef x
#   y <- newIORef y

#   count <- newIORef 1

#   whileM (fmap (> 0) (readIORef x)) $ do
#     modifyIORef x (-y)
#     modifyIORef count (+1)

#   readIORef count

# divide2 x y =
#   go x 1
#     where
#   go x count =
#     if x > 0
#     then go (count + 1) (x - y)
#     else count
