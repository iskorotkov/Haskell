main = print (_gcd 24 176)

_gcd a 0 = a
_gcd m n = _gcd n (mod m n)
