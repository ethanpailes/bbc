

// Inner is variable length because it contains a sum type
block inner
  f3 : 32sl
  f4 : arrayf 64ub 256
  f5 : tag 8ub foropts 32ul = 1 | 64ul = 2
end

block outer
  f1 : 8u
  f4 : arrayf inner 256
end
