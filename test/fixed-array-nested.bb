

block inner
  f3 : 32sl
  f4 : arrayf 64ub 256
  f5 : 16ub
end


block outer
  f1 : 8u
  f2 : arrayf inner 4
  // f2 : tag 8ub foropts 32ub = 1 | inner = 2
end
