block inner
  f1 : 16sl
  f2 : tag 8sb foropts 64sl = 0 | 32ub = 1
end

block outer
  f1 : 8u
  f2 : 32sl
  f3 : tag 16ub foropts 32sl = 0 | inner = 1
end
