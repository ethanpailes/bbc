

block test
  f6 : 16ub
  f8 : 32sl
end

block test2
  f9 : 32sl
  f10 : arrayf 16ul 512
end

block inner
  f3 : 32sl
  f4 : arrayf 64ub 256
  f5 : tag 8ub foropts test = 1 | test2 = 2
end


block outer
  f1 : 8u
  f2 : tag 8ub foropts 32ub = 1 | inner = 2 | 64sl = 3
end
