
st =. 3 : '> 0 { y'
is =. 3 : '> 1 { y'
pc =. 3 : '> 2 { y'
fp =. 3 : '> 3 { y'

next =. (>:&pc { is)

print =. 3 : 'y (1!:2) 2'

adv =. 3 : '(2 }.st y); (is y); (2+pc y); ((fp y) - 2)'
jmp =. 3 : '(2 }.st y); (is y); (next y); ((fp y) - 2)'

i1  =: 3 : '(((next y) # 0),st y); (is; 2&+&pc; next+fp) y'
i2  =: 3 : '((next,st); is ; 2&+&pc; >:&fp) y'
i3  =: 3 : '((((fp-next) { st),st); is; 2&+&pc; >:&fp) y'
i4  =: 3 : '(}.({.st y) ((fp-next)y) } st y); (is; 2&+&pc; <:&fp) y'
i5  =: 3 : '((+/1 0 { st y),2}.st y); (is; >:&pc; <:&fp) y'
i6  =: 3 : '((-/1 0 { st y),2}.st y); (is; >:&pc; <:&fp) y'
i7  =: 3 : '((*/1 0 { st y),2}.st y); (is; >:& pc; <:&fp) y'
i8  =: 3 : '((<.%/1 0 { st y),2}.st y); (is; >:&pc; <:&fp) y'
i9  =: 3 : '(st; is; next; fp) y'
i10 =: 3 : '(adv ` jmp @. (-.=/1 0 {st y)) y'
i11 =: 3 : '(adv ` jmp @. (>:/1 0 {st y)) y'
i12 =: 3 : '(adv ` jmp @. (<:/1 0 {st y)) y' 
i13 =: 3 : '(((({.&st), fp, 2&+&pc),}.& st) y); (is y); (next y);0'
i14 =: 3 : '(print {. st y)]((}.&st); is; (>:&pc); (<:&fp)) y'
i15 =: 3 : 0
   fp1 =. (>:&fp { st) y
   pc1 =. (2&+&fp { st) y
   (({. st y),(3+fp y)}. st y) ; (is y); pc1 ; fp1
)

step =: 3 : 0
   instr =. (pc { is) y
NB.   print (st y); (pc y); instr;fp y
   (]`i1`i2`i3`i4`i5`i6`i7`i8`i9`i10`i11`i12`i13`i14`i15  @. instr) y
)

state0 =:  ($0); instrs; 0; 0

step ^: _ state0
