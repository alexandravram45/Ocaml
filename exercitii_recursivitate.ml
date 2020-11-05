(*Exercitiul 1 - progresia geometrica*)

(*a*)

let rec pg n = 
  if n = 0 then 3
  else 2 * pg (n - 1)

let rez1 = pg 3

(*b*)

let progresie_geo baza ratie n = 
  let rec pg_aux n = 
    if n = 0 then baza 
    else ratie * pg_aux (n - 1)
    in pg_aux n

let rez2 = progresie_geo 3 2 3

(*c*)

let rec pg_c baza ratie n =
  if n = 0 then baza 
  else ratie * pg_c baza ratie ( n - 1)

let rez3 = pg_c 3 2 3


(*Exercitiul 2- cmmdc *)

let rec cmmdc a b = 
  if b = 0 then a 
  else cmmdc b (a mod b)

let rez4 = cmmdc 32 48 


(*Exercitiul 5- cifra maxima/ minima *)


let cifra_maxima n =
let rec max_aux ciframax n =
  if n = 0 then ciframax
  else (
    if (n mod 10) > ciframax 
       then max_aux ( n mod 10 ) (n / 10) 
    else max_aux ciframax (n / 10)
  )
  in max_aux 0 n

  let rez5 = cifra_maxima 175865


(*Exercitiul 6- resturi modulo p *)


let resturi a p =
  let rec resturi_aux a p k = 
    if a = 0 || p = 0 then 0
    else (
        if p mod a = 0 then 0
          else (
              if  a mod p = 1 then k
                 else ( 
                   let putere = float_of_int  a ** (float_of_int ) ( k + 1 )  in 
                   resturi_aux (int_of_float putere ) p (k + 1)
    )
    ))
   in resturi_aux a p 1
  
let rez6 = resturi 4 7
let rez7 = resturi 2 8


;;