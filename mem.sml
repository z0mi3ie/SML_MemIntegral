(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 |  Vanilla Integral 
    estimates the integral of function f over the range from x1 to x2
    using the Riemann sum with increments of.1. You can assume that the
    x1 and x2 are real valued numbers out to tenths, allowing them to fit 
    more neatly into the incrementations

    fn : (fn : real -> real) -> real -> real -> real
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
local 
  val INCR = 0.1;
in
  fun integral f x1 x2 = if x1 <= ( x2 - INCR ) then ((f x1) * INCR ) + ( integral f (x1+(INCR)) x2 ) else 0.0
end

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 | Integral Memoization
   Returns a function that can calculate integrals in a memoized way. 
   This implementation uses a hash map implementation with (Real*Real)
   tuples. The integral function takes the current x+increment value,
   checks to see if the value is/isnt present in the hashmap. Depending,
   it will either retrieve the area value found in the table, or calculate,
   store, and return the stored value.  

   Note, the hashmap used in this implementation was used in the text book. 
   I modified it to use reals. 

        ('a -> bool) -> 'a permutationSeq -> 'a * 'a permutationSeq
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
 fun integralMem f = 
  let 
    (* >> Set the array size *)
    val ARRAYSIZE = 10000
    val INCR = 0.1

    (* >> Hash Function: get index in array << *)
    fun h(x) = Real.floor ( x * 10.0 ) mod ARRAYSIZE

    (* >> Check to see if two reals are equivalent << *)
    fun realEquals( a : real, b : real) =
      let
          val diff = a - b
      in
          Real.==(a,b)
      end

    (* >> Inserts the x:real value into the list *) 
    fun insertList( x , a , []  ) = [(x,a)]
      | insertList( x , a , (lx,la)::t ) =
           if realEquals( x,lx ) then (lx,la)::t 
           else (lx,la)::insertList(x,a,t)

    fun insert(x,a,A) =
       let
           val bucket = h(x);
           val L = Array.sub(A,bucket)
       in
           Array.update(A,bucket,insertList(x,a,L));
           a (* Area of rectangle from hash map *)
       end

    (* >> Looks up the x:real value from a list *)
    fun lookupList( x , []  ) = NONE
    |   lookupList( x , (lx,la)::t ) =
           if realEquals(x,lx) then SOME la
           else lookupList(x,t)

    fun lookup(x,A) = lookupList(x,Array.sub(A,h(x)))

    (* >> Headers : This is the bucket 
                    for the current function *)
    val headers = Array.array( ARRAYSIZE, [] : ( real * real ) list )

    (* >> Memoized Integral: This is the function I am going to return
                             When called with x1 and x2 values finds the integral 
                             over the saved function *)
    
    fun memoInt x1 x2 = case Real.<= (x1, (x2 - INCR )) of false => 0.0
                                                    | true  =>(case lookup(x1,headers) of SOME a  => a + ( memoInt (x1 + INCR) x2 ) 
                                                                                        | NONE    => insert (x1,((f x1)*INCR),headers) + (memoInt (x1+INCR) x2 ) )
    in
        memoInt
    end



