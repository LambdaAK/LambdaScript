// Standard prelude: typeclass hierarchy (implement instances in user code).

// List data type definition
  type rec List<a> =
    | []
    | (::) of (a, List<a>)
  
  type Option<a> =
    | None
    | Some of a
  
  type Ordering =
    | LT
    | EQ
    | GT
  
  // The Functor typeclass abstracts over data types that can be mapped over.
  // A Functor represents types that can be “lifted” with a function from (a -> b)
  // so that the function is applied inside the container/context f<_>.
  // For example, List is a Functor because you can map a function over every element in the list.
  //
  // The main law-abiding operation provided by Functor is `fmap`, which has the type:
  //   fmap : (a -> b) -> f<a> -> f<b>
  // - The first argument is any function taking a value of type a and returning b.
  // - The second argument is a value inside the contextual/container type f containing elements of type a.
  // - The result is the container with its contents having undergone the transformation.
  // The Functor typeclass abstracts over data types that can be mapped over.
  // fmap applies a function to the contents of a container/context type.
  trait Functor<f<_>> where
    // fmap : (a -> b) -> f<a> -> f<b>
    //   - Takes a function from a to b,
    //   - and a container f containing elements of type a,
    //   - returns a container f containing elements with the function applied.
    val fmap : (a -> b) -> f<a> -> f<b>
  end
  
  // Applicative is a Functor with application of wrapped functions to wrapped values,
  // and with the ability to lift raw values into the context.
  trait Applicative<f<_>> requires Functor<f> where
    // ap : f<a -> b> -> f<a> -> f<b>
    //   - Takes a container of functions (a -> b) and a container of a's,
    //   - returns a container of b's with functions applied to values.
    val ap : f<a -> b> -> f<a> -> f<b>
    // pure : a -> f<a>
    //   - Lifts a raw value into the context/container.
    val pure : a -> f<a>
  end
  
  // Monad is an Applicative which can sequence operations that produce wrapped values.
  trait Monad<f<_>> requires Applicative<f> where
    // bind : f<a> -> (a -> f<b>) -> f<b>
    //   - Takes a container of a's and a function producing containers of b's,
    //   - returns the flattened result of applying the function.
    val bind : f<a> -> (a -> f<b>) -> f<b>
    // (>>=) : f<a> -> (a -> f<b>) -> f<b>
    //   - Infix operator alias for bind.
    val (>>=) : f<a> -> (a -> f<b>) -> f<b>
    // Default definition of (>>=) in terms of bind for convenience.
    let (>>=) x f = bind x f
  end
  
  // Alternative provides a choice operator for Applicative functors.
  trait Alternative<f<_>> requires Applicative<f> where
    val aempty : f<a>
    val (<|>) : f<a> -> f<a> -> f<a>
  end
  
  // Foldable abstracts over container types whose elements can be combined/folded.
  trait Foldable<f<_>> where
    // fold_left : (b -> a -> b) -> b -> f<a> -> b
    //   - Left-associative fold of the container with a combining function and initial accumulator value.
    val fold_left : (b -> a -> b) -> b -> f<a> -> b
    // fold_right : (a -> b -> b) -> b -> f<a> -> b
    //   - Right-associative fold of the container with a combining function and initial accumulator value.
    val fold_right : (a -> b -> b) -> b -> f<a> -> b
  end
  
  // Bifunctor abstracts over data types with two type parameters that can both be mapped.
  trait Bifunctor<f<_, _>> where
    // bimap : (a -> c) -> (b -> d) -> f<a, b> -> f<c, d>
    //   - Maps the first type parameter with one function,
    //   - and the second parameter with another function.
    val bimap : (a -> c) -> (b -> d) -> f<a, b> -> f<c, d>
  end
  
  // Show provides a human-readable string representation of a type.
  trait Show<a> where
    val show : a -> String
  end
  
  // Eq provides equality comparison for a type.
  trait Eq<a> where
    val (==) : a -> a -> Bool
    val (!=) : a -> a -> Bool
    let (!=) x y = if (==) x y then false else true
  end
  
  // Ord provides ordering comparison for a type.
  trait Ord<a> requires Eq<a> where
    val compare : a -> a -> Ordering
    val (<) : a -> a -> Bool
    val (<=) : a -> a -> Bool
    val (>) : a -> a -> Bool
    val (>=) : a -> a -> Bool
    let (<) x y = case compare x y do | LT -> true | _ -> false
    let (<=) x y = case compare x y do | GT -> false | _ -> true
    let (>) x y = case compare x y do | GT -> true | _ -> false
    let (>=) x y = case compare x y do | LT -> false | _ -> true
  end
  
  // Semigroup represents types with an associative binary operation called mappend.
  trait Semigroup<a> where
    // mappend : a -> a -> a
    //   - Binary operation combining two values of type a.
    val mappend : a -> a -> a
    val (++) : a -> a -> a
  end
  
  // Monoid is a Semigroup with an identity element.
  trait Monoid<a> requires Semigroup<a> where
    // empty : a
    //   - The identity element such that mappend empty x = x and mappend x empty = x.
    val empty : a
  end
  
  // Ordering
  
  impl Eq for Ordering where
    (==) x y =
      case (x, y) do
      | (LT, LT) -> true
      | (EQ, EQ) -> true
      | (GT, GT) -> true
      | _ -> false
  end
  
  impl Show for Ordering where
    show x =
      case x do
      | LT -> "LT"
      | EQ -> "EQ"
      | GT -> "GT"
  end
  
  // Int
  
  impl Show for Int where
    show = int_to_str
  end
  
  impl Eq for Int where
    (==) x y = int_eq x y
  end
  
  impl Ord for Int where
    compare x y = int_compare x y
  end
  
  // Bool
  
  impl Show for Bool where
    show x = if x then "true" else "false"
  end
  
  impl Eq for Bool where
    (==) x y = bool_eq x y
  end
  
  impl Ord for Bool where
    compare x y = bool_compare x y
  end
  
  // Unit
  
  impl Show for Unit where
    show () = "()"
  end
  
  impl Eq for Unit where
    (==) x y = unit_eq x y
  end
  
  impl Ord for Unit where
    compare x y = unit_compare x y
  end
  
  // String
  
  impl Show for String where
    show s = s
  end
  
  impl Eq for String where
    (==) x y = str_eq x y
  end
  
  impl Ord for String where
    compare x y = str_compare x y
  end
  
  impl Semigroup for String where
    mappend x y = str_concat x y
    ,
    (++) x y = mappend x y
  end
  
  impl Monoid for String where
    empty = ""
  end
  
  // Float
  
  impl Eq for Float where
    (==) x y = float_eq x y
  end
  
  impl Ord for Float where
    compare x y = float_compare x y
  end
  
  // Char
  
  impl Eq for Char where
    (==) x y = char_eq x y
  end
  
  impl Ord for Char where
    compare x y = char_compare x y
  end
  
  // List
  
  impl Semigroup for List<a> where
    mappend x y =
      case x do
      | [] -> y
      | h :: t -> h :: mappend t y
  
    ,
    (++) x y = mappend x y
  end
  
  impl Monoid for List<a> where
    empty = []
  end

  impl Show for List<a> requires Show<a> where
    show x =
      case x do
      | [] -> "[]"
      | h :: t -> "[" ^ (show h) ^ ", " ^ (show t) ^ "]"
  end
  
  // Option
  
  impl Functor for Option where
    fmap f x =
      case x do
      | None -> None
      | Some v -> Some (f v)
  end
  
  impl Applicative for Option where
    pure x = Some x
    ,
    ap f x =
      case f do
      | None -> None
      | Some g -> fmap g x
  end
  
  impl Monad for Option where
    bind x f =
      case x do
      | None -> None
      | Some v -> f v
  end
  
  impl Alternative for Option where
    aempty = None
    ,
    (<|>) x y =
      case x do
      | Some v -> Some v
      | None -> y
  end
  
  impl Semigroup for Option<a> where
    mappend x y = 
      case x do
        | None -> y
        | Some v -> Some v
    ,
    (++) x y = mappend x y
  end
  
  impl Monoid for Option<a> where
    empty = None
  end
  
  let println x = print_string (show x)