{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int) 
  type Stecker = ((Char, Char)) -- the supplied type is not correct; fix it!
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  -- encodeMessage str enigma = merge [(encodeChar (toUpper (head str)) checkEnigma)] (encodeMessage (tail str) checkEnigma)
  --   where checkEnigma = checkOffset enigma

  encodeMessage str enigma = [if (toUpper c) `elem` ['A'..'Z'] 
                                then encodeChar (toUpper c) (checkOffset enigma) 
                              else 
                                c 
                              | c <- str]

  -- Encodes a character
  encodeChar :: Char -> Enigma -> Char
  encodeChar c (SimpleEnigma rotor1 rotor2 rotor3 ref _) = tripleEncode (reflectLetter ref trip1) rotor1 rotor2 rotor3
    where trip1 = tripleEncode c rotor1 rotor2 rotor3 

  encodeChar c (SteckeredEnigma rotor1 rotor2 rotor3 ref _ _) = tripleEncode (reflectLetter ref trip1) rotor3 rotor2 rotor1
    where trip1 = tripleEncode c rotor1 rotor2 rotor3 

  -- Encodes a letter three times with the three different rotors
  tripleEncode :: Char -> Rotor -> Rotor -> Rotor -> Char
  tripleEncode c rotor1 rotor2 rotor3 = rotorEq rotor3 (rotorEq rotor2 (rotorEq rotor1 c))

  checkOffset :: Enigma -> Enigma
  checkOffset (SimpleEnigma (rr,rn) (mr,mn) (lr,ln) r (or, om, ol)) | ln+1 == or = if SimpleEnigma (rotateRotor(rr,rn)) (rotateRotor(mr,mn)) (lr,ln) r (or, om+1, ol+1)
                                                                    | (ln+1 == or && mn+1 == om) = SimpleEnigma (rotateRotor (rr,rn)) (rotateRotor (mr,mn)) (rotateRotor(lr,ln)) r (or+1, om+1, ol+1)
                                                                    | otherwise = SimpleEnigma (rotateRotor(rr,rn)) (mr,mn) (lr,ln) r (or, om, ol+1)

  checkOffset (SteckeredEnigma (rr,rn) (mr,mn) (lr,ln) r (or, om, ol) s) | rn+1 == or = SteckeredEnigma (rotateRotor(rr,rn)) (rotateRotor(mr,mn)) (lr,ln) r (or+1, om+1, ol) s
                                                                         | (rn+1 == or && mn+1 == om) = SteckeredEnigma (rotateRotor (rr,rn)) (rotateRotor (mr,mn)) (rotateRotor(lr,ln)) r (or+1, om+1, ol+1) s
                                                                         | otherwise = SteckeredEnigma (rotateRotor(rr,rn)) (mr,mn) (lr,ln) r (or+1, om, ol) s

  -- Returns the character at position 'alphaPos c'
  rotorEq :: (String, Int) -> Char -> Char
  rotorEq (st, _) c = st!!(alphaPos c)

  -- Returns the reflected letter from the tuple list by iterating though all tuples finding one item that matches input
  reflectLetter :: Reflector -> Char -> Char
  reflectLetter ref c = head [if c == a then b else a | (a,b) <- ref, a == c || b == c]

  swapLetter :: (Char, Char) -> [Char] -> String
  swapLetter (a,b) = concatMap (\c -> if c == a then [b] else [c])
  
  -- Takes the head of the string and adds it to the tail, also adds one to the offset
  rotateRotor :: Rotor -> Rotor
  rotateRotor (st, n) = (((tail st) ++ [(head st)]), n)

  merge :: [a] -> [a] -> [a]
  merge xs     []     = xs
  merge []     ys     = ys
  merge (x:xs) (y:ys) = x : y : merge xs ys

{- Part 2: Finding the Longest Menu -}

  type Menu = Bool -- the supplied type is not correct; fix it!
  type Crib = Bool -- the supplied type is not correct; fix it!

  longestMenu :: Crib -> Menu
  longestMenu _ = False

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'
