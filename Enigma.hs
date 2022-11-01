{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int) 
  type Stecker = ((Char, Char)) -- the supplied type is not correct; fix it!
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  -- Sets the rotors depending on their initial offset and calls startEncoding
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  encodeMessage str (SimpleEnigma (rr,rn) (mr,mn) (lr,ln) r (ol, om, or)) = 
    startEncoding str (SimpleEnigma (setRotor or (rr,rn)) (setRotor om (mr,mn)) (setRotor ol (lr,ln)) r (ol, om, or))

  -- Iterates through all Chars in the string calling encodeChar for each
  -- If char is not in the range ['A'..'Z'], i.e. special cases, it ignores it
  startEncoding :: String -> Enigma -> String
  startEncoding [] = []
  --startEncoding str enigma = startEncoding
  startEncoding str enigma = [encodeChar (toUpper c) (checkOffset enigma) | c <- str, (toUpper c) `elem` ['A'..'Z']]
  
  -- Sets the initial rotors depending on the initial offset
  -- Recursive function so that it is repeated n times
  setRotor :: Int -> Rotor -> Rotor
  setRotor 0 rotor = rotor
  setRotor n rotor = setRotor (n-1) (rotateRotor rotor)


  -- Encodes a character by making it travel through each rotor, reflector and back
  encodeChar :: Char -> Enigma -> Char
  encodeChar c (SimpleEnigma rotor1 rotor2 rotor3 ref _) = tripleEncode False (reflect ref trip1) rotor3 rotor2 rotor1
    where trip1 = tripleEncode True c rotor1 rotor2 rotor3 

  encodeChar c (SteckeredEnigma rotor1 rotor2 rotor3 ref _ _) = tripleEncode False (reflect ref trip1) rotor3 rotor2 rotor1
    where trip1 = tripleEncode True c rotor1 rotor2 rotor3 

  -- Encodes a letter three times with the three different rotors
  -- Boolean input indicates if its going left to right or viceversa
  tripleEncode :: Bool -> Char -> Rotor -> Rotor -> Rotor -> Char
  tripleEncode True c rotor1 rotor2 rotor3 = rotorEq rotor3 (rotorEq rotor2 (rotorEq rotor1 c))
  tripleEncode False c rotor1 rotor2 rotor3 = alphabetEq rotor3 (alphabetEq rotor2 (alphabetEq rotor1 c))

  -- Function that adds one to the offsets and checks if it will affect other rotors
  -- If offset greater than 25, rotor offset is set to 0
  -- If offset matches the knock-on position then add one to the rotor on the left
  checkOffset :: Enigma -> Enigma
  checkOffset (SimpleEnigma (rr,rn) (mr,mn) (lr,ln) r (ol, om, or)) | rn+1 == or = SimpleEnigma (rotateRotor(rr,rn)) (rotateRotor(mr,mn)) (lr,ln) r (ol, om+1, or+1)
                                                                    | (rn+1 == or && mn+1 == om) = SimpleEnigma (rotateRotor (rr,rn)) (rotateRotor (mr,mn)) (rotateRotor(lr,ln)) r (ol+1, om+1, or+1)
                                                                    | otherwise = SimpleEnigma (rotateRotor(rr,rn)) (mr,mn) (lr,ln) r (ol, om, or+1)

  checkOffset (SteckeredEnigma (rr,rn) (mr,mn) (lr,ln) r (ol, om, or) s) | rn+1 == or = SteckeredEnigma (rotateRotor(rr,rn)) (rotateRotor(mr,mn)) (lr,ln) r (or+1, om+1, ol) s
                                                                         | (rn+1 == or && mn+1 == om) = SteckeredEnigma (rotateRotor (rr,rn)) (rotateRotor (mr,mn)) (rotateRotor(lr,ln)) r (or+1, om+1, ol+1) s
                                                                         | otherwise = SteckeredEnigma (rotateRotor(rr,rn)) (mr,mn) (lr,ln) r (or+1, om, ol) s

  -- Returns the character at position 'alphaPos char' in a rotor
  rotorEq :: (String, Int) -> Char -> Char
  rotorEq (st, _) c = st!!(alphaPos c)

  -- Returns the character at the position of a char in a rotor in the alphabet
  alphabetEq :: (String, Int) -> Char -> Char
  alphabetEq (st, _) c = ['A'..'Z']!!(rotorIndex)
    where rotorIndex = fromJust $ elemIndex c st

  -- Returns the reflected letter from the tuple list by iterating though all tuples finding one item that matches input
  reflect :: Reflector -> Char -> Char
  reflect ref c = head [if c == a then b else a | (a,b) <- ref, a == c || b == c]
  
  -- Takes the head of the string and adds it to the tail, also adds one to the offset
  rotateRotor :: Rotor -> Rotor
  rotateRotor (st, n) = (((tail st) ++ [(head st)]), n)

  -- Merges two lists into one
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
