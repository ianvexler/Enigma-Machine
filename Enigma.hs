{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List
  import Data.Ord
  import Debug.Trace

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int) 
  type Stecker = [(Char, Char)]
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  -- Sets the rotors depending on their initial offset and calls startEncoding
  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  encodeMessage str (SimpleEnigma (rr,rn) (mr,mn) (lr,ln) r (ol, om, or)) = 
    startEncoding str (SimpleEnigma (setRotor or (rr,rn)) (setRotor om (mr,mn)) (setRotor ol (lr,ln)) r (ol, om, or))
  
  encodeMessage str (SteckeredEnigma (rr,rn) (mr,mn) (lr,ln) r (ol, om, or) pb) = 
    startEncoding str (SteckeredEnigma (setRotor or (rr,rn)) (setRotor om (mr,mn)) (setRotor ol (lr,ln)) r (ol, om, or) pb)

  -- Iterates through all Chars in the string calling encodeChar for each
  -- If char is not in the range ['A'..'Z'], i.e. special cases, it ignores it
  startEncoding :: String -> Enigma -> String
  startEncoding [] _ = []
  startEncoding str (SimpleEnigma r1 r2 r3 ref off) = 
    if (toUpper (head str)) `elem` ['A'..'Z']
      then merge ([encodeChar (toUpper (head str)) checkEnigma]) (startEncoding (tail str) checkEnigma) 
    else
      merge ([head str]) (startEncoding (tail str) simpleEnigma)
    where simpleEnigma = SimpleEnigma r1 r2 r3 ref off
          checkEnigma = checkOffset simpleEnigma

  --missing stecket on second round
  startEncoding str (SteckeredEnigma r1 r2 r3 ref off ste) = 
    if (toUpper (head str)) `elem` ['A'..'Z']
      then merge (encodeSteckered) (startEncoding (tail str) checkEnigma) 
    else
      merge ([head str]) (startEncoding (tail str) steckeredEnigma)
    where steckeredEnigma = SteckeredEnigma r1 r2 r3 ref off ste
          checkEnigma = checkOffset steckeredEnigma
          encodeSteckered = [steckerChar ste (encodeChar (steckerChar ste (toUpper (head str))) checkEnigma)]

  -- Sets the initial rotors depending on the initial offset
  -- Recursive function so that it is repeated n times
  setRotor :: Int -> Rotor -> Rotor
  setRotor 0 rotor = rotor
  setRotor n rotor = setRotor (n-1) (rotateRotor rotor)

  -- Encodes a character by making it travel through each rotor, reflector and back
  encodeChar :: Char -> Enigma -> Char
  encodeChar c (SimpleEnigma r1 r2 r3 ref _) = tripleEncode False (reflect ref trip1) r3 r2 r1
    where trip1 = tripleEncode True c r1 r2 r3 

  encodeChar c (SteckeredEnigma r1 r2 r3 ref _ _) = tripleEncode False (reflect ref trip1) r3 r2 r1
    where trip1 = tripleEncode True c r1 r2 r3 

  -- Encodes a letter three times with the three different rotors
  -- Boolean input indicates if its going left to right or viceversa
  tripleEncode :: Bool -> Char -> Rotor -> Rotor -> Rotor -> Char
  tripleEncode True c r1 r2 r3 = rotorEq r3 (rotorEq r2 (rotorEq r1 c))
  tripleEncode False c r1 r2 r3 = alphabetEq r3 (alphabetEq r2 (alphabetEq r1 c))

  -- Function that adds one to the offsets and checks if it will affect other rotors
  -- If offset greater than 25, rotor offset is set to 0
  -- If offset matches the knock-on position then add one to the rotor on the left
  checkOffset :: Enigma -> Enigma
  checkOffset (SimpleEnigma (rr,rn) (mr,mn) (lr,ln) r (ol, om, or)) 
    -- Checks if the knock-on position of the rotors are equal to the offset
    | (rn == or+1 && mn == om+1) 
      = SimpleEnigma (rotateRight) (rotateMid) (rotateLeft) r (ofLeft, ofMid, ofRight)
    | rn == or+1 
      = SimpleEnigma (rotateRight) (rotateMid) (lr,ln) r (ol, ofMid, ofRight)
    | otherwise 
      = SimpleEnigma (rotateRight) (mr,mn) (lr,ln) r (ol, om, ofRight)
        where rotateRight = rotateRotor (rr,rn)
              rotateMid = rotateRotor (mr,mn)
              rotateLeft = rotateRotor (lr,ln)
              ofRight = offsetOverflow (or+1)
              ofMid = offsetOverflow (om+1)
              ofLeft = offsetOverflow (ol+1)
  
  checkOffset (SteckeredEnigma (rr,rn) (mr,mn) (lr,ln) r (ol, om, or) s) 
    -- Checks if the knock-on position of the rotors are equal to the offset
    | (rn == or+1 && mn == om+1) 
      = SteckeredEnigma (rotateRight) (rotateMid) (rotateLeft) r (ofLeft, ofMid, ofRight) s
    | rn == or+1 
      = SteckeredEnigma (rotateRight) (rotateMid) (lr,ln) r (ofLeft, ofMid, ofRight) s
    | otherwise 
      = SteckeredEnigma (rotateRight) (mr,mn) (lr,ln) r (ol, om, ofRight) s
        where rotateRight = rotateRotor (rr,rn)
              rotateMid = rotateRotor (mr,mn)
              rotateLeft = rotateRotor (lr,ln)
              ofRight = offsetOverflow (or+1)
              ofMid = offsetOverflow (om+1)
              ofLeft = offsetOverflow (ol+1)

  -- Short for offset Overflow
  -- Checks if offset is greater than 25, if so returns 0
  offsetOverflow :: Int -> Int
  offsetOverflow n = if n >= 26 then 0 else n

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

  -- Similar to reflect but with the steckerboard
  steckerChar :: Stecker -> Char -> Char
  steckerChar ste c = if length reflect == 0 then c else head reflect
    where reflect = [if c == a then b else a | (a,b) <- ste, a == c || b == c]
  
  -- Takes the head of the string and adds it to the tail, also adds one to the offset
  rotateRotor :: Rotor -> Rotor
  rotateRotor (st, n) = (((tail st) ++ [(head st)]), n)

{- Part 2: Finding the Longest Menu -}

  type Menu = [Int]
  type Crib = [(Char,Char)] -- (plain, cipher)

  -- Queue contains all chains that have to be explored
  -- Explored contains all chains that have been fully explored
  type Queue = [Menu]
  type Explored = [Menu]

  -- Loop through every starting position (letter) at crib
  -- Generates all possible menus for each starting letter and concatenates all
  -- Sorts smallest to largest menu and takes the last element
  longestMenu :: Crib -> Menu
  longestMenu [] = []
  longestMenu crib = last $ sortBy (comparing $ length) (concat (map snd iterateCrib))
    {- Calls generateMenus for all tuples at crib with its index
       and an empty list as nothing has been explored -}
    where iterateCrib = [generateMenus ([[fromJust $ elemIndex (a,b) crib]], []) crib | (a,b) <- crib]

  -- Goes through all menus in the queue until they have been fully explored
  {- Once the queue is empty, it returns a tuple containing the empty
     queue and the fully explored menus -}
  generateMenus :: (Queue, Explored) -> Crib -> (Queue, Explored)
  generateMenus ([], es) _ = ([], es) 
  generateMenus (ms, es) crib = generateMenus ((updateQueue ms (fst assign)), snd assign) crib
    -- possibleMenus = Finds the next menus for all menus as queue
    -- assign = Assigns them to either Queue or Explored
    where possibleMenus = concat $ [findMenu (cipherEq (last m) crib) m crib | m <- ms]
          assign = assignMenus possibleMenus (ms, es)

  -- Iterates through a list of menus assigning them to either queue or explored
  assignMenus :: [(Menu, Bool)] -> (Queue, Explored) -> (Queue, Explored)
  assignMenus [] qs = qs
  assignMenus (m:ms) (qs) = assignMenus ms (checkMenu m qs)

  -- Checks if the menu should go to Queue or Explored
  -- Adds the menu to its corresponding list
  checkMenu :: (Menu, Bool) -> (Queue, Explored) -> (Queue, Explored)
  checkMenu (menu, b) (ms, es) | b == True = (ms, es ++ [menu])
                               | otherwise = (ms ++ [menu], es)

  {- Updates the queue by removing the menus that 
  have already being explored -}
  updateQueue :: [Menu] -> Queue -> Queue
  updateQueue [] q = q
  updateQueue (m:ms) q = updateQueue ms (removeItem m q)

  -- Checks if menu has been fully explored, if not it returns the menus possible
  -- True if fully Explored, False if it goes to Queue
  findMenu :: Char -> [Int] -> Crib -> [(Menu, Bool)]
  findMenu c xs crib | fromCipher == Nothing = [(xs, True)]
                     | otherwise = [(xs ++ [x], False) | x <- fromJust $ fromCipher, not (x `elem` xs)]
                      where fromCipher = findsChain c crib

  {- Checks if there is another chain to add to the menu and returns 
    the new menus, if not returns Nothing -}
  findsChain :: Char -> Crib -> Maybe [Int]
  findsChain c crib | length filterCrib > 0 = Just [fromJust $ elemIndex (a,b) crib | (a,b) <- filterCrib]
                    | otherwise = Nothing
    where filterCrib = filter ((== c) . fst) crib
  
  -- Returns the Char at position n in the crib
  cipherEq :: Int -> Crib -> Char
  cipherEq n crib = snd $ crib!!n 
  
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

  -- Merges two lists into one
  merge :: [a] -> [a] -> [a]
  merge xs [] = xs
  merge [] ys = ys
  merge (x:xs) (y:ys) = x : y : merge xs ys

  -- Removes items from list
  removeItem :: Eq a => a -> [a] -> [a]
  removeItem _ [] = []
  removeItem x (y:ys) | x == y = removeItem x ys
                      | otherwise = y : removeItem x ys
