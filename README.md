# Enigma Machine

A pure Haskell implementation of the legendary **Enigma cipher machine** used by Nazi Germany during World War II, along with a simulation of the **Bombe**, designed by Alan Turing to break Enigma-encrypted messages.

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/3/3e/EnigmaMachineLabeled.jpg/440px-EnigmaMachineLabeled.jpg" alt="Enigma Machine" width="300"/>
</p>

---

## ✨ Features

- **Full Enigma Simulation** — Accurate simulation of the 3-rotor Enigma machine with configurable rotors, reflector, and initial offsets
- **Steckerboard Support** — Implements the plugboard for additional encryption complexity
- **Rotor Mechanics** — Realistic rotor stepping with knock-on positions
- **Bombe Cryptanalysis** — Simulates the Bombe machine to break Enigma encryption using crib-based attacks
- **Menu Finding** — Implements the longest menu algorithm used in cryptanalysis

---

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler) 8.0+
- Or install via [GHCup](https://www.haskell.org/ghcup/)

### Running the Project

```bash
# Compile
ghc -o enigma Main.hs Enigma.hs

# Run
./enigma
```

Or run interactively with GHCi:

```bash
ghci Main.hs
```

---

## Usage

### Encoding Messages

```haskell
import Enigma

enigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 25)

encodeMessage "HELLO WORLD" enigma
-- Returns encrypted text (only A-Z are encrypted, spaces preserved)
```

### Using the Plugboard

```haskell
plugboard = [('F','T'), ('D','U'), ('V','A'), ('K','W'), ('H','Z'), ('I','X')]
steckeredEnigma = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 25) plugboard

encodeMessage "SECRET MESSAGE" steckeredEnigma
```

### Solving Enigma

```haskell
-- Given a crib
plaintext  = "WETTERVORHERSAGEBISKAYA"
ciphertext = encodeMessage plaintext enigma

-- Create the crib as pairs
crib = zip plaintext ciphertext

-- Find the longest menu
longestMenu crib

-- Attempt to break the encryption
breakEnigma crib
-- Returns: Maybe (Offsets, Stecker)
```

---

## The Bombe Algorithm

The Bombe simulation implements a crib-based attack:

1. **Menu Generation** — Finds chains of letter connections between plaintext and ciphertext
2. **Longest Menu** — Selects the longest chain to maximize constraint propagation
3. **Stecker Hypothesis** — Tests plugboard configurations against the menu
4. **Contradiction Detection** — Eliminates invalid configurations through logical contradictions

