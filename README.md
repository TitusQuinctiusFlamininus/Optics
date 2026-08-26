# Fun With All Things Optics

> An experimental Haskell project exploring optics, profunctors, and related computational structures by implementing them from first principles.

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/profunctor.png "Profunctor")

This repository is a from-scratch exploration of **optics in functional programming**, with a particular focus on the profunctor formulation of lenses, prisms, isomorphisms, adapters, affines, and related structures.

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/optic.png "Optic")

Rather than treating optics as a finished abstraction to be imported and used, the project asks a more fundamental question:

**What does it take to build these structures ourselves, and how far can the underlying abstractions be pushed?**

The code is therefore both an implementation project and a laboratory for understanding the algebraic structure behind modern Haskell optics.

## What this project explores

The repository currently contains implementations and experiments around several families of optics and profunctors, including:

- **Lenses**
- **Prisms**
- **Isomorphisms**
- **Adapters**
- **Affines**
- **Forget**
- **Tagged**
- **Upstar / Downstar**
- **Multistar**
- **Traversal-related structures**
- **Cartesian / Strong variants**
- **CoCartesian / Choice variants**
- **Monoidal variants**
- **Closed, Traversing, Phantom, Cartographic and Sieve-related variants**
- **Profunctor optics**
- Experimental `TFunctor` and `RFunctor` structures

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/tfunctor.png "TFunctor")

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/rfunctor.png "RFunctor")

The status table below is intentionally kept as the project's working map of what has been implemented, what is known not to work, and what remains unexplored.

## Status legend

The repository uses three markers:

- 🟢 **Implemented / can be formed**
- 🟠 **Currently cannot be formed / does not work yet**
- ⚪ **Not investigated or implemented yet**

This is a development status rather than a claim about the theoretical possibility of a construction. In particular, an 🟠 entry may represent an interesting technical or type-theoretic obstacle rather than a permanent limitation.

## Why build optics from scratch?

Libraries such as [`lens`](https://hackage.haskell.org/package/lens) and [`optics`](https://hackage.haskell.org/package/optics) provide mature, practical implementations of optic abstractions.

This project takes a different route.

Reimplementing the machinery makes the relationships between the abstractions visible: how an optic can be represented by a profunctor, how typeclass constraints control what an optic can do, and how different profunctor structures correspond to different optic capabilities.

The goal is therefore not simply to produce another optics library. It is to use implementation as a way of investigating the structure itself.

## Repository structure

The repository is organised around a conventional Haskell package layout:

```text
.
├── app/              # Executable entry point
├── images/           # Project images / diagrams
├── src/              # Optics and supporting implementations
├── test/             # Test suite
├── CHANGELOG.md      # Project change history
├── LICENSE            # BSD-3-Clause license
├── optics-fun.cabal  # Cabal package description
├── package.yaml      # Hpack package configuration
├── stack.yaml        # Stack configuration
└── README.md
```

The package currently exposes a fairly broad collection of modules corresponding to individual optic variants. The Cabal configuration also shows dependencies on `base`, `comonad`, `lens`, `MissingH`, and `profunctors`.

## Building the project

This is a Haskell project using **Stack** and **Hpack/Cabal** configuration.

A typical checkout/build workflow is:

```bash
git clone https://github.com/TitusQuinctiusFlamininus/Optics.git
cd Optics

stack build
```

To run the executable:

```bash
stack exec optics-fun-exe
```

To run the test suite:

```bash
stack test
```

If you prefer working directly with Cabal, the generated `optics-fun.cabal` file contains the package, library, executable, and test-suite definitions.

> **Note:** The repository is experimental and under active development. Build behaviour may change as the implementations and package configuration evolve.

## A conceptual roadmap

A useful way to read the project is to move through the layers rather than trying to understand every module simultaneously.

### 1. Ordinary optics

Start with the basic optic families:

- `Lens`
- `Prism`
- `Iso`
- `Adapter`
- `Affine`

These provide the vocabulary for the rest of the repository.

### 2. Profunctor structure

The next layer investigates which additional capabilities can be expressed by changing the profunctor used to interpret an optic.

This leads naturally to structures such as:

- Strong / Cartesian
- Choice / CoCartesian
- Monoidal
- Closed
- Traversing
- Phantom
- Cartographic
- Sieve

The status matrix in this README records the current state of those experiments.

### 3. Profunctor optics

The **Profunctor-Optics** section explores the corresponding constructions in a more explicitly profunctor-oriented formulation.

This is where the project becomes particularly useful as a study of the relationship between optic types and profunctor capabilities.

### 4. Experimental functors

The final section contains two experimental abstractions:

- **`TFunctor`**, described here as a *post-processing profunctor*
- **`RFunctor`**, described here as a *pre-processing profunctor*

These are explicitly experimental ideas rather than established parts of the standard optics vocabulary.

The purpose is exploratory: determine what structures can be derived from these ideas, what laws they satisfy, and where the type system pushes back.

## Relationship to existing optics libraries

This project is best viewed alongside existing Haskell optics work rather than as a replacement for it.

Useful reference points include:

- [`lens`](https://hackage.haskell.org/package/lens) — a mature and widely used Haskell optics library.
- [`optics`](https://hackage.haskell.org/package/optics) — a modern optics ecosystem with an abstract interface for lenses, traversals, prisms and related optics.
- [`profunctors`](https://hackage.haskell.org/package/profunctors) — foundational abstractions used by the profunctor representation of optics.
- [`comonad`](https://hackage.haskell.org/package/comonad) — relevant to several of the constructions explored in the project.
- [Optics in the Haskell Wiki](https://wiki.haskell.org/Optics) — useful background and terminology.
- [Haskell `lens` documentation](https://hackage.haskell.org/package/lens/docs/Control-Lens.html) — extensive practical and theoretical reference material.
- [Optics package documentation](https://hackage.haskell.org/package/optics) — documentation for the modern `optics` approach.

For a deeper theoretical treatment, the following topics are especially relevant:

- profunctor optics
- lenses and prisms
- profunctor encodings
- Cartesian/Strong profunctors
- Choice/CoCartesian profunctors
- monoidal profunctors
- traversals and traversing profunctors
- representable and co-representable structures
- Kan extensions and Kan lifts

## Development philosophy

This repository deliberately keeps the boundary between **known theory**, **working implementation**, and **experimentation** visible.

That means incomplete entries are useful information. They identify places where the implementation has encountered a problem, where a construction has not yet been derived, or where the theoretical relationship is still being investigated.

In that sense, the status matrix is part of the documentation: it is a map of the research process as much as a feature list.

## Current scope

The project currently concentrates on the type-level and algebraic machinery behind optics rather than presenting a polished end-user API.

In particular, expect:

- experimental interfaces
- partially implemented optic variants
- exploratory typeclass designs
- implementations that may change substantially
- unfinished constructions
- tests that document behaviour and help drive development

This makes the repository particularly suited to **learning, experimentation, and investigating the foundations of optics in Haskell**.

## Contributing / extending the project

If you want to extend the project, a useful approach is to pick one of the ⚪ or 🟠 entries in the status matrix and investigate:

1. What profunctor capability the construction requires.
2. Whether an existing Haskell abstraction already captures that capability.
3. What typeclass constraints are necessary.
4. Which optic laws should hold.
5. Whether the construction composes correctly with the existing implementations.
6. Whether the behaviour can be captured with a focused test.

For experimental structures such as `TFunctor` and `RFunctor`, it is also worth documenting the intended laws before expanding the implementation. That can prevent the typeclass hierarchy from becoming a very clever pile of furniture with no floor plan.

## References and further reading

A few particularly useful starting points:

- [Haskell `lens` package](https://hackage.haskell.org/package/lens)
- [Haskell `optics` package](https://hackage.haskell.org/package/optics)
- [Haskell `profunctors` package](https://hackage.haskell.org/package/profunctors)
- [Haskell `comonad` package](https://hackage.haskell.org/package/comonad)
- [Haskell Wiki: Optics](https://wiki.haskell.org/Optics)
- [Wikipedia: Lens (programming)](https://en.wikipedia.org/wiki/Lens_(programming))
- [Wikipedia: Profunctor](https://en.wikipedia.org/wiki/Profunctor)

The repository itself is the authoritative source for the current implementation status.

## License

This project is released under the **BSD 3-Clause License**. See [`LICENSE`](LICENSE) for the full license text.

---

# Implementation Status

## Profunctor

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/profunctor.png "Profunctor")

🟢 means profunctors can be formed  
🟠 means profunctors can't be formed  
⚪ means I haven't gotten to it yet

### Vanilla Profunctors

🟢 Lens  
🟢 Prism  
🟢 Adapter  
🟢 Forget  
🟢 Tagged  
🟢 Upstar  
🟢 Iso  
🟢 Downstar  
🟢 Affine  
🟠 Traversal

### Lens Variants

🟢 Cartesian (Strong) Lens  
🟠 CoCartesian (Choice) Lens  
🟢 Monoidal Lens  
🟠 Traversing Lens  
🟠 InPhantom Lens  
⚪ OutPhantom Lens  
🟢 Cartesian (Strong) Cartographic Lens  
🟠 CoCartesian (Choice) Cartographic Lens  
🟠 Closed Lens  
⚪ Sieve Lens

### Prism Variants

🟢 Cartesian (Strong) Prism  
🟠 CoCartesian (Choice) Prism  
⚪ Monoidal Prism  
🟠 InPhantom Prism  
⚪ OutPhantom Prism  
⚪ Traversing Prism  
⚪ Closed Prism  
⚪ Cartographic Prism  
⚪ Sieve Prism

### Affine Variants

⚪ Cartesian (Strong) Affine  
⚪ CoCartesian (Choice) Affine  
⚪ Monoidal Affine  
⚪ InPhantom Affine  
⚪ OutPhantom Affine  
⚪ Traversing Affine  
⚪ Closed Affine  
⚪ Cartographic Affine  
⚪ Sieve Affine

### Iso Variants

🟢 Cartesian (Strong) Iso  
🟢 CoCartesian (Choice) Iso  
⚪ Monoidal Iso  
⚪ Traversing Iso  
⚪ InPhantom Iso  
⚪ OutPhantom Iso  
⚪ Closed Iso  
⚪ Cartographic Iso  
⚪ Sieve Iso

### Upstar Variants

🟢 Cartesian (Strong) Upstar  
🟢 CoCartesian (Choice) Upstar  
🟢 Monoidal Upstar  
🟠 Closed Upstar  
🟠 InPhantom Upstar  
⚪ OutPhantom Upstar  
⚪ Cartographic Upstar  
⚪ Sieve Upstar

### Downstar Variants

🟠 Cartesian (Strong) Downstar  
🟠 CoCartesian (Choice) Downstar  
🟠 Monoidal Downstar  
🟠 InPhantom Downstar  
🟠 Closed Downstar  
⚪ OutPhantom Downstar  
⚪ Cartographic Downstar  
⚪ Sieve Downstar

### Multistar Variants

🟢 Vanilla  
🟢 Strong Sieve

### Tagged Variants

🟠 Tagged Cartesian (Strong)  
🟢 Tagged CoCartesian (Choice)  
⚪ Tagged Monoidal  
⚪ Tagged InPhantom  
⚪ Tagged Closed  
⚪ Tagged Traversing  
⚪ Tagged OutPhantom  
⚪ Tagged Cartographic  
⚪ Tagged Sieve

### Other Modifications

⚪ Traversing  
⚪ Right Kan Lift

## Profunctor-Optics

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/optic.png "Optic")

🟢 Lens  
🟢 Cartesian (Strong) Lens  
🟠 CoCartesian (Choice) Lens  
🟢 Monoidal Lens  
🟠 Traversing Lens  
🟠 InPhantom Lens  
⚪ OutPhantom Lens  
⚪ Cartographic Lens  
🟠 Closed Lens  
🟢 Prism  
🟢 Cartesian (Strong) Prism Lens  
🟢 Cartesian (Strong) Cartographic Lens  
🟠 CoCartesian (Choice) Cartographic Lens  
🟠 CoCartesian (Choice) Prism  
🟠 InPhantom Prism  
🟢 Iso  
🟢 Cartesian (Strong) Iso  
🟢 CoCartesian (Choice) Iso  
🟢 Adapter  
🟢 Affine  
⚪ Traversal  
⚪ Procompose

# Experimental Types

## TFunctor (Post Processing Profunctor)

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/tfunctor.png "TFunctor")
(This is my own invention, let's see what works here)

🟢 Lens  
⚪ Prism  
⚪ Upstar  
⚪ Iso  
⚪ Cartesian (Strong) Upstar  
⚪ CoCartesian (Choice) Upstar  
⚪ Monoidal Upstar  
⚪ Downstar  
⚪ Adapter  
⚪ Forget  
⚪ Tagged  
⚪ Arrow  
⚪ Affine  
⚪ Bazaar  
⚪ Traversal  
⚪ Monoidal  
⚪ Traversing  
⚪ Closed  
⚪ Right Kan Lift

## RFunctor (Pre Processing Profunctor)

![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/rfunctor.png "RFunctor")
(This is also my own invention, let's see what works here)

⚪ Lens  
⚪ Prism  
⚪ Upstar  
⚪ Iso  
⚪ Cartesian (Strong) Upstar  
⚪ CoCartesian (Choice) Upstar  
⚪ Monoidal Upstar  
⚪ Downstar  
⚪ Adapter  
⚪ Forget  
⚪ Tagged  
⚪ Arrow  
⚪ Affine  
⚪ Bazaar  
⚪ Traversal  
⚪ Monoidal  
⚪ Traversing  
⚪ Closed  
⚪ Right Kan Lift
