# Fun With All Things Optics

Attempting to create Optics and other Computational structures from scratch

## Profunctors (dimap :: (a -> b) -> (c -> d) -> p b c -> p a d)
- :green_circle: Upstar
- :green_circle: Downstar
- :green_circle: Adapter
- :white_circle: Forget
- :white_circle: Tagged
- :white_circle: Affine
- :white_circle: Cartesian   (or Strong)
- :white_circle: CoCartesian (or Choice)
- :green_circle: Lens
- :green_circle: Prism
- :white_circle: Traversal
- :white_circle: Closed

## Profunctor-Optics (type Optic p a b s t = (p a b -> p s t))
- :white_circle: Cartesian   (or Strong)
- :white_circle: CoCartesian (or Choice)
- :green_circle: Adapter
- :green_circle: Lens
- :green_circle: Prism
- :white_circle: Affine
- :white_circle: Traversal

# Experimental Types

## TFunctor
(This is my own invention, let's see what works here)
![alt text](https://github.com/TitusQuinctiusFlamininus/Optics/blob/main/images/tfunctor.png "TFunctor")

- :white_circle: Upstar
- :white_circle: Downstar
- :white_circle: Adapter
- :white_circle: Forget
- :white_circle: Tagged
- :white_circle: Affine
- :white_circle: Cartesian   (or Strong)
- :white_circle: CoCartesian (or Choice)
- :white_circle: Lens
- :white_circle: Prism
- :white_circle: Traversal
- :white_circle: Closed


## Others
- :green_circle: Multistar / Spike (Upstar-Downstar)
- :white_circle: Multistar / Ditch (Downstar-Upstar)