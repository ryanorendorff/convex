# Using Dependent Types in Mathematical Optimization Algorithms

## How to build

This repo is using cabal and nix. Once you run the nix shell expression you
should be set up to compile the content of this repo.

```
nix-shell
cabal run
```

If you want to run the jupyter notebooks, you can do so as follows.

```
cd jupyter
nix-shell jupyter.nix
jupyter-notebook
```


## TODOs

- Making sure the examples in the repo and jupyter notebooks run.
- Adding in new solvers, such as a second order cone solver.
- Adding in capabilities to generate the matrices for certain problems with known structure, such as Model Predictive Control (MPC).
- Create a framework to optimize the linear algebra when certain properties of a matrix are know (idempotency, hermitian, etc).
- Perform some rewrites/fusion for the matrix free forms from a given expression
- Matrix free SVD calculation.


## Abstract from LambdaConf 2019 talk

How does a drone remain stable in a chaotic flying environment? By convex
optimization, of course! This talk will delve into how to solve real world
problems via convex optimization. This technique pairs strong mathematical
guarantees with implementation correctness using dependent type theory.
Mathematical optimization is a subfield of mathematics that focuses on selecting
the best element from a set of elements, often in the form of finding the
element that minimizes the value of a chosen function. The algorithms developed
in this field of mathematics are used to train machine learning algorithms,
develop self driving cars, build safe buildings with minimal materials, and
improve battery efficiency, to name only a few applications. Much of the
mechanical operation of these algorithms is performed by repeated calls to
linear algebra packages which, while highly tuned over many decades, provide no
form of compile time guarantee that the given algorithm is correct.

In this talk, we will discuss writing numerical optimization algorithms using
dependent types as a form of compile time check on the correctness of the
algorithm. Dependent types allow the programmer to assert that the dimensions
used in the linear algebra are correct at compile time. We will discuss the
advantages of the dependently typed algorithms, as well as additional methods in
which to ensure they are correct.

In addition, we will provide real world example of using dependently typed
numerical optimizations for applications such as scheduling and controls. In
particular, we will discuss a novel method of reconstructing medical images from
raw data using numerical optimization. Through this example, we will explore
both dependently typed linear algebra as well as functional approaches to linear
algebra through matrix-free methods.

Numerical optimization is the algorithmic backbone behind many modern numerical
computing applications, such as machine learning, fluid dynamics, physics
simulations, and many more. Attendees will leave with an understanding of the
basics of the mathematics behind these algorithms, as well as an understanding
of how functional programming and type theory can assist with proving an
algorithm’s correctness.
