# `cmvm`: Hardware-Optimized Approximate Matrix-Vector Multiplication

[![Actions Status](https://github.com/aproposorg/cmvm/actions/workflows/ci.yml/badge.svg)](https://github.com/aproposorg/cmvm/actions)

This is an attempt at reproducing the functionality of the _computation coding_ tool proposed by Lehnert et al. in the following reference using [Chisel](https://github.com/chipsalliance/chisel3) as hardware generation backend.

```bibtex
@article{lehnert2023most,
  title={Most Resource Efficient Matrix Vector Multiplication on FPGAs},
  author={Lehnert, Alexander and Holzinger, Philipp and Pfenning, Simon and M{\"u}ller, Ralf and Reichenbach, Marc},
  journal={IEEE Access},
  volume={11},
  pages={3881--3898},
  year={2023},
  publisher={IEEE}
}
```

The tool uses the following terminology:

- _Decomposition_ referring to the collection of slices that make up a decomposed matrix in computation coding. (Internally, these are of type `Array[Array[Array[Array[Double]]]]`)
- _Slice_ referring to a collection of matrix factors that make up a decomposed, vertical matrix slice. (Internally, type `Array[Array[Array[Double]]]`)
- _Matrix factor_ referring to the sub-elements of a slice. (Internally, type `Array[Array[Double]]`)
- _Factor_ (or _matrix factor element_) referring to the individual elements of a matrix factor. (Internally, type `Array[Array[Double]]`)

Beware that factors are assumed to be represented by a limited number of bits. Unless the factors are passed to the tool manually, it assumes a slightly asymmetric distribution around zero. Moreover, the current algorithm for picking factors for slice rows suffers from exponential runtime in the number of non-zero elements. This means the overall tool runs rather slowly, even for relatively small matrices. We provide automated test flows for matrix sizes up to $256\times256$, which can execute in a few minutes on a laptop.

***
# Requirements

Utilizing Chisel and ChiselTest, `cmvm` requires a suitable installation of Scala. For this purpose, we use the Scala Build Tool (`sbt`) for which we provide a suitable build script. 

A suitable installation of Verilator is also needed to run all the hardware tests in the library. Note that only [specific versions of Verilator](https://github.com/ucb-bar/chiseltest#verilator-versions) are officially supported.

This library is tested in Ubuntu 20.04 with Verilator 4.028.

***
# Using the Library

## Decomposing Matrices

Our tool makes decomposing matrices in computation coding very easy. It offers a single API entry point defined as:

```scala
def decompose(target: Array[Array[Double]], factors: Array[Double] = Array.empty[Double], p: Int = 2, e: Int = 2, numBits: Int = 6, minSqnr: Int = 45): Array[Array[Array[Array[Double]]]]
```

Many of the arguments to this function have default values that resemble those used in the reference shown above. The arguments are:

- `target` the matrix to decompose.
- `factors` the array of factors to use in the decomposition. If left empty, the tool uses the aforementioned, slightly asymmetric distribution around zero.
- `p` the number of non-trivial matrix factors in each slice.
- `e` the maximum number of non-zero elements in a matrix factor row.
- `numBits` the number of bits to use in the factor representations.
- `minSqnr` the minimum signal-to-quantization-noise ratio (SQNR) in dB for which to terminate the decomposition early.

Note that the tool does not currently support manually specifying the slice width to use. To limit computations a little, we manually pick the minimum of $8$ and the nearest, greater power of two than $2\cdot E$.

## Generating Hardware

It is equally easy to generate the hardware descriptions of matrices. The tool offers two API entry points: one for manually decomposed matrices and one that does the decomposition before hardware generation. These are:

```scala
def generateConstant(target: Array[Array[Double]], dataW: Int, pipe: Boolean = false, factors: Array[Double] = Array.empty[Double], p: Int = 2, e: Int = 2, numBits: Int = 6, minSqnr: Int = 45): String

def generateConstant(dec: Array[Array[Array[Array[Double]]]], dataW: Int, pipe: Boolean, numBits: Int): String
```

In addition to the arguments listed above, these functions take the following hardware-related ones:

- `dataW` the width of the data path in the circuit (i.e., the input vector element widths, not the factors). Despite obvious drawbacks, we assume this width stays constant regardless of the circuit depth.
- `pipe` whether to pipeline the data path after each matrix factor.
