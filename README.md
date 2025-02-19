# x3d2: Next-Generation High-Fidelity CFD Solver

x3d2 is an open-source Computational Fluid Dynamics (CFD) solver, designed as the next-generation evolution of Xcompact3d.  It leverages GPU acceleration while maintaining the high-order finite-difference methods and spectral-like accuracy of its predecessor. x3d2 is optimised for High-Performance Computing (HPC) environments and supports both CPU and GPU parallelisation.

## Key Features
* **High-Order Finite-Difference Methods:** Compact finite-difference schemes delivering spectral-like accuracy on a monobloc Cartesian mesh, ensuring high-fidelity simulations.
* **Dual Parallelisation Backends:**
    * **OpenMP:** Optimised for multi-core CPU execution.
    * **CUDA:** Designed for GPU acceleration on NVIDIA hardware.
* **Scalable Parallel Computing:** Efficient MPI-based parallelisation for large-scale Direct Numerical Simulation (DNS) of turbulent flows, allowing for simulations on hundreds of thousands of cores and multiple GPUs.
* **Incompressible & Low-Mach Variable Density Flows:** Solves the Navier-Stokes equations with fractional time-stepping and spectral Poisson solvers.
* **Modern Software Engineering:** x3d2 is being actively developed with a focus on maintainability, extensibility, and code quality.

## Performance and Scalability

x3d2 is built for high-performance computing and is designed to scale efficiently on modern HPC architectures.  Its performance is enhanced by:

* **DistD2-TDS:** A novel algorithm for solving tridiagonal systems with improved data locality and minimal data movement. This algorithm is specifically optimised for both CPUs (using vectorisation) and GPUs (using thread-level parallelism) and significantly reduces inter-process communication.
* **2D Domain Decomposition for FFT:** x3d2 utilises 2D domain decomposition for Fast Fourier Transforms (FFTs), enabling greater scalability and allows for efficient parallelisation of the Poisson solver, especially for large simulations. x3d2 uses optimised FFT implementations (cuFFT for GPUs and 2DECOMP&FFT for CPUs) to accelerate the solution of the Poisson equation.

## Installation
See [Getting Started](https://x3d2.readthedocs.io/en/latest/getting_started.html) for installation instructions for Linux and macOS platforms.

## Documentation
Comprehensive user and developer documentation is available at [Read the Docs](https://x3d2.readthedocs.io/en/latest/index.html).

## Contributing
We welcome contributions from the CFD community! Please check out [Contributing to x3d2](https://x3d2.readthedocs.io/en/latest/developer/index.html) for guidelines on submitting Pull Requests and reporting issues.

## Acknowledgments
x3d2 builds upon Xcompact3d and benefits from contributions across multiple institutions. We acknowledge funding support and contributions from the Imperial College London Department of Aeronautics.

## Current Status and Roadmap
x3d2 is under active development. We are actively working on implementing new features and adding more canonical test cases.