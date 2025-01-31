x3d2: Next-Generation High-Fidelity CFD Solver
==============================================

x3d2 is an advanced open-source Computational Fluid Dynamics (CFD) solver, designed as the next-generation evolution of Xcompact3d. x3d2 is built to fully leverage GPU acceleration while maintaining the high-order finite-difference methods and spectral-like accuracy of Xcompact3d.

The solver is optimized for High-Performance Computing (HPC) environments and supports two parallelization backends:

OpenMP Backend – Optimized for multi-core CPU execution.
CUDA Backend – Designed for GPU acceleration on NVIDIA hardware.


🔥 Key Features

- High-Order Finite-Difference Methods: Compact finite-difference schemes delivering spectral-like accuracy on a monobloc Cartesian mesh.
- Dual Parallelization Backends:
    - OpenMP for multi-threaded execution on CPUs.
    - CUDA for GPU-accelerated computations.
- Scalable Parallel Computing: Efficient MPI-based parallelization for large-scale Direct Numerical Simulation (DNS) of turbulent flows.
- Enhanced Immersed Boundary Method (IBM): Improved treatment of complex geometries with moving boundary capabilities.
- Incompressible & Low-Mach Variable Density Flows: Solves the Navier-Stokes equations with fractional time-stepping and spectral Poisson solvers.
- Backward Compatibility: Designed to facilitate a transition from Xcompact3d while introducing modern performance enhancements.

🚀 Parallel Execution & Performance
x3d2 is built for high-performance computing, supporting:

- OpenMP Backend: Multi-threaded CPU execution.
- CUDA Backend: Full GPU acceleration with CUDA kernels.
- MPI + OpenMP or MPI + CUDA: Efficient parallelization across multiple nodes, enabling large-scale simulations on HPC clusters.

With optimized Fast Fourier Transforms (FFTs) and an enhanced 2D domain decomposition, the solver efficiently scales across thousands of CPU cores and multiple GPUs.

🛠 Getting Started
See <<LINK>> for installation instructions for Linux and macOS platforms.

📖 Documentation
Online hosted docs: Read The Docs

🏗 Roadmap
🔹 Optimized CUDA backend for large-scale GPU simulations.
🔹 Improved load balancing for heterogeneous architectures.
🔹 Enhanced parallel I/O for large-scale simulations.

👥 Contributing
We welcome contributions from the CFD community! Please check out the CONTRIBUTING.md file for guidelines on submitting patches, reporting issues, and joining discussions.

🏛 Acknowledgments
x3d2 builds upon Xcompact3d and benefits from contributions across multiple institutions. We acknowledge funding support and contributions from the Imperial College London Department of Aeronautics.

📜 License
<<LINK TO LICENSE>>