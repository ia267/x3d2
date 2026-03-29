Getting Started
===============

Installing on Linux
-------------------

Dependencies
~~~~~~~~~~~~

To build x3d2 on Linux, you will need the following dependencies:

Fortran Compiler
^^^^^^^^^^^^^^^^

A recent modern Fortran compiler is required to build x3d2. For example GNU Fortran compiler (``gfortran``) version 9 and above. You can install it using your package manager. For Ubuntu this can be obtained using:

.. code-block:: bash
   
   sudo apt install gfortran

OpenMP is typically included with most modern compilers, including GCC. Therefore, installing ``gfortran`` for example should also provide OpenMP support.

MPI
^^^

You can install Open MPI using your package manager. For Ubuntu, this can be obtained using

.. code-block:: bash

   sudo apt install openmpi-bin libopenmpi-dev

CMake
^^^^^

To build x3d2, you will need CMake version 3.20 and above. You can download the latest version from the `CMake website <https://cmake.org/download/>`_. Alternatively, you can install it using your package manager. For Ubuntu, this can be obtained using:

.. code-block:: bash

   sudo apt install cmake

NVIDIA HPC SDK
^^^^^^^^^^^^^^

If you want to install x3d2 for NVIDIA GPUs, please download and install the NVIDIA HPC SDK for your target platform from the `NVIDIA website <https://developer.nvidia.com/hpc-sdk-downloads>`_. To ensure that ``mpirun`` and ``mpif90`` pick up the version shipped with NVIDIA HPC SDK (instead of another installation) you will need to set up the environment variables (consider adding them to your ``~/.bashrc`` file if you want them to persist). For example:

.. code-block:: bash

   export PATH=/opt/nvidia/hpc_sdk/Linux_x86_64/24.11/comm_libs/mpi/bin:/opt/nvidia/hpc_sdk/Linux_x86_64/24.11/compilers/bin:$PATH

   export LD_LIBRARY_PATH=/opt/nvidia/hpc_sdk/Linux_x86_64/24.11/comm_libs/mpi/lib:/opt/nvidia/hpc_sdk/Linux_x86_64/24.11/compilers/lib:$LD_LIBRARY_PATH

.. note::

   By default, the SDK installs in ``/opt/nvidia/hpc_sdk/`` but the installation directory and version number may be different if you installed the NVIDIA HPC SDK in a custom location or used a different version. To ensure that the correct version of MPI is being used, you can check the paths of ``mpirun`` and ``mpif90`` by running:

   .. code-block:: bash

      which mpirun
      which mpif90
   
   Expected output:

   .. code-block:: bash

      /opt/nvidia/hpc_sdk/Linux_x86_64/24.11/comm_libs/mpi/bin/mpirun
      /opt/nvidia/hpc_sdk/Linux_x86_64/24.11/comm_libs/mpi/bin/mpif90
   
   If these do not match the expected paths, your system might be using a different version of MPI. You will need to update your ``PATH`` and ``LD_LIBRARY_PATH`` environment accordingly.

Compile from source
~~~~~~~~~~~~~~~~~~~~

To compile x3d2 from source, follow these steps:

1. Clone the repository and change into the x3d2 directory

.. code-block:: bash

   git clone https://github.com/xcompact3d/x3d2.git
   cd x3d2


4. Set Fortran Compiler flag to use ``mpif90``

.. code-block:: bash

   export FC=mpif90

5. Create the build system using CMake

.. code-block:: bash

   cmake -S . -B build -DCMAKE_BUILD_TYPE=Release

where ``build`` specifies the directory to which we write the build configuration files and ``DCMAKE_BUILD_TYPE=Release`` specifies the build type (in this case we are using Release build; if you want to install the debug please use ``Debug`` instead).

6. Change into the build directory and create the build

.. code-block:: bash

   cd build
   make

This should create a binary file called ``xcompact`` within ``build/src/`` directory.

7. Verify the installation by running the test suite

.. code-block:: bash

   make test

A successful installation should indicate 100% tests passed.

Alternatively, you can use CMake presets (see `Building with CMake Presets`_
below) which simplify the configure, build, and test steps.


Installing on macOS
-------------------

Dependencies
~~~~~~~~~~~~

It is assumed that you have Xcode Command Line Tools and `Homebrew <https://brew.sh/>`_ installed. To build x3d2 on macOS, you will need the following dependencies:

Fortran Compiler
^^^^^^^^^^^^^^^^

A recent modern Fortran compiler is required to build x3d2. For example GNU Fortran compiler (``gfortran``) version 9 and above. You can install it using Homebrew:

.. code-block:: bash

   brew install gcc

OpenMP is typically included with most modern compilers, including GCC. Therefore, installing ``gcc`` for example should also provide OpenMP support. Next, identify your installed version (e.g., ``gcc-15``). You will need this for the next steps.

.. code-block:: bash

   ls $(brew --prefix)/bin/gcc-*

Open MPI
^^^^^^^^

You must build Open MPI from source to ensure it is compatible with GNU compilers. Replace ``15`` below with your specific GCC version.

.. code-block:: bash

   export HOMEBREW_CXX=g++-15
   export HOMEBREW_CC=gcc-15
   brew install open-mpi --build-from-source

CMake
^^^^^

To build x3d2, you will need CMake version 3.20 and above. You can download the latest version from the `CMake website <https://cmake.org/download/>`_. Alternatively, you can install it using Homebrew:

.. code-block:: bash

   brew install cmake

Compile from source
~~~~~~~~~~~~~~~~~~~

To install x3d2 from source, follow these steps:

1. Clone the repository and change into the x3d2 directory

.. code-block:: bash

   git clone https://github.com/xcompact3d/x3d2.git

2. Configure compilers
macOS users must explicitly set GNU compilers for C/C++ to ensure OpenMP support is detected correctly. Export the compilers (replace ``15`` with your installed version if different):

.. code-block:: bash

   export FC=mpif90
   export CC=gcc-15
   export CXX=g++-15

3. Create the build system using CMake

.. code-block:: bash

   cmake -S . -B build -DCMAKE_BUILD_TYPE=Release

where ``build`` specifies the directory to which we write the build configuration files and ``DCMAKE_BUILD_TYPE=Release`` specifies the build type (in this case we are using Release build; if you want to install the debug please use ``Debug`` instead).

4. Change into the build directory and create the build

.. code-block:: bash

   cd build
   make

This should create a binary file called ``xcompact`` within ``build/src/`` directory.

5. Verify the installation by running the test suite

.. code-block:: bash

   make test

A successful installation should indicate 100% tests passed.

Alternatively, you can use CMake presets (see `Building with CMake Presets`_
below) which simplify the configure, build, and test steps.


Building with CMake Presets
---------------------------

x3d2 provides a ``CMakePresets.json`` file that bundles the compiler,
build type, and feature flags into named presets. This avoids having to
remember individual ``-D`` flags and ensures consistent builds.

Listing available presets
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   cmake --list-presets

Available presets
~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1
   :widths: 30 70

   * - Preset
     - Description
   * - ``gnu-debug``
     - GNU compiler, debug build with OpenMP backend
   * - ``gnu-release``
     - GNU compiler, optimised release build with OpenMP backend
   * - ``gnu-debug-adios2``
     - GNU compiler, debug build with OpenMP backend and ADIOS2
   * - ``gnu-release-adios2``
     - GNU compiler, optimised release build with OpenMP backend and ADIOS2
   * - ``nvhpc-debug``
     - NVHPC compiler, debug build with CUDA backend
   * - ``nvhpc-release``
     - NVHPC compiler, optimised release build with CUDA backend
   * - ``nvhpc-debug-adios2``
     - NVHPC compiler, debug build with CUDA backend and ADIOS2
   * - ``nvhpc-release-adios2``
     - NVHPC compiler, optimised release build with CUDA backend and ADIOS2
   * - ``cray-debug``
     - Cray compiler, debug build
   * - ``cray-release``
     - Cray compiler, optimised release build
   * - ``cray-debug-adios2``
     - Cray compiler, debug build with ADIOS2
   * - ``cray-release-adios2``
     - Cray compiler, optimised release build with ADIOS2

Usage
~~~~~

To configure, build, and test using a preset:

.. code-block:: bash

   # Configure
   cmake --preset gnu-debug

   # Build
   cmake --build --preset gnu-debug

   # Run tests
   ctest --preset gnu-debug

Replace ``gnu-debug`` with any preset name from the table above.

.. note::

   Each preset defines its own build directory (e.g. ``build/`` for GNU,
   ``build-cuda/`` for NVHPC, ``build-cray/`` for Cray), so multiple
   configurations can coexist without interfering with each other.

.. note::

   The ``nvhpc-*`` presets use ``mpif90`` as the Fortran compiler, which is
   an MPI wrapper that delegates to whichever Fortran compiler is in your
   environment. Ensure the NVIDIA HPC SDK is in your ``PATH`` before using
   these presets so that ``mpif90`` resolves to ``nvfortran``. See the
   `NVIDIA HPC SDK`_ section above for environment setup instructions.
