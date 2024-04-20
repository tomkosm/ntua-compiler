# Grace Compiler Project - NTUA Compilers 2023

## Overview
This project contains the source code for a compiler for the Grace programming language, developed as part of the Compilers course at the National Technical University of Athens. The compiler is designed to translate Grace language programs into executable code.

## Prerequisites
To build and run the compiler, the following dependencies are required:
- LLVM-16
- Clang++
- Flex
- Bison

Alternatively, you can use:
- Docker or Podman

## Installation
To build the compiler, you can either use the provided Makefile:

```bash
make
```
Or, build with Docker or Podman, use the corresponding script:

```bash

./scripts/make_docker.sh
# For Podman:
./scripts/make_docker.sh podman
```

## Usage

To compile a Grace program to an executable a.out, use:

```
bash ./scripts/compiler_grace_to_exec.sh [path_to_grace_file]
```

To include optimizations, add the -O flag.

For standard usage following the course specifications, run:

```bash
./scripts/grace.sh
```

## License
This project is licensed under the MIT License, as found in the LICENSE file.

## Contact
For questions or support related to this project, please contact me.

## Additional Information
For more details on the Grace language and its features, refer to the language specification document provided in this repository.
