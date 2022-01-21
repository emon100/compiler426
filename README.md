#426CulturalHeritage
## How to start

1. Install LLVM >= 12.
2. `mkdir build && cd build && cmake .. && make`

## Project Stucture
```
├── cmake    # Some cmake files for antlr4
├── CMakeLists.txt # project's Cmakelists.txt
├── example   # language example
│   ├── comment.txt
│   └── longprogram.txt
├── grammar426.g4  # language's definition in g4
├── README.md     
├── src      # source file directory
    └── main.cpp
└── third_party # third party dependencies
    └── antlr-4.9.3-complete.jar
```