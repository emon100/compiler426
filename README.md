# 426CulturalHeritage
## How to start

1. Install LLVM >= 12.
2. `mkdir build && cd build && cmake .. && make`
3. To run compiled .llvm, please run commands below.
```zsh
    % llc tmp.llvm -filetype=obj -o ok.o
    % clang ok.o -o ok
    % ./ok
   ```

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

## Usage

`$ compiler <inputfile> <outputfile>`

## Attention
ANTLR 4.10 isn't released yet (Until 2/2/2022), but I'm already using std::any to replace antlrcpp::Any.

Be careful, `std::any_cast<int *>(std::any(nullptr))` is wrong.

Should use, `std::any_cast<int *>(std::make_any<int *>(nullptr))`.