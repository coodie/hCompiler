# hCompiler
Compiler for subset of C language written in Haskell purely for fun

This is simple compiler for subset of C language written purely for fun - this means this project isn't associated with any university course, company etc. I wrote this, because I always wanted to know how compilers work, learn some assembly and Haskell.

What is supported:

- Recursive function calls
- Stack based memory allocation and variables naming
- 4-byte ints
- If and while.

Installing this might bring some problems. You have to install proper Haskell libraries (like Parsec). Keep in mind you also need 32-bit standard C library to actually use this compiler! 

This project is very small and has many bugs and lacks ton of features. But you can't expect much from inexperienced student which spent around 4-5 days to get this working.

Overall structure of project:
- Parsing is done using Parsec
- Translating to assembly is done directly from parsed code to asm string.
- Asm string is used as an input to 'GNU as' which produces .o file
- .o file is then fed to gcc to link it with standard library, and trust me, getting this job done using ld is nightmare, at least if you try to see what flags gcc gives to ld.

Look into issues for more information.
