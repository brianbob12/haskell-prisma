# haskell-prisma
## Cyrus Singer (cysinger), Matthew Pattok (mpattok)

Haskell Prisma is a Haskell generator for Prisma ORM, supporting basic CRUD operations on SQLite databases.

- `src/HaskellPrisma.hs` exports the one function a user should interact with (for generating modules): `generateModule`. It takes two arguments, the location of the `schema.prisma` file to parse and the directory to put the generated module files in. 
  - `src/PrismaSchemaRaw.hs` contains the abstract syntax of a Prisma schema, as well as a `ToString` typeclass (used for printing a schema as it would appear in a `schema.prisma` file-- we chose not to use `Show` for this because it was useful to be able to see both the AST representation and the concrete syntax) and instances for the AST. 
  - `src/PrismaGenerator.hs` contains the code to generate modules. It provides the function `generate`, which takes a schema AST and outputs a list of string pairs, where a pair gives the filename to be created and the second contains the contents it should have.
    - `src/GeneratorToolkit.hs` provides some utility functions used by `PrismaGenerator`
    - `src/FormatString.hs` provides an implementation of format strings used by `PrismaGenerator`
  - `src/PrismaParser.hs` provides `parseSchema`, which parses a schema AST from schema source code as a string
    - `src/PrismaParserInternal.hs` provides parsers for various parts of the schema AST.
    - `src/Parser.hs` is the parser module from class, but with a `Monad` instance for `Parser`
- `test/Spec.hs` contains the `main` function to run all tests
  - `test/TestPrismaParser.hs` contains QuickCheck properties on the parser
  - `test/GeneratorToolkitSpec.hs` contains unit tests on the toolkit
  - `test/FormatStringSpec.hs` contains unit tests on the format strings
- `app/Main.hs` defines the executable, which is an interface with `HaskellPrisma`
