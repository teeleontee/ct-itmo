# LL(1) Parser Generator

You can find examples of how the grammar file should look like at `calc/calc.gr` and `pylambda/pyLambda.gr`.

A sample from `pyLambda.gr`

```
# name: PyLambda
# start: start

start : LAMBDA param COLON expr11

param : ID param2

param2 : COMMA ID param2
       | EPS
       
COMMA : ","
COLON : ":"
LAMBDA : "lambda"
NUM : @ [0-9]+ @
ID : @ [a-z]+ @
```

## Generating files

```kotlin
fun main() {
    generateGrammar("calc/calc.gr")
//    generateGrammar("pylambda/pyLambda.gr")
}
```

The files will be generated at `build/generated/{yourGrammarName}`, to use the generated files
remember to add them to your `build.gradle.kts`

```gradle
sourceSets {
    main {
        java {
            srcDirs("build/generated/Calc")
        }
    }
}
```

## API

After generating you will have a Lexer Parser and tokens class, for example

```kotlin
val parser = CalcParser()
val node = parser.parse("2 + 3 * 4")
println(node.attr) // 14
```

You can also traverse `node` and see the parse tree of the given string.
