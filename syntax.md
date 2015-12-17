# Jack programming language syntax

The parts of Representation and

## Letters

```
new_line = "\n"
```


## Expression

```
expression = number_literal | string_literal
             | function_application | operator_application
function_application = identifier { expression }
operator_application = expression identifier expression { expression }
```


# Statement

```
statement = declaration_statement | definition_statement | return_statement

declaration_statement = identifier ":" type new_line
definition_statement
  = function_name { argument_name } [ new_line
    { indent ( declaration_statement | defition_statement ) new_line }
    indent ] return_statement new_line
return_statement = "=" expression
```


## Type

```
type = identifier | function_type

function_type = type_list "->" type_list
type_list = type { "," type }
```
