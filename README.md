# ppx_fillup

_ppx_fillup_ はOCamlでアドホック多相を実現するためのライブラリです。

<!-- ## Installation
Prepare _ppx_fillup_ in the project. -->

## Buildsystem integration

With Dune, you should add a preprocess directive to your target:
ppx_fillup ファイルのDune を利用して、

```dune 
(executable
 (name test)
 (preprocess (staged_pps ppx_fillup))
 )
```

## Usage 

1. Definition of functions with ad hoc polymorphism

    ```ocaml
    let print inst v = print_endline (inst v)
    ```

    The parameter (e.g. `inst`) changes the behavior of the function.

1. Definition of ___instance___

    ```ocaml
    let show_int [@instance] = string_of_int
    let show_float [@instance] = string_of_float
    ```

    _Instance_ is the value passed to the function parameter (e.g. `inst`) and identified by the `[@instance]`.

1. Execution

    ```ocaml
    let () =
    print ## 123;
    print ## 1.23
    ```

    Pass `##` instead of the argument to the parameter `inst`.

(WIP)