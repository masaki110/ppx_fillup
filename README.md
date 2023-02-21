# ppx_fillup

_ppx_fillup_ はOCamlでアドホック多相を実現するためのライブラリです。

## Buildsystem integration (WIP)

Dune を利用する場合、 以下のように`preprocess`に`staged_pps`として登録します。

```dune 
(executable
 (name test)
 (preprocess (staged_pps ppx_fillup))
 )
```

## Usage (WIP)

### 例：Show関数

1. 関数の用意

    引数`x`をstring型に変換する関数`show`を以下のように定義します。

    ```ocaml
    let show inst v = inst v
    ```
    
1. **インスタンス**の用意

    **インスタンス**は関数の動作を決定する式で、[@instance]という属性を与えることで定義されます。
    例えば、今回は`show`関数の動作を決定するために、`show`関数の第一引数`inst`に渡される式として定義します。
    
    ```ocaml
    let show_int [@instance] = string_of_int
    let show_float [@instance] = string_of_float
    ```
    
1. 実行

    `inst`引数に`##`を渡すと、引数の型に応じて自動的に`show`関数の動作を決定します。
    
    ```ocaml
    show ## 123 
    show ## 1.23
    ```
    
    上記の実行はコンパイル時に以下のように解釈されます。 
    
    ```ocaml
    show show_int 123 
    show show_float 1.23
    ```
