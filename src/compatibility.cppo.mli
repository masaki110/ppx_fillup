val lident_of_path : Path.t -> Longident.t
val type_structure : Env.t -> Parsetree.structure -> Typedtree.structure
val repr_type : Env.t -> Types.type_expr -> Types.type_desc
val match_type : Env.t -> Types.type_expr -> Types.type_expr -> bool
val unify : Env.t -> Types.type_expr -> Types.type_expr -> unit
val default_untyper : Untypeast.mapper
