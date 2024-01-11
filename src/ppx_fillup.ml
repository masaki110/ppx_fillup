Ppxlib.Driver.(
  register_transformation
    ~rules:Ppx_fillupsyntax.[ open_instance_toplevel; open_instance_local ]
    ~instrument:(Instrument.make Ppx_filluplib.transform ~position:After)
    (* ~impl:Ppx_filluplib.transform *)
    "ppx_fillup")
