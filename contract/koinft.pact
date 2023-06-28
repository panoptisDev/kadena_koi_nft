(module koi-fish-nft
    (defschema fish-schema
      (object
        name:string
        color:string
        age:integer
      )
    )
  
    (defcap mint-fish (account:address fish:fish-schema) :write)
    (defcap get-fish (account:address) :read)
  
    (defun-prim make-fish (name:string color:string age:integer) fish-schema
      { "name": name, "color": color, "age": age }
    )
  
    (defun mint-fish (account:address name:string color:string age:integer)
      (seq
        (enforce-keyset account)
        (let* ((fish (make-fish name color age)))
          (insert! account {"koi-fish-nft": fish})
        )
      )
    )
  
    (defun get-fish (account:address)
      (at 'koi-fish-nft (get account))
    )
  )