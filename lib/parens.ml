module ParensMatcher = struct
  open Re

  (** matches a single word *)
  let one_word = word (repn alpha 1 None)
  (** matches any of the punctuation marks found inside the parentheticals *)
  let delims = alt [ char ';'; char ','; char '?'; char '!' ]
  (** matches the repetition of alphanumeric words inside parens *)
  let inner = repn (alt [ one_word; space; delims ]) 1 None
  (** matches left side parens, up to 5 *)
  let left = repn (char '(') 1 (Some 5)
  (** ditto right *)
  let right = repn (char ')') 1 (Some 5)
  let before = opt @@ seq [ upper; repn lower 1 None ]
  let after = opt one_word
  let parens = seq [ before; left; inner; opt delims; right ] |> compile
end