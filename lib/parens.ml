module ParensMatcher = struct
  open Re

  (** matches a single word *)
  let one_word = word (repn alpha 1 None)

  (** matches any of the punctuation marks found inside the parentheticals *)
  let delims = alt [ char ';'; char ','; char '?'; char '!' ]

  (** matches the repetition of alphanumeric words inside parens *)
  let inner =
    repn
      (alt [ one_word; space; delims; seq [ word (repn alpha 1 None); punct ] ])
      1 None

  (** matches left side parens, up to 5 *)
  let left = repn (char '(') 1 (Some 5)

  (** ditto right *)
  let right = repn (char ')') 1 (Some 5)

  let before = opt @@ seq [ upper; repn lower 1 None ]
  let after = opt one_word
  let parens = seq [ before; left; inner; opt delims; right ] |> compile
end

module ParensParser = struct
  module A = Angstrom

  let ( <|> ) = A.( <|> )
  let ( <* ) = A.( <* )
  let ( >>= ) = A.( >>= )
  let ( let+ ) = A.( let+ )
  let ( and+ ) = A.( and+ )
  let punctuation = A.char ';' <|> A.char ',' <|> A.char '!' <|> A.char '?'
  let parens = A.many @@ A.char '('
  let words = A.take_while1 (function 'a' .. 'z' -> true | _ -> false)
  let phrase = words <* A.char '!'
  let parser = A.parse_string ~consume:A.Consume.All words
  let _ = parser "hello world!"
end
