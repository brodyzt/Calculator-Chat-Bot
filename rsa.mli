(*all the rsa comutation, using the congruence module, in addition to prime
 *gerneration*)

type public_key = integer * integer

type private_key = nteger * integer * integer

 val gen_private_key: unit -> private_key

 val gen_public_key: unit -> public_key

 val encrypt: private_key -> string -> integer

 val decrypt: private_key -> integer -> string

 val crack: public_key -> integer -> string option
