data Result x y =  Err x | Ok y

unwrap (Err error) = error
unwrap (Ok res) = unwrap res

main = print (unwrap (Ok (Ok (Ok( (Err 4))))))
