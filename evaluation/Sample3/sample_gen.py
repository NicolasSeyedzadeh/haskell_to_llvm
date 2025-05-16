from pathlib import Path

file = Path("evaluation/Sample3/sample3.ml")


with file.open(mode="w"):
    pass

with file.open(mode="a") as f:
    str_to_print=f"type 'a result = \n | Err of 'a \n | Ok of 'a result\nlet rec unwrap = function\n | Err x -> x\n | Ok r -> unwrap r\nlet x ( "
    for i in range(1000):
        str_to_print+= "Ok("
    str_to_print+= "Err 3"
    for i in range(1001):
        str_to_print+= ")"
    str_to_print+= "\n print_int unwrap x"


    f.write(str_to_print)
