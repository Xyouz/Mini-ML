let count = ref (-1)
let letter_count = ref 0

let greekletter n = match n with
  | 1  -> "'a"
  | 2  -> "'b"
  | 3  -> "'c"
  | 4  -> "'d"
  | 5  -> "'e"
  | 6  -> "'f"
  | 7  -> "'g"
  | 8  -> "'h"
  | 9  -> "'i"
  | 10 -> "'j"
  | 11 -> "'k"
  | 12 -> "'l"
  | _  -> "'t"

let next_letter () = incr letter_count; count := (-1)

let label () = (incr count);
  if !count = 0
  then greekletter !letter_count
  else ((greekletter !letter_count) ^ (string_of_int (!count)))

