
constant A = 1 << 10;
constant B = 1 << 10;

let a = ARBITRARY: integer {0..A};
let b = ARBITRARY: integer {0..B};

var z = a ^ b;

func main () => integer
begin
  return 0;
end;
