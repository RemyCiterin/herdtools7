func BadBitCount {N} (x: bits(N)) => integer {0..N}
begin
  return N + 1;
end;

func main() => integer
begin
  assert BadBitCount ('101') == 2;
  return 0;
end;
