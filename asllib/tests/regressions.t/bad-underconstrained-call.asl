func GetBitAt{N}(x: bits(N), i: integer {0..N-1}) => bits(1)
begin
  return x[i];
end;

func GetMiddleBit{M}(x: bits(M)) => bits(1)
begin
  // return GetBitAt(x, M DIVRM 2);
  return GetBitAt{M}(x, M);
end;

func main() => integer
begin
  - = GetMiddleBit{8}('11110000');
  return 0;
end;
