impdef func Foo{N: integer{32,64}}(n : boolean) => bits(N)
begin
  return Zeros{N};
end;

implementation func Foo{N: integer{32,64}}(m : boolean) => bits(N)
begin
  return Ones{N};
end;
