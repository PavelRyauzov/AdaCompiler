procedure Proc
 (Var1 : Integer;
  Var2 : Integer;
  Var3 : Integer);

function Func (Var : Integer) return Integer;

procedure Hello is
    Var1 : INTEGER;
	Var2 : INTEGER;
    Var3 : INTEGER;
	
begin
    null;
end Hello;

procedure Proc
 (Var1 : Integer;
  Var2 : Integer;
  Var3 : Integer)
is
begin
   Var3 := Var3 + 1;
end Proc;

function Func (Var : Integer) return Integer
is
begin
   return Var + 1;
end Func;


procedure Show_Increment is
    A, B, C : Integer;

    procedure Display_Result is
    begin
        null;
    end Display_Result;

begin
    A := 10;
    B := 3;
    C := Increment_By (A, B);
    Display_Result;
end Show_Increment;