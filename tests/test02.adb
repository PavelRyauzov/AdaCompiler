procedure Show_Increment is
    A, B, C : Integer;
    type Grade is array(0..100) of Float;   
    function Display_Result return Integer is
    begin
        return 5;
    end Display_Result;

begin
    A := 10;
    B := 3;

    for Iter in 1..5 loop
		null;
	end loop;

    -- test comment

    C := Display_Result();
end Show_Increment;