{ Program Seven utilizes the average cost criterion. The model is service }
{ rate control of the continuous time M / M / 1 queue with linear holding }
{ costs. The benchmark policy serves at that constant rate that minimizes }
{ the average cost among constant rate policies. }

 


 

program AverageCostRateControlContinuousTimeQueue (input, output);

 

{Section 10.4}

{This is a single server queue with (exponential) service rates being the actions.}

{The arrivals occur according to a Poisson process with rate lambda . }

{The excess probability is sent to state N . }

{Because the print out is organized in 4 columns, we must choose N divisible by 4.}

 

const

N = 48;

NUMACT = 3;

type

RLista = array[1..NUMACT] of double;

RListb = array[0..N] of double;

IntList = array[1..N] of integer;

 

var

MAX, S, I: integer;

lambda, H, tau, arr, epsilon, Old, delta: double;

Act, TAct, CostAct: RLista;

Fun, WFun: RListb;

OptAct: IntList;

fpout: TEXT;

file_name: string[80];

 

procedure FillAct;

{This creates the array of service rates.}

{The rates are positive increasing numbers and the largest rate must exceed lambda . }

 

var

i: integer;

begin

writeln('Please enter', NUMACT : 2, ' positive increasing service rates. The largest rate must exceed lambda.');

writeln(fpout, 'The service rates are:');

for i := 1 to NUMACT do

begin

read(Act[i]);

writeln(fpout, Act[i] : 15 : 6);

end;

end;

 

procedure FillCostAct;

{This creates the array of service rate costs. These are nonnegative increasing numbers . }

 

var

i: integer;

begin

writeln('Please enter', NUMACT : 2, ' nonnegative increasing service rate costs.');

writeln(fpout, 'The service rate costs are:');

for i := 1 to NUMACT do

begin

read(CostAct[i]);

writeln(fpout, CostAct[i] : 15 : 6);

end;

end;

 

procedure FillTAct;

{This creates an array whose entries are tau x the service rates.}

 

var

i: integer;

begin

for i := 1 to NUMACT do

begin

TAct[i] := tau * Act[i];

end;

end;

 

procedure InitialFun;

{This fills the Fun vector with zeros. Fun is the u array.}

 

var

i: integer;

begin

for i := 0 to N do

begin

Fun[i] := 0;

end;

end;

 

procedure CreateWFun;

{This creates the WFun array. This is the w array.}



var

i, a, TempAct: integer;

Temp, TempNew, New: double;

begin

   WFun[0] := (1 - arr) * Fun[0] + arr * Fun[1];

   for i := 1 to N do

   begin

      if i < N then

      begin

	 New := Fun[i + 1];

      end;

      if i = N then

      begin

	 New := Fun[N];

      end;

      Temp := CostAct[1] + TAct[1] * Fun[i - 1] + (1 - arr - TAct[1]) * Fun[i];

      TempAct := 1;

      for a := 2 to NUMACT do

      begin

	 TempNew := CostAct[a] + TAct[a] * Fun[i - 1] + (1 - arr - TAct[a]) * Fun[i];

	 if TempNew < Temp then

	 begin

	    Temp := TempNew;

	    TempAct := a;

	 end;

      end;

      WFun[i] := H * i + arr * New + Temp;

      OptAct[i] := TempAct;

   end;

end;

 

procedure UpdateFun;

 

var

i: integer;

begin

Fun[0] := 0;

for i := 1 to N do

begin

Fun[i] := WFun[i] - WFun[0];

end;

end;

 

procedure WriteOptAct;

 

var

g: integer;

begin

for g := 0 to (N div 4) - 1 do

begin

writeln(4 * g + 1 : S, OptAct[4 * g + 1] : S, 4 * g + 2 : S, OptAct[4 * g + 2] : S, 4 * g + 3 : S, OptAct[4 * g + 3] : S, 4 * g +
4 : S, OptAct[4 * g + 4] : S);

writeln(fpout, 4 * g + 1 : S, OptAct[4 * g + 1] : S, 4 * g + 2 : S, OptAct[4 * g + 2] : S, 4 * g + 3 : S, OptAct[4 * g + 3] : S, 4
* g + 4 : S, OptAct[4 * g + 4] : S);

end;

end;

 

{Here is the main program.}

 

begin

writeln('Enter the run name.');

readln(file_name);

Assign(fpout, file_name);

rewrite(fpout);

writeln(fpout, file_name);

 

writeln;

writeln(fpout);

 

writeln('The approximation level is', N);

writeln(fpout, 'The approximation level is', N);

 

writeln('Enter the positive customer arrival rate.');

readln(lambda);

writeln(fpout, 'The customer arrival rate is', lambda : 15 : 6);

 

writeln('Enter the positive holding cost rate coefficient.');

readln(H);

writeln(fpout, 'The holding cost rate coefficient is', H : 15 : 6);

 

writeln('Enter the tolerance. This is a small positive number.');

readln(epsilon);

writeln(fpout, 'The tolerance level is', epsilon);

 

FillAct;

FillCostAct;

 

writeln('Enter the maximum number of iterations allowed.');

readln(MAX);

writeln(fpout, 'The maximum number of iterations allowed is', MAX);

 

writeln;

writeln(fpout);

 

tau := 0.5 * (1 / (lambda + Act[NUMACT]));

FillTAct;

arr := tau * lambda;

InitialFun;

Old := 1;

delta := 1;

I := 1;

S := 8;

 

 

while (delta > epsilon) and (I < MAX) do

begin

CreateWFun;

delta := abs(WFun[0] - Old);

Old := WFun[0];

UpdateFun;

I := I + 1;

end; {while}

 

writeln('The number of iterations is: ', I);

writeln(fpout, 'The number of iterations is: ', I);

 

writeln('The value of delta is: ', delta : 15 : 10);

writeln(fpout, 'The value of delta is: ', delta : 15 : 10);

 

writeln('The approximate average cost is:', WFun[0] : 15 : 6);

writeln(fpout, 'The approximate average cost is:', WFun[0] : 15 : 6);

 

writeln;

writeln(fpout);

 

writeln(fpout, 'The optimal actions are:');

 

WriteOptAct; {modified to also output to file}

close(fpout);

 

end.
