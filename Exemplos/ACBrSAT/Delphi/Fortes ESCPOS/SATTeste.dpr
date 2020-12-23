program SATTeste;

uses
  Forms, 
  unit1 in 'unit1.pas' {Form1},
  configuraserial in 'configuraserial.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SAT Teste - Projeto ACBr';
  Application.CreateForm(TForm1, Form1) ;
  Application.Run;
end.

