program PinPadTest;



uses
  Forms,
  configuraserial in 'configuraserial.pas',
  formprincipal in 'formprincipal.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
