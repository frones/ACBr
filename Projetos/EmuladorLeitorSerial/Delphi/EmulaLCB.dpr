program EmulaLCB;

uses
  Forms,
  EmulaLCB1 in 'EmulaLCB1.pas' {frEmulador};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrEmulador, frEmulador);
  Application.Run;
end.
