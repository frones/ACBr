program SPEDPisCofinsImp;

uses
  Forms,
  UFormImport in 'UFormImport.pas' {FormImport};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormImport, FormImport);
  Application.Run;
end.
