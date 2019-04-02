program SPEDFiscalImp;

uses
  Forms,
  UFormImport in 'UFormImport.pas' {FormImport};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormImport, FormImport);
  Application.Run;
end.
