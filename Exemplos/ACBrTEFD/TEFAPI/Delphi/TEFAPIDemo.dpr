program TEFAPIDemo;

uses
  Forms,
  configuraserial in 'configuraserial.pas' {frConfiguraSerial},
  frIncluirPagamento in 'frIncluirPagamento.pas' {FormIncluirPagamento},
  frMenuTEF in 'frMenuTEF.pas' {FormMenuTEF},
  frObtemCampo in 'frObtemCampo.pas' {FormObtemCampo},
  frPrincipal in 'frPrincipal.pas' {FormPrincipal},
  uVendaClass in 'uVendaClass.pas',
  frExibeMensagem in 'frExibeMensagem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TEFAPIDemo';
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.

