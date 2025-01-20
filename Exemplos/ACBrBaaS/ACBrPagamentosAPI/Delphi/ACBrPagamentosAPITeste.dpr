program ACBrPagamentosAPITeste;

uses
  Forms,
  udemobaas in 'udemobaas.pas' {Form1},
  uPagamentos in 'uPagamentos.pas' {frmPagamentos};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPagamentosAPITeste, frPagamentosAPITeste);
  Application.Run;
end.
