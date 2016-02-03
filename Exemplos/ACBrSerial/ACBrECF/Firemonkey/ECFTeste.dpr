program ECFTeste;

uses
  System.StartUpCopy,
  FMX.Forms,
  ECFTeste1 in 'ECFTeste1.pas' {Form1},
  ConfiguraSerial in 'ConfiguraSerial.pas' {frConfiguraSerial},
  RelatorioGerencialFormatado in 'RelatorioGerencialFormatado.pas' {frmGerencialFormatado},
  VendeItem in 'VendeItem.pas' {frVendeItem},
  EfetuaPagamento in 'EfetuaPagamento.pas' {frPagamento},
  Relatorio in 'Relatorio.pas' {frRelatorio},
  Sobre in 'Sobre.pas' {frmSobre},
  uDAV in 'uDAV.pas' {frmDAV},
  uDAVOS in 'uDAVOS.pas' {frmDAVOS},
  uVendaFrenetica in 'uVendaFrenetica.pas' {FrVendaFrenetica};

{$R *.res}

begin

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmGerencialFormatado, frmGerencialFormatado);
  Application.CreateForm(TfrVendeItem, frVendeItem);
  Application.CreateForm(TfrPagamento, frPagamento);
  Application.CreateForm(TfrRelatorio, frRelatorio);
  Application.CreateForm(TfrmSobre, frmSobre);
  Application.CreateForm(TfrmDAV, frmDAV);
  Application.CreateForm(TfrmDAVOS, frmDAVOS);
  Application.CreateForm(TFrVendaFrenetica, FrVendaFrenetica);
  Application.Run;
end.
