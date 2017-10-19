unit ACBrBoletoRelatorioRetorno;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  SysUtils, Controls, Forms, Graphics, Dialogs, RLReport,
  ACBrBoleto, ACBrUtil, FileUtil;

type

  { TfrmACBrBoletoRelatorioRet }

  TfrmACBrBoletoRelatorioRet = class(TForm)
    rliBanco: TRLImage;
    RLBand1: TRLBand;
    rlbRodape: TRLBand;
    rlbCabecalhoTitulos: TRLBand;
    rlbCabecalho: TRLBand;
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw13: TRLDraw;
    RLDraw14: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw16: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw18: TRLDraw;
    RLDraw19: TRLDraw;
    RLDraw20: TRLDraw;
    RLDraw21: TRLDraw;
    RLDraw22: TRLDraw;
    rliLogo: TRLImage;
    RLLabel1: TRLLabel;
    RLLabel10: TRLLabel;
    RLLabel11: TRLLabel;
    RLLabel12: TRLLabel;
    RLLabel13: TRLLabel;
    RLLabel14: TRLLabel;
    RLLabel15: TRLLabel;
    RLLabel16: TRLLabel;
    RLLabel17: TRLLabel;
    RLLabel18: TRLLabel;
    rllTituloCarteira: TRLLabel;
    rllTituloDocumento: TRLLabel;
    rllTituloNossoNumero: TRLLabel;
    RLLabel2: TRLLabel;
    rllTituloDataPago: TRLLabel;
    rllTituloValorPago: TRLLabel;
    rlmTituloRejeicao: TRLMemo;
    rllTituloValor: TRLLabel;
    rllTituloOcorrencia: TRLLabel;
    RLLabel3: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLLabel7: TRLLabel;
    RLLabel8: TRLLabel;
    ResumoRetornoRemessa: TRLReport;
    RLLabel9: TRLLabel;
    RLSubDetail1: TRLSubDetail;
    rllArquivoRetorno: TRLLabel;
    rllNumeroPagina: TRLLabel;
    rllNomeCedente: TRLLabel;
    rllCNPJ: TRLLabel;
    rllBanco: TRLLabel;
    rllAgencia: TRLLabel;
    rllCodigoCedende: TRLLabel;
    rllConta: TRLLabel;
    procedure ResumoRetornoRemessaDataRecord(Sender: TObject; RecNo: Integer;
      CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure rlbCabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLLabel9BeforePrint(Sender: TObject; var AText: string;
      var PrintIt: Boolean);
    procedure rllNumeroPaginaBeforePrint(Sender: TObject; var AText: string;
      var PrintIt: Boolean);
    procedure rllTituloCarteiraBeforePrint(Sender: TObject; var aText: string;
      var PrintIt: Boolean);
    procedure rllTituloDataPagoBeforePrint(Sender: TObject; var AText: string;
      var PrintIt: Boolean);
    procedure rllTituloDocumentoBeforePrint(Sender: TObject; var aText: string;
      var PrintIt: Boolean);
    procedure rllTituloNossoNumeroBeforePrint(Sender: TObject;
      var aText: string; var PrintIt: Boolean);
    procedure rllTituloOcorrenciaBeforePrint(Sender: TObject; var aText: string;
      var PrintIt: Boolean);
    procedure rllTituloValorBeforePrint(Sender: TObject; var aText: string;
      var PrintIt: Boolean);
    procedure rllTituloValorPagoBeforePrint(Sender: TObject; var AText: string;
      var PrintIt: Boolean);
    procedure rlmTituloRejeicaoBeforePrint(Sender: TObject; var aText: string;
      var PrintIt: Boolean);
    procedure RLSubDetail1DataRecord(Sender: TObject; RecNo: Integer;
      CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
  private
    FACBrBoleto: TACBrBoleto;
    FArquivoRetorno: String;
    FLogoEmpresa: String;
    FNumItem: Integer;
    FPathLogo : String;
    { private declarations }
  public
    { public declarations }
    property ArquivoRetorno : String read FArquivoRetorno write FArquivoRetorno;
    property ACBrBoleto     : TACBrBoleto read FACBrBoleto write FACBrBoleto;
    property PathLogo       : String read FPathLogo write FPathLogo;
    property LogoEmpresa    : String read FLogoEmpresa write FLogoEmpresa;
  end;

const
  cNomeBanco = '%.3d - %s';
  cContaAgencia = '%s-%s';


var
  frmACBrBoletoRelatorioRet: TfrmACBrBoletoRelatorioRet;

implementation

{$R *.lfm}

{ TfrmACBrBoletoRelatorioRet }

procedure TfrmACBrBoletoRelatorioRet.rlbCabecalhoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  rliBanco.Picture.LoadFromFile(Self.FPathLogo + PathDelim + IntToStrZero(self.ACBrBoleto.Banco.Numero, 3)+'.bmp' );

  if FileExists(FLogoEmpresa) then
     rliLogo.Picture.LoadFromFile(FLogoEmpresa);

  rllArquivoRetorno.Caption := ArquivoRetorno;
  rllNomeCedente.Caption    := self.ACBrBoleto.Cedente.Nome;
  rllCNPJ.Caption           := self.ACBrBoleto.Cedente.CNPJCPF;

  rllBanco.Caption          := Format(cNomeBanco, [self.ACBrBoleto.Banco.Numero, self.ACBrBoleto.Banco.Nome]);
  rllAgencia.Caption        := Format(cContaAgencia, [self.ACBrBoleto.Cedente.Agencia, self.ACBrBoleto.Cedente.AgenciaDigito]);
  rllCodigoCedende.Caption  := self.ACBrBoleto.Cedente.CodigoCedente;
  rllConta.Caption          := Format(cContaAgencia, [self.ACBrBoleto.Cedente.Conta, self.ACBrBoleto.Cedente.ContaDigito]);
end;

procedure TfrmACBrBoletoRelatorioRet.RLLabel9BeforePrint(Sender: TObject;
  var AText: string; var PrintIt: Boolean);
begin
  AText := self.ACBrBoleto.Cedente.Nome;
end;

procedure TfrmACBrBoletoRelatorioRet.rllNumeroPaginaBeforePrint(
  Sender: TObject; var AText: string; var PrintIt: Boolean);
begin
  AText := IntToStr(ResumoRetornoRemessa.PageNumber);
end;

procedure TfrmACBrBoletoRelatorioRet.ResumoRetornoRemessaDataRecord(
  Sender: TObject; RecNo: Integer; CopyNo: Integer; var Eof: Boolean;
  var RecordAction: TRLRecordAction);
begin
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrmACBrBoletoRelatorioRet.rllTituloCarteiraBeforePrint(
  Sender: TObject; var aText: string; var PrintIt: Boolean);
begin
  aText := self.ACBrBoleto.ListadeBoletos[self.FNumItem].Carteira;
end;

procedure TfrmACBrBoletoRelatorioRet.rllTituloDataPagoBeforePrint(
  Sender: TObject; var AText: string; var PrintIt: Boolean);
begin
  if self.ACBrBoleto.ListadeBoletos[self.FNumItem].DataCredito > 0 then
     AText := DateToStr(self.ACBrBoleto.ListadeBoletos[self.FNumItem].DataCredito);
end;

procedure TfrmACBrBoletoRelatorioRet.rllTituloDocumentoBeforePrint(
  Sender: TObject; var aText: string; var PrintIt: Boolean);
begin
  aText := self.ACBrBoleto.ListadeBoletos[self.FNumItem].NumeroDocumento;
end;

procedure TfrmACBrBoletoRelatorioRet.rllTituloNossoNumeroBeforePrint(
  Sender: TObject; var aText: string; var PrintIt: Boolean);
begin
  aText := self.ACBrBoleto.ListadeBoletos[self.FNumItem].NossoNumero;
end;

procedure TfrmACBrBoletoRelatorioRet.rllTituloOcorrenciaBeforePrint(
  Sender: TObject; var aText: string; var PrintIt: Boolean);
begin
  aText := ACBrStr(self.ACBrBoleto.ListadeBoletos[self.FNumItem].OcorrenciaOriginal.Descricao);
end;

procedure TfrmACBrBoletoRelatorioRet.rllTituloValorBeforePrint(Sender: TObject;
  var aText: string; var PrintIt: Boolean);
begin
  aText := FormatFloat('R$ ,0.00##', self.ACBrBoleto.ListadeBoletos[self.FNumItem].ValorDocumento);
end;

procedure TfrmACBrBoletoRelatorioRet.rllTituloValorPagoBeforePrint(
  Sender: TObject; var AText: string; var PrintIt: Boolean);
begin
  aText := FormatFloat('R$ ,0.00##', self.ACBrBoleto.ListadeBoletos[self.FNumItem].ValorRecebido);
end;

procedure TfrmACBrBoletoRelatorioRet.rlmTituloRejeicaoBeforePrint(
  Sender: TObject; var aText: string; var PrintIt: Boolean);
var
  J: Integer;
begin
  aText := '';
  for J:= 0 to self.ACBrBoleto.ListadeBoletos[self.FNumItem].DescricaoMotivoRejeicaoComando.Count-1 do
    aText := aText + ACBrStr(self.ACBrBoleto.ListadeBoletos[self.FNumItem].DescricaoMotivoRejeicaoComando[J])+sLineBreak;
end;

procedure TfrmACBrBoletoRelatorioRet.RLSubDetail1DataRecord(Sender: TObject;
  RecNo: Integer; CopyNo: Integer; var Eof: Boolean;
  var RecordAction: TRLRecordAction);
begin
  FNumItem := RecNo - 1 ;

  Eof := (RecNo > self.ACBrBoleto.ListadeBoletos.Count) ;
  RecordAction := raUseIt ;
end;

end.

