unit Principal;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, ACBrCupomVerde, ACBrNFe, ACBrBase, ACBrSocket;

type

  { TForm1 }

  TfrACBrCupomVerdeTeste = class(TForm)
    ACBrCupomVerde1: TACBrCupomVerde;
    btConsultar: TButton;
    btAdicionar: TButton;
    btCancelar: TButton;
    btEnviar: TButton;
    btLimpar: TButton;
    edChave: TEdit;
    edValorTotal: TEdit;
    edParcelas: TEdit;
    edCPF: TEdit;
    edCPF1: TEdit;
    edCodOperador: TEdit;
    edCodDocumento: TEdit;
    edDescricao: TEdit;
    edXML: TEdit;
    gbComprovantes: TGroupBox;
    lbComprovantes: TLabel;
    lbChave: TLabel;
    lbValorTotal: TLabel;
    lbParcelas: TLabel;
    lbCPF: TLabel;
    lbCPF1: TLabel;
    lbCodOperador: TLabel;
    lbCodDocumento: TLabel;
    lbDescricao: TLabel;
    lbXML2: TLabel;
    mmLog: TMemo;
    mmLogCancelar: TMemo;
    mmLogEnviar: TMemo;
    OpenDialog1: TOpenDialog;
    pgPrincipal: TPageControl;
    btAcharXML: TSpeedButton;
    tsCancelarDocumento: TTabSheet;
    tsConsultarCPF: TTabSheet;
    tsEnviarXML: TTabSheet;
    procedure btAdicionarClick(Sender: TObject);
    procedure btCancelarClick(Sender: TObject);
    procedure btConsultarClick(Sender: TObject);
    procedure btEnviarClick(Sender: TObject);
    procedure btAcharXMLClick(Sender: TObject);
    procedure btLimparClick(Sender: TObject);
  private
    function CarregarXML: AnsiString;

  public

  end;

var
  frACBrCupomVerdeTeste: TfrACBrCupomVerdeTeste;

implementation

uses
  synautil,
  ACBrUtil.Base;

{$R *.dfm}

{ TForm1 }

procedure TfrACBrCupomVerdeTeste.btConsultarClick(Sender: TObject);
begin
  if EstaVazio(edCPF.Text) then
  begin
    ShowMessage('Preencha o CPF');
    Exit;
  end;

  ACBrCupomVerde1.ArqLOG := '_Log.txt';
  ACBrCupomVerde1.NivelLog := 4;
  if ACBrCupomVerde1.ConsultarCPF(edCPF.Text) then
    mmLog.Lines.Text := ACBrCupomVerde1.RespostaConsulta.AsJSON
  else
    mmLog.Lines.Text := ACBrCupomVerde1.RespostaErro.AsJSON;
end;

procedure TfrACBrCupomVerdeTeste.btEnviarClick(Sender: TObject);
begin  
  if EstaVazio(edXML.Text) then
  begin
    ShowMessage('Preencha os Dados');
    Exit;
  end;

  ACBrCupomVerde1.ArqLOG := '_Log.txt';
  ACBrCupomVerde1.NivelLog := 4;
  ACBrCupomVerde1.XMLEnviado.xml := CarregarXML;
  ACBrCupomVerde1.XMLEnviado.cpf := edCPF1.Text;
  ACBrCupomVerde1.XMLEnviado.codigoOperador := edCodOperador.Text;
  ACBrCupomVerde1.XMLEnviado.codDocumento := edCodDocumento.Text;
  if ACBrCupomVerde1.EnviarXML then
  begin
    ShowMessage('Enviado com Sucesso!');
    mmLogEnviar.Lines.Text := ACBrCupomVerde1.RespostaConsulta.AsJSON
  end
  else
  begin
    mmLogEnviar.Lines.Text := ACBrCupomVerde1.RespostaErro.AsJSON;
    ShowMessage('Erro ao enviar: ' + ACBrCupomVerde1.RespostaErro.message);
  end;
end;

procedure TfrACBrCupomVerdeTeste.btAdicionarClick(Sender: TObject);
begin
  with ACBrCupomVerde1.XMLEnviado.comprovantesPagamento.New do
  begin
    descricao := edDescricao.Text;
    parcelas := StrToIntDef(edParcelas.Text, 0);
    valorTotal := StrToFloatDef(edValorTotal.Text, 0);
  end;
  lbComprovantes.Caption := 'Comprovantes: ' + IntToStr(ACBrCupomVerde1.XMLEnviado.comprovantesPagamento.Count);
end;

procedure TfrACBrCupomVerdeTeste.btCancelarClick(Sender: TObject);
begin
  if EstaVazio(edChave.Text) then
  begin
    ShowMessage('Preencha a Chave');
    Exit;
  end;

  ACBrCupomVerde1.ArqLOG := '_Log.txt';
  ACBrCupomVerde1.NivelLog := 4;
  if ACBrCupomVerde1.CancelarDocumento(edChave.Text) then
    ShowMessage('Cancelado com Sucesso!')
  else
    ShowMessage('Erro ao cancelar: ' + ACBrCupomVerde1.RespostaErro.message);
end;

procedure TfrACBrCupomVerdeTeste.btAcharXMLClick(Sender: TObject);
begin
  if NaoEstaVazio(edXML.Text) then
    OpenDialog1.FileName := edXML.Text;
  if OpenDialog1.Execute then
    edXML.Text := OpenDialog1.FileName;
end;

procedure TfrACBrCupomVerdeTeste.btLimparClick(Sender: TObject);
begin
  ACBrCupomVerde1.XMLEnviado.xml := EmptyStr;
  ACBrCupomVerde1.XMLEnviado.cpf := EmptyStr;
  ACBrCupomVerde1.XMLEnviado.codigoOperador := EmptyStr;
  ACBrCupomVerde1.XMLEnviado.codDocumento := EmptyStr;
  ACBrCupomVerde1.XMLEnviado.comprovantesPagamento.Clear;
end;

function TfrACBrCupomVerdeTeste.CarregarXML: AnsiString;
var
  fs: TFileStream;
  wXML: AnsiString;
  wFile: String;
begin
  Result := EmptyStr;
  wFile := edXML.Text;
  if EstaVazio(wFile) or (not FileExists(wFile)) then
    Exit;

  fs := TFileStream.Create(wFile, fmOpenRead or fmShareDenyWrite);
  try
    fs.Position := 0;
    wXML := ReadStrFromStream(fs, fs.Size);
    Result := wXML;
  finally
    fs.Free;
  end;
end;

end.

