{$I ACBr.inc}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACBrCTe, ACBrNFeUtil, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    Button4: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ACBrCTe1 : TACBrCTe;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 ACBrCTe1.WebServices.StatusServico.Executar;
 Memo1.Text := ACBrCTe1.WebServices.StatusServico.RetWS;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
 UF, Documento : String;
begin
 if not(InputQuery('WebServices Consulta Cadastro ', 'UF do Documento a ser Consultado:',    UF)) then
    exit;
 if not(InputQuery('WebServices Consulta Cadastro ', 'Documento(CPF/CNPJ)',    Documento)) then
    exit;
  Documento :=  Trim(NotaUtil.LimpaNumero(Documento));

  ACBrCTe1.WebServices.ConsultaCadastro.UF  := UF;
  if Length(Documento) > 11 then
     ACBrCTe1.WebServices.ConsultaCadastro.CNPJ := Documento
  else
     ACBrCTe1.WebServices.ConsultaCadastro.CPF := Documento;
  ACBrCTe1.WebServices.ConsultaCadastro.Executar;

  Memo1.Text := ACBrCTe1.WebServices.ConsultaCadastro.RetWS;
  ShowMessage(ACBrCTe1.WebServices.ConsultaCadastro.xMotivo);
  ShowMessage(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Items[0].xNome);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 ACBrCTe1 := TACBrCTe.Create(Self);
{$IFDEF ACBrCTeOpenSSL}
 ACBrCTe1.Configuracoes.Certificados.Certificado := 'arquivo.pfx';
 ACBrCTe1.Configuracoes.Certificados.Senha := 'senha';
{$ELSE}
 ACBrCTe1.Configuracoes.Certificados.SelecionarCertificado;
{$ENDIF}
 ACBrCTe1.Configuracoes.WebServices.UF := 'RS';
 ACBrCTe1.Configuracoes.WebServices.Visualizar := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
 Chave : String;
begin
 if not(InputQuery('WebServices Consulta CT ', 'Chave:', Chave)) then
    exit;

  ACBrCTe1.WebServices.Consulta.CTeChave := Chave;
  ACBrCTe1.WebServices.Consulta.Executar;

 Memo1.Text := ACBrCTe1.WebServices.Consulta.RetWS;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
 Chave, Protocolo, Justificativa : String;
begin
 if not(InputQuery('WebServices Cancelar CT ', 'Chave:', Chave)) then
    exit;
 Protocolo := '143090000247269';
 if not(InputQuery('WebServices Cancelar CT ', 'Protocolo:', Protocolo)) then
    exit;
 Justificativa := 'Teste de Cancelamento';
 if not(InputQuery('WebServices Cancelar CT ', 'Justificativa:', Justificativa)) then
    exit;

//  ACBrCTe1.WebServices.Cancelamento.CTeChave := Chave;
//  ACBrCTe1.Cancelamento('Teste de Cancelamento');

  ACBrCTe1.WebServices.Cancelamento.CTeChave      := Chave;
  ACBrCTe1.WebServices.Cancelamento.Protocolo     := Protocolo;
  ACBrCTe1.WebServices.Cancelamento.Justificativa := Justificativa;
  ACBrCTe1.WebServices.Cancelamento.Executar;

 Memo1.Text := ACBrCTe1.WebServices.Cancelamento.RetWS;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
 CNPJ, Modelo, Serie, Ano, NumeroInicial, NumeroFinal, Justificativa : String;
begin
 if not(InputQuery('WebServices Inutilização ', 'CNPJ',   CNPJ)) then
    exit;
 if not(InputQuery('WebServices Inutilização ', 'Ano',    Ano)) then
    exit;
 if not(InputQuery('WebServices Inutilização ', 'Modelo', Modelo)) then
    exit;
 if not(InputQuery('WebServices Inutilização ', 'Serie',  Serie)) then
    exit;
 if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroInicial)) then
    exit;
 if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroFinal)) then
    exit;
 if not(InputQuery('WebServices Inutilização ', 'Justificativa', Justificativa)) then
    exit;
  ACBrCTe1.WebServices.Inutiliza(CNPJ, Justificativa, StrToInt(Ano), StrToInt(Modelo), StrToInt(Serie), StrToInt(NumeroInicial), StrToInt(NumeroFinal));
  Memo1.Lines.Text :=  UTF8Encode(ACBrCTe1.WebServices.Inutilizacao.RetWS);
end;


end.
