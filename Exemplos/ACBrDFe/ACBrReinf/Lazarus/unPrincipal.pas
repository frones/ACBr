unit unPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RxPlacemnt, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, ACBrReinf;

type
  TForm2 = class(TForm)
    Label5: TLabel;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    btnGerar: TButton;
    edProtocolo: TEdit;
    chkClear: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    chk1000: TCheckBox;
    cbS1010: TCheckBox;
    cbS1020: TCheckBox;
    cbS1030: TCheckBox;
    cbS1070: TCheckBox;
    rdgOperacao: TRadioGroup;
    rdgGrupo: TRadioGroup;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    edRetificador: TEdit;
    chkS1000Excluir: TCheckBox;
    cbEvento: TComboBox;
    edCPF: TEdit;
    edNIS: TEdit;
    TabSheet2: TTabSheet;
    mmoRet: TMemo;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    Memo2: TMemo;
    TabSheet5: TTabSheet;
    mmoStatus: TMemo;
    FormStorage1: TFormStorage;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure btnGerarClick(Sender: TObject);
  private
    FACBrReinf: TACBrReinf;
    procedure SelecionaEventos;
    procedure LimparDocsPasta;
    procedure Configurar(AACBrReinf: TACBrReinf);
    {Eventos}
    procedure GerarReinf1000;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses ACBrDFeSSL, pcnConversao, ShellAPI;

procedure TForm2.btnGerarClick(Sender: TObject);
begin
  FACBrReinf := TACBrReinf.Create(nil);
  try
    Configurar(FACBrReinf);
  finally
    FACBrReinf.Free;
  end;
end;

procedure TForm2.Configurar(AACBrReinf: TACBrReinf);
begin
  // Configuracao Geral
  AACBrReinf.Configuracoes.Arquivos.PathSchemas := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Schemas2.3';
  AACBrReinf.Configuracoes.Arquivos.Salvar := False;
  AACBrReinf.Configuracoes.Arquivos.SepararPorCNPJ := True;
  AACBrReinf.Configuracoes.Geral.Salvar := True;
  AACBrReinf.Configuracoes.WebServices.Salvar  := True;
  AACBrReinf.Configuracoes.Geral.FormaEmissao := teNormal;
  AACBrReinf.Configuracoes.WebServices.Ambiente := taHomologacao;
  AACBrReinf.Configuracoes.WebServices.Salvar := True;
  // Configuracao WebService
  AACBrReinf.Configuracoes.WebServices.UF := 'CE';
  AACBrReinf.Configuracoes.Certificados.VerificarValidade := True;
  AACBrReinf.Configuracoes.WebServices.AguardarConsultaRet      := 5000; // tempo padrão que vai aguardar para consultar após enviar a NF-e
  AACBrReinf.Configuracoes.WebServices.IntervaloTentativas      := 3000; // Intervalo entre as tentativas de envio
  AACBrReinf.Configuracoes.WebServices.Tentativas               := 10;   // quantidade de tentativas de envio
  AACBrReinf.Configuracoes.WebServices.AjustaAguardaConsultaRet := True; // ajustar "AguardarConsultaRet" com o valor retornado pelo webservice
  // Configuracao Certificados
  AACBrReinf.Configuracoes.Geral.SSLLib := libOpenSSL;
  AACBrReinf.Configuracoes.Geral.SSLHttpLib := httpWinHttp;

  AACBrReinf.Configuracoes.Certificados.ArquivoPFX := 'C:\Certificados\DATAFOCUS_1234.pfx';
  AACBrReinf.Configuracoes.Certificados.NumeroSerie := '23bce65ca331e63aebe165850790c9a6';
  AACBrReinf.Configuracoes.Certificados.Senha := '1234';

  {Identificação}
 // AACBrReinf.IdEmpregador :=  '02191905000111';
 // AACBrReinf.IdTransmissor := '02191905000111';
end;

procedure TForm2.GerarReinf1000;
begin

end;

procedure TForm2.LimparDocsPasta;
var
  path: string;
  FileOp: TSHFileOpStruct;
begin
  try
    path := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) + 'Docs';
    FillChar(FileOp, SizeOf(FileOp), 0);
    FileOp.wFunc := FO_DELETE;
    FileOp.pFrom := PChar(path+#0);//double zero-terminated
    FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
    SHFileOperation(FileOp);
    ForceDirectories(path);
  except
  end;
end;

procedure TForm2.SelecionaEventos;
begin
  if chk1000.Checked then
    GerarReinf1000;
end;

end.
