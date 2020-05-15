unit frPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Spin, Buttons, DBCtrls, ExtCtrls, PairSplitter, Grids, ACBrTEFD,
  ACBrPosPrinter;

type

  TProximaOperacao = (popLivre, popCancelarOperacao, popIrParaPagamentos, popIniciarNovaOperacao);
  TStatusOperacao = (stsLivre, stsIniciada, stsEmPagamento, stFinalizada);

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrTEFD1: TACBrTEFD;
    btConfiguracoes: TBitBtn;
    btImprimir: TBitBtn;
    btLerParametros: TBitBtn;
    btLimparImpressao: TBitBtn;
    btLimparLog: TBitBtn;
    btOperacao: TBitBtn;
    btOperacaoTEF: TBitBtn;
    btEfetuarPagamentos: TBitBtn;
    btSalvarParametros: TBitBtn;
    btSerial: TSpeedButton;
    btTestarPosPrinter: TBitBtn;
    btTestarTEF: TBitBtn;
    cbSuportaSaque: TCheckBox;
    cbImprimirViaReduzida: TCheckBox;
    cbSuportaReajusteValor: TCheckBox;
    cbxGP: TComboBox;
    cbxModeloPosPrinter: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    cbAutoAtivar: TCheckBox;
    cbMultiplosCartoes: TCheckBox;
    cbSuportaDesconto: TCheckBox;
    gbPagamentos: TGroupBox;
    Label14: TLabel;
    mImpressao: TMemo;
    mLog: TMemo;
    pBotoesPagamentos: TPanel;
    pBotoesImpressora: TPanel;
    pBotoesMemo: TPanel;
    pImpressao: TPanel;
    pStatus: TPanel;
    seEsperaSleep: TSpinEdit;
    seMaxCartoes: TSpinEdit;
    edTotalOperacao: TEdit;
    edTotalPago: TEdit;
    edTroco: TEdit;
    seTrocoMaximo: TFloatSpinEdit;
    seEsperaSTS: TSpinEdit;
    edLog: TEdit;
    gbValorOperacao: TGroupBox;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    gbConfigTEF: TGroupBox;
    gbConfigImpressora: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pOperacao: TPanel;
    pBotoesConfig: TPanel;
    pgPrincipal: TPageControl;
    SbArqLog: TSpeedButton;
    seTotalAcrescimo: TFloatSpinEdit;
    seColunas: TSpinEdit;
    seTotalDesconto: TFloatSpinEdit;
    seEspLinhas: TSpinEdit;
    seLinhasPular: TSpinEdit;
    seValorOperacao: TFloatSpinEdit;
    Splitter1: TSplitter;
    sgPagamentos: TStringGrid;
    tsConfiguracao: TTabSheet;
    tsOperacao: TTabSheet;
    procedure btEfetuarPagamentosClick(Sender: TObject);
    procedure btOperacaoClick(Sender: TObject);
    procedure btConfiguracoesClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btLimparImpressaoClick(Sender: TObject);
    procedure btLimparLogClick(Sender: TObject);
    procedure btOperacaoTEFClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure seTotalOperacaoChange(Sender: TObject);
    procedure seValorOperacaoChange(Sender: TObject);
  private
    FProximaOperacao: TProximaOperacao;
    FStatusOperacao: TStatusOperacao;
    FTotalOperacao: Double;

    function GetNomeArquivoConfiguracao: String;
    procedure SetProximaOperacao(AValue: TProximaOperacao);
    procedure SetStatusOperacao(AValue: TStatusOperacao);
    procedure SetTotalOperacao(AValue: Double);

  protected
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

    procedure IrParaOperacoesTEF;
    procedure Configurar;
    procedure ConfigurarTEF;
    procedure ConfigurarPosPrinter;

    procedure CalcularTotalOperacao;
    procedure PrepararNovaOperacao;
    procedure CancelarOperacao;
    procedure CancelarTodosPagamentos;

    procedure AdicionarLinhaLog(AMensagem: String);
    procedure AdicionarLinhaImpressao(ALinha: String);
  public
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;

    property StatusOperacao: TStatusOperacao read FStatusOperacao write SetStatusOperacao;
    property ProximaOperacao: TProximaOperacao read FProximaOperacao write SetProximaOperacao;
    property TotalOperacao: Double read FTotalOperacao write SetTotalOperacao;

  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  IniFiles, typinfo,
  configuraserial,
  ACBrUtil, ACBrTEFDClass;

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.FormCreate(Sender: TObject);
var
  I: TACBrTEFDTipo;
  N: TACBrPosPrinterModelo;
  O: TACBrPosPaginaCodigo;
begin
  cbxGP.Items.Clear ;
  For I := Low(TACBrTEFDTipo) to High(TACBrTEFDTipo) do
     cbxGP.Items.Add( GetEnumName(TypeInfo(TACBrTEFDTipo), integer(I) ) ) ;
  cbxGP.Items.Delete(0);  // gpNenhum
  cbxGP.ItemIndex := 0 ;

  cbxModeloPosPrinter.Items.Clear ;
  For N := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModeloPosPrinter.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(N) ) ) ;

  cbxPagCodigo.Items.Clear ;
  For O := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(O) ) ) ;

  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );
  {$IfDef MSWINDOWS}
  ACBrPosPrinter1.Device.AcharPortasUSB( cbxPorta.Items );
  {$EndIf}
  ACBrPosPrinter1.Device.AcharPortasRAW( cbxPorta.Items );

  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;

  {$IfNDef MSWINDOWS}
   cbxPorta.Items.Add('/dev/ttyS0') ;
   cbxPorta.Items.Add('/dev/ttyUSB0') ;
   cbxPorta.Items.Add('/tmp/ecf.txt') ;
  {$Else}
   cbxPorta.Items.Add('\\localhost\Epson') ;
   cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  {$EndIf}

  pgPrincipal.ShowTabs := False;
  pgPrincipal.ActivePageIndex := 0;

  LerConfiguracao;
  FStatusOperacao := High(TStatusOperacao);  // Força atualização
  FProximaOperacao := High(TProximaOperacao);
end;

procedure TFormPrincipal.btOperacaoTEFClick(Sender: TObject);
begin
  IrParaOperacoesTEF;
end;

procedure TFormPrincipal.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TFormPrincipal.btLimparImpressaoClick(Sender: TObject);
begin
  mImpressao.Lines.Clear;
end;

procedure TFormPrincipal.btLimparLogClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TFormPrincipal.btConfiguracoesClick(Sender: TObject);
begin
  ACBrPosPrinter1.Desativar;
  pgPrincipal.ActivePage := tsConfiguracao;
end;

procedure TFormPrincipal.btOperacaoClick(Sender: TObject);
begin
  if ProximaOperacao = popIniciarNovaOperacao then
    PrepararNovaOperacao
  else
    CancelarOperacao;
end;

procedure TFormPrincipal.btEfetuarPagamentosClick(Sender: TObject);
begin
  StatusOperacao := stsEmPagamento;
end;

procedure TFormPrincipal.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

function TFormPrincipal.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt( Application.ExeName,'.ini' ) ;
end;

procedure TFormPrincipal.LerConfiguracao;
Var
  INI : TIniFile ;
begin
  INI := TIniFile.Create(NomeArquivoConfiguracao);
  try
    cbxModeloPosPrinter.ItemIndex := INI.ReadInteger('PosPrinter', 'Modelo', 1);
    cbxPorta.Text := INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta);
    cbxPagCodigo.ItemIndex := INI.ReadInteger('PosPrinter','PaginaDeCodigo', 2);
    ACBrPosPrinter1.Device.ParamsString := INI.ReadString('PosPrinter','ParamsString','');
    seColunas.Value := INI.ReadInteger('PosPrinter','Colunas',ACBrPosPrinter1.ColunasFonteNormal);
    seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasEntreCupons',ACBrPosPrinter1.LinhasEntreCupons);

    cbxGP.ItemIndex := INI.ReadInteger('TEF', 'GP', 0);
    edLog.Text := INI.ReadString('TEF', 'Log', '');
    seEsperaSleep.Value := INI.ReadInteger('TEF', 'Sleep', seEsperaSleep.Value);
    seEsperaSTS.Value := INI.ReadInteger('TEF', 'TimeoutSTS', seEsperaSTS.Value);
    seMaxCartoes.Value := INI.ReadInteger('TEF', 'MaxCartoes', seMaxCartoes.Value);
    seTrocoMaximo.Value := INI.ReadFloat('TEF', 'TrocoMaximo', seTrocoMaximo.Value);
    cbAutoAtivar.Checked := INI.ReadBool('TEF', 'AutoAtivar', cbAutoAtivar.Checked);
    cbImprimirViaReduzida.Checked := INI.ReadBool('TEF', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    cbMultiplosCartoes.Checked := INI.ReadBool('TEF', 'MultiplosCartoes', cbMultiplosCartoes.Checked);
    cbSuportaDesconto.Checked := INI.ReadBool('TEF', 'SuportaDesconto', cbSuportaDesconto.Checked);
    cbSuportaSaque.Checked := INI.ReadBool('TEF', 'SuportaSaque', cbSuportaSaque.Checked);
    cbSuportaReajusteValor.Checked := INI.ReadBool('TEF', 'SuportaReajusteValor', cbSuportaReajusteValor.Checked);
  finally
     INI.Free ;
  end ;
end;

procedure TFormPrincipal.GravarConfiguracao;
Var
  INI : TIniFile ;
begin
  INI := TIniFile.Create(NomeArquivoConfiguracao);
  try
    INI.WriteInteger('PosPrinter', 'Modelo', cbxModeloPosPrinter.ItemIndex);
    INI.WriteString('PosPrinter','Porta', cbxPorta.Text);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo', cbxPagCodigo.ItemIndex);
    INI.WriteString('PosPrinter','ParamsString', ACBrPosPrinter1.Device.ParamsString);
    INI.WriteInteger('PosPrinter','Colunas', seColunas.Value);
    INI.WriteInteger('PosPrinter','EspacoLinhas', seEspLinhas.Value);
    INI.WriteInteger('PosPrinter','LinhasEntreCupons', seLinhasPular.Value);

    INI.WriteInteger('TEF', 'GP', cbxGP.ItemIndex);
    INI.WriteString('TEF', 'Log', edLog.Text);
    INI.WriteInteger('TEF', 'Sleep', seEsperaSleep.Value);
    INI.WriteInteger('TEF', 'TimeoutSTS', seEsperaSTS.Value);
    INI.WriteInteger('TEF', 'MaxCartoes', seMaxCartoes.Value);
    INI.WriteFloat('TEF', 'TrocoMaximo', seTrocoMaximo.Value);
    INI.WriteBool('TEF', 'AutoAtivar', cbAutoAtivar.Checked);
    INI.WriteBool('TEF', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    INI.WriteBool('TEF', 'MultiplosCartoes', cbMultiplosCartoes.Checked);
    INI.WriteBool('TEF', 'SuportaDesconto', cbSuportaDesconto.Checked);
    INI.WriteBool('TEF', 'SuportaSaque', cbSuportaSaque.Checked);
    INI.WriteBool('TEF', 'SuportaReajusteValor', cbSuportaReajusteValor.Checked);
  finally
     INI.Free ;
  end ;
end;

procedure TFormPrincipal.IrParaOperacoesTEF;
begin
  Configurar;
  pgPrincipal.ActivePage := tsOperacao;
  PrepararNovaOperacao;
end;

procedure TFormPrincipal.AdicionarLinhaLog(AMensagem: String);
begin
  mLog.Lines.Add(AMensagem);
end;

procedure TFormPrincipal.AdicionarLinhaImpressao(ALinha: String);
begin
  mImpressao.Lines.Add(ALinha);
end;

procedure TFormPrincipal.SetProximaOperacao(AValue: TProximaOperacao);
var
  MsgOperacao: String;
begin
  if FProximaOperacao = AValue then Exit;

  MsgOperacao := '';

  case AValue of
    popCancelarOperacao:
      MsgOperacao := 'Cancelar Operacao';

    popIniciarNovaOperacao:
      MsgOperacao := 'Nova Operacao';
  end;

  FProximaOperacao := AValue;

  btEfetuarPagamentos.Visible := (FProximaOperacao in [popIrParaPagamentos]);
  btOperacao.Visible := (MsgOperacao <> '');
  btOperacao.Caption := MsgOperacao;
end;

procedure TFormPrincipal.SetStatusOperacao(AValue: TStatusOperacao);
var
  MsgStatus: String;
begin
  if FStatusOperacao = AValue then Exit;

  gbValorOperacao.Enabled := AValue in [stsLivre, stsIniciada];
  gbPagamentos.Enabled := AValue in [stsEmPagamento];

  case AValue of
    stsIniciada:
    begin
      MsgStatus := 'EM VENDA';
      ProximaOperacao := popIrParaPagamentos;
    end;

    stsEmPagamento:
    begin
      MsgStatus := 'EM PAGAMENTO';
      ProximaOperacao := popCancelarOperacao;
      sgPagamentos.SetFocus;
    end;

    stFinalizada:
    begin
      MsgStatus := 'FINALIZADA';
      ProximaOperacao := popIniciarNovaOperacao;
    end;

  else
    MsgStatus := 'CAIXA LIVRE';
    ProximaOperacao := popLivre;
  end;

  pStatus.Caption := MsgStatus;
  FStatusOperacao := AValue;
end;

procedure TFormPrincipal.SetTotalOperacao(AValue: Double);
begin
  edTotalOperacao.Text := FormatFloatBr(AValue);
  FTotalOperacao := AValue;

  if (FTotalOperacao > 0) then
    StatusOperacao := stsIniciada
  else
    StatusOperacao := stsLivre;
end;

procedure TFormPrincipal.btSerialClick(Sender: TObject);
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    frConfiguraSerial.Device.Porta        := ACBrPosPrinter1.Device.Porta ;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text ;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString ;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
       cbxPorta.Text := frConfiguraSerial.Device.Porta ;
       ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString ;
    end ;
  finally
     FreeAndNil( frConfiguraSerial ) ;
  end ;
end;

procedure TFormPrincipal.seTotalOperacaoChange(Sender: TObject);
begin
  CalcularTotalOperacao;
end;

procedure TFormPrincipal.seValorOperacaoChange(Sender: TObject);
begin
  seTotalDesconto.MaxValue := seValorOperacao.Value;
  CalcularTotalOperacao;
end;

procedure TFormPrincipal.CalcularTotalOperacao;
begin
  TotalOperacao := seValorOperacao.Value - seTotalDesconto.Value + seTotalAcrescimo.Value;
end;

procedure TFormPrincipal.PrepararNovaOperacao;
begin
  seValorOperacao.Value := 0;
  seTotalDesconto.Value := 0;
  seTotalAcrescimo.Value := 0;
  seValorOperacao.SetFocus;
  sgPagamentos.Clear;
  StatusOperacao := stsLivre;
end;

procedure TFormPrincipal.CancelarOperacao;
begin
  CancelarTodosPagamentos;
  PrepararNovaOperacao;
end;

procedure TFormPrincipal.CancelarTodosPagamentos;
begin
  {}
end;

procedure TFormPrincipal.Configurar;
begin
  ConfigurarPosPrinter;
  ConfigurarTEF;
end;

procedure TFormPrincipal.ConfigurarTEF;
begin
  ACBrTEFD1.AtivarGP(TACBrTEFDTipo(cbxGP.ItemIndex));
end;

procedure TFormPrincipal.ConfigurarPosPrinter;
begin
  ACBrPosPrinter1.Desativar;
  ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModeloPosPrinter.ItemIndex );
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo( cbxPagCodigo.ItemIndex );
  ACBrPosPrinter1.Porta := cbxPorta.Text;
  ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
  ACBrPosPrinter1.LinhasEntreCupons := seLinhasPular.Value;
  ACBrPosPrinter1.EspacoEntreLinhas := seEspLinhas.Value;
end;

end.


