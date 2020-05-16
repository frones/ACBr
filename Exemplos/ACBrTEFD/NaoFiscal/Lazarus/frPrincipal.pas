unit frPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Spin, Buttons, DBCtrls, ExtCtrls, PairSplitter, Grids, ACBrTEFD,
  ACBrPosPrinter, ACBrTEFDClass;

type

  TProximaOperacao = (popLivre, popCancelarOperacao, popIrParaPagamentos, popIniciarNovaOperacao);
  TStatusOperacao = (stsLivre, stsIniciada, stsEmPagamento, stsFinalizada);

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrTEFD1: TACBrTEFD;
    btAdministrativo: TBitBtn;
    btConfiguracoes: TBitBtn;
    btIncluirPagamentos: TBitBtn;
    btExcluirPagamento: TBitBtn;
    btLerParametros: TBitBtn;
    btOperacao: TBitBtn;
    btEfetuarPagamentos: TBitBtn;
    btOperacaoTEF: TBitBtn;
    btSalvarParametros: TBitBtn;
    btSerial: TSpeedButton;
    btTestarPosPrinter: TBitBtn;
    btTestarTEF: TBitBtn;
    cbAutoAtivar: TCheckBox;
    cbIMprimirViaReduzida: TCheckBox;
    cbMultiplosCartoes: TCheckBox;
    cbSimularErroNoDoctoFiscal: TCheckBox;
    cbSuportaDesconto: TCheckBox;
    cbSuportaReajusteValor: TCheckBox;
    cbSuportaSaque: TCheckBox;
    cbxGP: TComboBox;
    cbxModeloPosPrinter: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    edLog: TEdit;
    gbConfigImpressora: TGroupBox;
    gbConfigTEF: TGroupBox;
    gbPagamentos: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lSaidaImpressao: TLabel;
    mImpressao: TMemo;
    mLog: TMemo;
    pBotoesConfiguracao: TPanel;
    pBotaoOperacao: TPanel;
    pConfiguracao: TPanel;
    pLogs: TPanel;
    pLogMsgs: TPanel;
    pMensagem: TPanel;
    pMensagemCliente: TPanel;
    pMensagemOperador: TPanel;
    pBotoesPagamentos: TPanel;
    pImpressao: TPanel;
    pSimulador: TPanel;
    pStatus: TPanel;
    edTotalOperacao: TEdit;
    edTotalPago: TEdit;
    edTroco: TEdit;
    gbValorOperacao: TGroupBox;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ImageList1: TImageList;
    pOperacao: TPanel;
    pgPrincipal: TPageControl;
    SbArqLog: TSpeedButton;
    sbImprimir: TSpeedButton;
    seColunas: TSpinEdit;
    seEsperaSleep: TSpinEdit;
    seEsperaSTS: TSpinEdit;
    seEspLinhas: TSpinEdit;
    seLinhasPular: TSpinEdit;
    seMaxCartoes: TSpinEdit;
    seTotalAcrescimo: TFloatSpinEdit;
    seTotalDesconto: TFloatSpinEdit;
    seTrocoMaximo: TFloatSpinEdit;
    seValorOperacao: TFloatSpinEdit;
    sbLimparLog: TSpeedButton;
    sbLimparImpressora: TSpeedButton;
    Splitter1: TSplitter;
    sgPagamentos: TStringGrid;
    Splitter2: TSplitter;
    tsConfiguracao: TTabSheet;
    tsOperacao: TTabSheet;
    procedure ACBrTEFD1ComandaECF(Operacao: TACBrTEFDOperacaoECF;
      Resp: TACBrTEFDResp; var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFAbreVinculado(COO, IndiceECF: String;
      Valor: Double; var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFImprimeVia(
      TipoRelatorio: TACBrTEFDTipoRelatorio; Via: Integer;
      ImagemComprovante: TStringList; var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFPagamento(IndiceECF: String; Valor: Double;
      var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFSubtotaliza(DescAcre: Double;
      var RetornoECF: Integer);
    procedure ACBrTEFD1DepoisConfirmarTransacoes(
      RespostasPendentes: TACBrTEFDRespostasPendentes);
    procedure ACBrTEFD1ExibeMsg(Operacao: TACBrTEFDOperacaoMensagem;
      Mensagem: String; var AModalResult: TModalResult);
    procedure ACBrTEFD1GravarLog(const GP: TACBrTEFDTipo; ALogLine: String;
      var Tratado: Boolean);
    procedure ACBrTEFD1InfoECF(Operacao: TACBrTEFDInfoECF;
      var RetornoECF: String);
    procedure btEfetuarPagamentosClick(Sender: TObject);
    procedure btOperacaoClick(Sender: TObject);
    procedure btConfiguracoesClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btOperacaoTEFClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
    procedure btTestarPosPrinterClick(Sender: TObject);
    procedure btTestarTEFClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbImprimirClick(Sender: TObject);
    procedure sbLimparImpressoraClick(Sender: TObject);
    procedure sbLimparLogClick(Sender: TObject);
    procedure seTotalOperacaoChange(Sender: TObject);
    procedure seValorOperacaoChange(Sender: TObject);
  private
    FProximaOperacao: TProximaOperacao;
    FStatusOperacao: TStatusOperacao;
    FTotalOperacao: Double;
    FTotalPago: Double;

    function GetNomeArquivoConfiguracao: String;
    procedure SetProximaOperacao(AValue: TProximaOperacao);
    procedure SetStatusOperacao(AValue: TStatusOperacao);
    procedure SetTotalOperacao(AValue: Double);
    procedure SetTotalPago(AValue: Double);

    procedure TratarException(Sender : TObject; E : Exception);

  protected
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

    procedure ConfigurarTEF;
    procedure AtivarTEF;
    procedure ConfigurarPosPrinter;
    procedure AtivarPosPrinter;
    procedure Ativar;
    procedure Desativar;

    procedure CalcularTotalOperacao;
    procedure PrepararNovaOperacao;
    procedure CancelarOperacao;
    procedure CancelarTodosPagamentos;

    procedure FinalizarOperacao; // Em caso de Venda, Gere e transmita seu Documento Fiscal


    procedure AdicionarLinhaLog(AMensagem: String);
    procedure AdicionarLinhaImpressao(ALinha: String);
  public
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;

    property StatusOperacao: TStatusOperacao read FStatusOperacao write SetStatusOperacao;
    property ProximaOperacao: TProximaOperacao read FProximaOperacao write SetProximaOperacao;
    property TotalOperacao: Double read FTotalOperacao write SetTotalOperacao;
    property TotalPago: Double read FTotalPago write SetTotalPago;

  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  IniFiles, typinfo, dateutils,
  configuraserial,
  ACBrUtil, ACBrTEFComum;

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

  Application.OnException := @TratarException;
end;

procedure TFormPrincipal.sbImprimirClick(Sender: TObject);
begin
  ACBrPosPrinter1.Buffer.Assign(mImpressao.Lines);
  ACBrPosPrinter1.Imprimir;
end;

procedure TFormPrincipal.sbLimparImpressoraClick(Sender: TObject);
begin
  mImpressao.Lines.Clear;
end;

procedure TFormPrincipal.sbLimparLogClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TFormPrincipal.btOperacaoTEFClick(Sender: TObject);
begin
  AdicionarLinhaLog('- btOperacaoTEFClick');
  Ativar;
  pgPrincipal.ActivePage := tsOperacao;
  PrepararNovaOperacao;
end;

procedure TFormPrincipal.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TFormPrincipal.btConfiguracoesClick(Sender: TObject);
begin
  AdicionarLinhaLog('- btConfiguracoesClick');
  Desativar;
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

procedure TFormPrincipal.ACBrTEFD1ExibeMsg(Operacao: TACBrTEFDOperacaoMensagem;
  Mensagem: String; var AModalResult: TModalResult);
var
   Fim : TDateTime;
   OldMensagem : String;
begin
  case Operacao of

    opmOK :
       AModalResult := MessageDlg( Mensagem, mtInformation, [mbOK], 0);

    opmYesNo :
       AModalResult := MessageDlg( Mensagem, mtConfirmation, [mbYes, mbNo], 0);

    opmExibirMsgOperador, opmRemoverMsgOperador :
         pMensagemOperador.Caption := Mensagem ;

    opmExibirMsgCliente, opmRemoverMsgCliente :
         pMensagemCliente.Caption := Mensagem ;

    opmDestaqueVia :
       begin
         OldMensagem := pMensagemOperador.Caption ;
         try
            pMensagemOperador.Caption := Mensagem ;
            pMensagemOperador.Visible := True ;
            pMensagem.Visible         := True ;

            { Aguardando 3 segundos }
            Fim := IncSecond( now, 3)  ;
            repeat
               sleep(200) ;
               pMensagemOperador.Caption := Mensagem + ' ' + IntToStr(SecondsBetween(Fim,now));
               Application.ProcessMessages;
            until (now > Fim) ;
         finally
            pMensagemOperador.Caption := OldMensagem ;
         end;
       end;
  end;

  pMensagemOperador.Visible := (pMensagemOperador.Caption <> '') ;
  pMensagemCliente.Visible  := (pMensagemCliente.Caption <> '') ;

  pMensagem.Visible := pMensagemOperador.Visible or pMensagemCliente.Visible;
  Application.ProcessMessages;
end;

procedure TFormPrincipal.ACBrTEFD1DepoisConfirmarTransacoes(
  RespostasPendentes: TACBrTEFDRespostasPendentes);
var
  I : Integer;
begin
  for I := 0 to RespostasPendentes.Count-1  do
  begin
     with RespostasPendentes[I] do
     begin
        AdicionarLinhaLog('Confirmado: '+Header+' ID: '+IntToStr( ID ) );

        // Lendo os campos mapeados //
        AdicionarLinhaLog('- Rede: '  + Rede + ', NSU: '  + NSU );
        AdicionarLinhaLog('- Parcelas: '+ IntToStr(QtdParcelas) +
                          ', parcelado por: '+ GetEnumName(TypeInfo(TACBrTEFRespParceladoPor), integer(ParceladoPor) ));
        AdicionarLinhaLog('- É Débito: '+BoolToStr(Debito)+
                          ', É Crédito: '+BoolToStr(Credito)+
                          ', Valor: '+ FormatFloat('###,###,##0.00',ValorTotal)) ;

        // Lendo um Campo Específico //
        AdicionarLinhaLog('- Campo 11: ' + LeInformacao(11,0).AsString );
     end;
  end;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECF(Operacao: TACBrTEFDOperacaoECF;
  Resp: TACBrTEFDResp; var RetornoECF: Integer);
begin
  AdicionarLinhaLog('ACBrTEFD1ComandaECF: '+GetEnumName( TypeInfo(TACBrTEFDOperacaoECF), integer(Operacao) ));

  try
    case Operacao of
      opeAbreGerencial:
        AdicionarLinhaImpressao('</zera>');

      opeSubTotalizaCupom:
        begin
          if StatusOperacao = stsIniciada then
            StatusOperacao := stsEmPagamento;
        end;

      opeCancelaCupom:
         CancelarOperacao;

      opeFechaCupom:
         FinalizarOperacao;

      opePulaLinhas, opeFechaGerencial, opeFechaVinculado:
        begin
          AdicionarLinhaImpressao('</pular_linhas>');
          AdicionarLinhaImpressao('</corte>');
        end;
    end;

    RetornoECF := 1 ;
  except
    RetornoECF := 0 ;
  end;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFAbreVinculado(COO,
  IndiceECF: String; Valor: Double; var RetornoECF: Integer);
begin
  AdicionarLinhaLog( 'ACBrTEFD1ComandaECFAbreVinculado, COO:'+COO+
     ' IndiceECF: '+IndiceECF+' Valor: '+FormatFloatBr(Valor) ) ;
  AdicionarLinhaImpressao('</zera>');
  AdicionarLinhaImpressao('</linha_dupla>');
  RetornoECF := 1;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFImprimeVia(
  TipoRelatorio: TACBrTEFDTipoRelatorio; Via: Integer;
  ImagemComprovante: TStringList; var RetornoECF: Integer);
begin
  AdicionarLinhaLog( 'ACBrTEFD1ComandaECFImprimeVia: '+IntToStr(Via) );
  mImpressao.Lines.AddStrings( ImagemComprovante );
  RetornoECF := 1 ;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFPagamento(IndiceECF: String;
  Valor: Double; var RetornoECF: Integer);
begin
  AdicionarLinhaLog('ACBrTEFD1ComandaECFPagamento, IndiceECF: '+IndiceECF+' Valor: '+FormatFloatBr(Valor));
  RetornoECF := 1;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFSubtotaliza(DescAcre: Double;
  var RetornoECF: Integer);
begin
  AdicionarLinhaLog('ACBrTEFD1ComandaECFSubtotaliza: DescAcre: ' + FormatFloatBr(DescAcre));
  if StatusOperacao = stsIniciada then
    StatusOperacao := stsEmPagamento;
end;

procedure TFormPrincipal.ACBrTEFD1GravarLog(const GP: TACBrTEFDTipo;
  ALogLine: String; var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TFormPrincipal.ACBrTEFD1InfoECF(Operacao: TACBrTEFDInfoECF;
  var RetornoECF: String);
begin
   //try
   //   if not ACBrECF1.Ativo then
   //      ACBrECF1.Ativar ;
   //except
   //   { Para CliSiTEF ou V&SPague aplique o IF abaixo em sua aplicação, que
   //     permite saber se o Cupom foi concluido mesmo com o ECF desligado }
   //
   //   if (not ACBrTEFD1.TEF.Inicializado) and   { Está na inicialização ? }
   //      (Operacao = ineEstadoECF) and          { Quer Saber o estado do ECF ? (mas se chegou aqui é pq o ECF já está com problemas) }
   //      (ACBrTEFD1.GPAtual in [gpCliSiTef,gpVeSPague]) then
   //   begin
   //      { Leia o último Documento Gravado no seu Banco de Dados, e verifique
   //        se o Cupom já foi finalizado,ou se já foi aberto um CCD ou Gerencial...
   //        Exemplo:
   //
   //        Documento.Le(0);
   //
   //        if (Documento.Finalizado) or (pos(Documento.Denominacao,'CC|RG') > 0) then
   //           RetornoECF := 'R'
   //        else
   //           RetornoECF := 'O' ;
   //      }
   //
   //      //RetornoECF := 'O';    // Executará CancelarTransacoesPendentes;
   //      RetornoECF := 'R';    // Executará ConfirmarESolicitarImpressaoTransacoesPendentes;
   //      exit ;
   //   end ;
   //
   //   raise ;
   //end;

   case Operacao of
     ineSubTotal :
       RetornoECF := FloatToStr( TotalOperacao - TotalPago ) ;

     ineTotalAPagar :
       RetornoECF := FloatToStr( TotalPago ) ;

     ineEstadoECF :
       begin
         Case StatusOperacao of
           stsLivre: RetornoECF := 'L' ;
           stsIniciada: RetornoECF := 'V' ;
           stsEmPagamento: RetornoECF := 'P' ;
         else
           RetornoECF := 'O' ;
         end;
       end;
   end;
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
  AdicionarLinhaLog('- LerConfiguracao');

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
  AdicionarLinhaLog('- GravarConfiguracao');

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
    popCancelarOperacao, popIrParaPagamentos:
      MsgOperacao := 'Cancelar Operacao';

    popIniciarNovaOperacao:
      MsgOperacao := 'Nova Operacao';
  end;

  FProximaOperacao := AValue;

  btEfetuarPagamentos.Enabled := (FProximaOperacao in [popIrParaPagamentos]);
  btOperacao.Visible := (MsgOperacao <> '');
  btOperacao.Caption := MsgOperacao;
end;

procedure TFormPrincipal.SetStatusOperacao(AValue: TStatusOperacao);
var
  MsgStatus: String;
begin
  if FStatusOperacao = AValue then Exit;
  AdicionarLinhaLog('- StatusOperacao: '+GetEnumName(TypeInfo(TStatusOperacao), integer(AValue) ));

  gbValorOperacao.Enabled := (AValue in [stsLivre, stsIniciada]);
  gbPagamentos.Enabled := (AValue = stsEmPagamento);
  btAdministrativo.Enabled := (AValue = stsLivre);
  btConfiguracoes.Enabled := (AValue = stsLivre);

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

    stsFinalizada:
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

procedure TFormPrincipal.SetTotalPago(AValue: Double);
begin
  edTotalPago.Text := FormatFloatBr(AValue);
  FTotalPago := AValue;
end;

procedure TFormPrincipal.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog(E.ClassName);
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');
end;

procedure TFormPrincipal.btSerialClick(Sender: TObject);
begin
  AdicionarLinhaLog('- btSerialClick');
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

procedure TFormPrincipal.btTestarPosPrinterClick(Sender: TObject);
begin
  AdicionarLinhaLog('- btTestarPosPrinterClick');
  try
    AtivarPosPrinter;

    ACBrPosPrinter1.Buffer.Clear;
    with ACBrPosPrinter1.Buffer do
    begin
      Add('</zera>');
      Add('</linha_dupla>');
      Add('FONTE NORMAL: '+IntToStr(ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
      Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteNormal));
      Add('<e>EXPANDIDO: '+IntToStr(ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
      Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteExpandida));
      Add('</e><c>CONDENSADO: '+IntToStr(ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
      Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteCondensada));
      Add('</c><n>FONTE NEGRITO</N>');
      Add('<in>FONTE INVERTIDA</in>');
      Add('<S>FONTE SUBLINHADA</s>');
      Add('<i>FONTE ITALICO</i>');
      Add('FONTE NORMAL');
      Add('</corte_total>');
    end;

    ACBrPosPrinter1.Imprimir;
  except
    On E: Exception do
    begin
      MessageDlg('Falha ao ativar a Impressora' + sLineBreak + E.Message, mtError, [mbOK], 0);
    end;
  end
end;

procedure TFormPrincipal.btTestarTEFClick(Sender: TObject);
var
  NomeTEF: String;
begin
  NomeTEF := GetEnumName(TypeInfo(TACBrTEFDTipo), cbxGP.ItemIndex);
  AdicionarLinhaLog('- btTestarTEFClick: '+NomeTEF);
  try
    AtivarTEF;
    MessageDlg(Format('TEF %S ATIVO', [NomeTEF]), mtInformation, [mbOK], 0);
  except
    On E: Exception do
    begin
      MessageDlg(Format('Falha ao ativar TEF %S' + sLineBreak + E.Message, [NomeTEF]), mtError, [mbOK], 0);
    end;
  end;
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
  AdicionarLinhaLog('- PrepararNovaOperacao');
  seValorOperacao.Value := 0;
  seTotalDesconto.Value := 0;
  seTotalAcrescimo.Value := 0;
  seValorOperacao.SetFocus;
  sgPagamentos.Clear;
  StatusOperacao := stsLivre;
end;

procedure TFormPrincipal.CancelarOperacao;
begin
  AdicionarLinhaLog('- CancelarOperacao');
  CancelarTodosPagamentos;
  PrepararNovaOperacao;
end;

procedure TFormPrincipal.CancelarTodosPagamentos;
begin
  AdicionarLinhaLog('- CancelarTodosPagamentos');
end;

procedure TFormPrincipal.FinalizarOperacao;
begin
  // AQUI você deve Gerar e Transmitir o Documento Fiscal (NFCe ou SAT)

  AdicionarLinhaImpressao('--- COMPROVANTE ---');
  //TODO
end;

procedure TFormPrincipal.ConfigurarTEF;
begin
  AdicionarLinhaLog('- ConfigurarTEF');
  ACBrTEFD1.ArqLOG := edLog.Text;
  ACBrTEFD1.EsperaSleep := seEsperaSleep.Value;
  ACBrTEFD1.EsperaSTS := seEsperaSTS.Value;
  ACBrTEFD1.TrocoMaximo := seTrocoMaximo.Value;
  ACBrTEFD1.AutoAtivarGP := cbAutoAtivar.Checked;
  ACBrTEFD1.ImprimirViaClienteReduzida := cbImprimirViaReduzida.Checked;
  ACBrTEFD1.MultiplosCartoes := cbMultiplosCartoes.Checked;
  ACBrTEFD1.NumeroMaximoCartoes := seMaxCartoes.Value;
  ACBrTEFD1.SuportaDesconto := cbSuportaDesconto.Checked;
  ACBrTEFD1.SuportaReajusteValor := cbSuportaReajusteValor.Checked;
  ACBrTEFD1.SuportaSaque := cbSuportaSaque.Checked;

  // Configurações abaixo são obrigatórios, para funcionamento de Não Fiscal //
  ACBrTEFD1.AutoEfetuarPagamento := False;
  ACBrTEFD1.AutoFinalizarCupom := False;
end;

procedure TFormPrincipal.AtivarTEF;
begin
  AdicionarLinhaLog('- AtivarTEF');
  ConfigurarTEF;
  ACBrTEFD1.Inicializar(TACBrTEFDTipo(cbxGP.ItemIndex));
  ACBrTEFD1.ATV;
end;

procedure TFormPrincipal.ConfigurarPosPrinter;
begin
  AdicionarLinhaLog('- ConfigurarPosPrinter');
  ACBrPosPrinter1.Desativar;
  ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModeloPosPrinter.ItemIndex );
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo( cbxPagCodigo.ItemIndex );
  ACBrPosPrinter1.Porta := cbxPorta.Text;
  ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
  ACBrPosPrinter1.LinhasEntreCupons := seLinhasPular.Value;
  ACBrPosPrinter1.EspacoEntreLinhas := seEspLinhas.Value;
end;

procedure TFormPrincipal.AtivarPosPrinter;
begin
  AdicionarLinhaLog('- AtivarPosPrinter');
  ConfigurarPosPrinter;
  if (ACBrPosPrinter1.Porta <> '') then
  begin
    ACBrPosPrinter1.Ativar;
    sbImprimir.Enabled := ACBrPosPrinter1.Ativo;
  end
  else
    raise Exception.Create('Porta não definida');
end;

procedure TFormPrincipal.Ativar;
begin
  AdicionarLinhaLog('- Ativar');
  AtivarTEF;
  try
    AtivarPosPrinter;
  except
    On E: Exception do
    begin
      TratarException(nil, E);
      sbImprimir.Enabled := False;
    end;
  end;
end;

procedure TFormPrincipal.Desativar;
begin
  AdicionarLinhaLog('- Desativar');
  ACBrPosPrinter1.Desativar;
  ACBrTEFD1.DesInicializar(TACBrTEFDTipo(cbxGP.ItemIndex));
end;

end.


