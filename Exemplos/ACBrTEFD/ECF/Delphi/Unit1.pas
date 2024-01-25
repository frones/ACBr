{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit Unit1;

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ACBrECF, ACBrDevice, ACBrTEFD,
  ACBrTEFDClass, ACBrUtil.Base, ACBrUtil.Strings, ACBrTEFDCliSiTef, ACBrBase;

type

  { TForm1 }

  TForm1 = class(TForm)
     ACBrECF1 : TACBrECF;
     ACBrTEFD1 : TACBrTEFD;
     bAbreVendeSubTotaliza1 : TButton;
     bAbreVendeSubTotaliza2 : TButton;
     bAbreVendeSubTotaliza3 : TButton;
     bAbreVendeSubTotaliza4 : TButton;
     bADM : TButton;
     bAtivar : TButton;
     bFechaRelatorio : TButton;
     bFPG : TButton;
     bAtivarGP : TButton;
     bATV : TButton;
     bAbreVendeSubTotaliza : TButton;
     bCNC : TButton;
     bCNF : TButton;
     bCRT : TButton;
     bCHQ : TButton;
     bFechar : TButton;
     bInicializar : TButton;
     bLeituraX : TButton;
     bNCN : TButton;
     bReducaoZ : TButton;
     bPagamento: TButton;
     btSerial : TSpeedButton;
     bCancelarResp : TButton;
     bVendeItem : TButton;
     bSubTotaliza : TButton;
     bCancelar : TButton;
     bAbreCupom : TButton;
     bEstado : TButton;
     cbxGP : TComboBox;
     cbxGP1 : TComboBox;
     cbxModelo : TComboBox;
     cbxPorta : TComboBox;
     ckAutoAtivar : TCheckBox;
     ckAutoEfetuarPagamento : TCheckBox;
     ckAutoFinalizarCupom : TCheckBox;
     ckCHQemGerencial: TCheckBox;
     ckVSPague : TCheckBox ;
     ckHIPERTEF : TCheckBox;
     ckMultiplosCartoes : TCheckBox;
     ckMultiplosCartoes1 : TCheckBox;
     ckTEFDIAL : TCheckBox;
     ckTEFDISC : TCheckBox;
     ComboBox1 : TComboBox ;
     edEsperaSTS : TEdit;
     edFPGCartao : TEdit;
     edFPGCheque : TEdit;
     edEsperaSleep : TEdit;
     edValorECF : TEdit;
     edValorTEF : TEdit;
     gbComandosTEF : TGroupBox;
     gbConfigECF : TGroupBox;
     gbConfigTEF : TGroupBox;
     gbCupomECF : TGroupBox;
     GroupBox1 : TGroupBox ;
     Label1 : TLabel;
     Label10 : TLabel;
     Label11 : TLabel;
     Label12: TLabel;
     lMensagemCliente : TLabel ;
     Label2 : TLabel;
     Label3 : TLabel;
     Label4 : TLabel;
     Label5 : TLabel;
     Label6 : TLabel;
     Label7 : TLabel;
     Label8 : TLabel;
     Label9 : TLabel;
     lECFName : TLabel;
     lMensagemOperador : TLabel ;
     Memo1: TMemo;
     mPagamentos: TMemo;
     PageControl1 : TPageControl;
     Panel1 : TPanel;
     Panel2 : TPanel;
     Panel3: TPanel;
    pnlPagamentosAFazer: TPanel;
     Panel5: TPanel;
     pMensagemOperador : TPanel;
     pMensagemCliente : TPanel;
     pMensagem : TPanel;
     spbAdicionaPagamento: TSpeedButton;
     spbAdicionaPagamento1: TSpeedButton;
     spRemovePagamento: TSpeedButton;
     spbLimpaPagamentos: TSpeedButton;
     Splitter1: TSplitter;
     sVSPague : TShape ;
     sECF : TShape;
     sHiperTEF : TShape;
     StatusBar1 : TStatusBar;
     sTEFDial : TShape;
     sTEFDisc : TShape;
     tsConfig : TTabSheet;
     tsOperacao : TTabSheet;
     sCliSiTef: TShape;
     ckCliSiTef: TCheckBox;
    ckAuttar: TCheckBox;
    sAuttar: TShape;
    lMeuAcresDesc: TLabel;
    edValorDescAcre: TEdit;
    ckViaClienteReduzida: TCheckBox;
     procedure ACBrECF1MsgPoucoPapel(Sender : TObject) ;
     procedure ACBrTEFD1AguardaResp(Arquivo : String;
        SegundosTimeOut : Integer; var Interromper : Boolean);
     procedure ACBrTEFD1AntesCancelarTransacao(RespostaPendente: TACBrTEFDResp);{%h-}
     procedure ACBrTEFD1AntesFinalizarRequisicao(Req : TACBrTEFDReq);
     procedure ACBrTEFD1BloqueiaMouseTeclado(Bloqueia : Boolean;
        var Tratado : Boolean);
     procedure ACBrTEFD1ComandaECF(Operacao : TACBrTEFDOperacaoECF;
        Resp : TACBrTEFDResp; var RetornoECF : Integer );{%h-}
     procedure ACBrTEFD1ComandaECFAbreVinculado(COO, IndiceECF : String;
        Valor : Double; var RetornoECF : Integer);
     procedure ACBrTEFD1ComandaECFImprimeVia(
        TipoRelatorio : TACBrTEFDTipoRelatorio; Via : Integer;
        ImagemComprovante : TStringList; var RetornoECF : Integer);
     procedure ACBrTEFD1ComandaECFPagamento(IndiceECF : String; Valor : Double;
        var RetornoECF : Integer);
     procedure ACBrTEFD1ComandaECFSubtotaliza(DescAcre: Double;
        var RetornoECF: Integer);
     procedure ACBrTEFD1DepoisConfirmarTransacoes(
        RespostasPendentes: TACBrTEFDRespostasPendentes);
     procedure ACBrTEFD1ExibeMsg(Operacao : TACBrTEFDOperacaoMensagem;
        Mensagem : String; var AModalResult : TModalResult);
     procedure ACBrTEFD1InfoECF(Operacao : TACBrTEFDInfoECF;
        var RetornoECF : String );
     procedure ACBrTEFD1MudaEstadoReq(EstadoReq : TACBrTEFDReqEstado);
     procedure ACBrTEFD1MudaEstadoResp(EstadoResp : TACBrTEFDRespEstado);
     procedure ACBrTEFD1RestauraFocoAplicacao(var Tratado : Boolean);
     procedure bAbreVendeSubTotaliza1Click(Sender : TObject);
     procedure bAbreVendeSubTotaliza2Click(Sender : TObject);
     procedure bAbreVendeSubTotaliza3Click(Sender : TObject);
     procedure bAbreVendeSubTotaliza4Click(Sender : TObject);
     procedure BaneseObtemInformacao(var ItemSelecionado : Integer);
     procedure bCancelarRespClick(Sender : TObject);
     procedure bPagamentoClick(Sender: TObject);
     procedure cbxGPChange(Sender : TObject);
     procedure ckCHQemGerencialChange(Sender: TObject);
     procedure ckAuttarChange(Sender : TObject) ;
     procedure ckCliSiTefChange(Sender : TObject);
     procedure ckVSPagueChange(Sender : TObject) ;
     procedure CliDTEFExibeMenu(Titulo : String ; Opcoes : TStringList ;
        var ItemSelecionado : Integer ; var VoltarMenu : Boolean) ;
     procedure CliDTEFObtemInformacao(var ItemSelecionado : Integer) ;
     procedure edEsperaSleepChange(Sender : TObject);
     procedure edEsperaSTSChange(Sender : TObject);
     procedure pMensagemOperadorClick(Sender: TObject);
     procedure pMensagemResize(Sender : TObject);
     procedure spbAdicionaPagamento1Click(Sender: TObject);
     procedure spbAdicionaPagamentoClick(Sender: TObject);
     procedure spbLimpaPagamentosClick(Sender: TObject);
     procedure spRemovePagamentoClick(Sender: TObject);
     procedure TrataErros(Sender : TObject; E : Exception);
     procedure bAbreVendeSubTotalizaClick(Sender : TObject);
     procedure bCHQClick(Sender : TObject);
     procedure bCNCClick(Sender : TObject);
     procedure bCNFClick(Sender : TObject);
     procedure bCRTClick(Sender : TObject);
     procedure bEstadoClick(Sender : TObject);
     procedure bFecharClick(Sender : TObject);
     procedure bFPGClick(Sender : TObject);
     procedure bNCNClick(Sender : TObject);
     procedure bSubTotalizaClick(Sender : TObject);
     procedure bVendeItemClick(Sender : TObject);
     procedure bAbreCupomClick(Sender : TObject);
     procedure bADMClick(Sender : TObject);
     procedure bAtivarGPClick(Sender : TObject);
     procedure bATVClick(Sender : TObject);
     procedure bInicializarClick(Sender : TObject);
     procedure bAtivarClick(Sender : TObject);
     procedure btSerialClick(Sender : TObject);
     procedure bReducaoZClick(Sender : TObject);
     procedure bLeituraXClick(Sender : TObject);
     procedure bFechaRelatorioClick(Sender : TObject);
     procedure bCancelarClick(Sender : TObject);
     procedure cbxModeloChange(Sender : TObject);
     procedure cbxPortaChange(Sender : TObject);
     procedure ckAutoEfetuarPagamentoChange(Sender : TObject);
     procedure ckAutoFinalizarCupomChange(Sender : TObject);
     procedure ckMultiplosCartoesChange(Sender : TObject);
     procedure ckAutoAtivarChange(Sender : TObject);
     procedure ckHIPERTEFChange(Sender : TObject);
     procedure ckTEFDISCChange(Sender : TObject);
     procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
     procedure FormCreate(Sender : TObject);
     procedure ckTEFDIALChange(Sender : TObject);
     procedure Memo1Change(Sender : TObject);

     procedure ACBrTEFD1BaneseObtemOpcaoAdm(var opcao: Integer);
     procedure ACBrTEFD1BaneseObtemInformacao(var ItemSelecionado: Integer);
    procedure ACBrTEFD1VeSPagueExibeMenu(Titulo: string; Opcoes,
      Memo: TStringList; var ItemSelecionado: Integer);
    procedure ACBrTEFD1VeSPagueObtemCampo(Titulo, Mascara: string;
      Tipo: AnsiChar; var Resposta: string; var Digitado: Boolean);
    procedure ACBrTEFD1CliSiTefExibeMenu(Titulo: string; Opcoes: TStringList;
      var ItemSelecionado: Integer; var VoltarMenu: Boolean);
    procedure ACBrTEFD1CliSiTefObtemCampo(Titulo: String; TamanhoMinimo,
      TamanhoMaximo, TipoCampo: Integer;
      Operacao: TACBrTEFDCliSiTefOperacaoCampo; var Resposta: AnsiString;
      var Digitado, VoltarMenu: Boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure ckViaClienteReduzidaClick(Sender: TObject);
  private
     fCancelado : Boolean ;

     procedure AvaliaTEFs;
     procedure MostraSaldoRestante;
     procedure VerificaECFAtivo;

     Function CalculaTotalPago : Double ;
     Function CalculaSaldoRestante : Double ;
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1 : TForm1;

implementation

Uses typinfo, dateutils, strutils, ConfiguraSerial, Unit2, Unit3, Unit4, Unit5,
     Unit6, Unit7;

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender : TObject);
var
   I : TACBrTEFDTipo;
   J : TACBrECFModelo;
begin
  fCancelado := False ;
  Application.OnException := TrataErros;

  cbxModelo.Items.Clear ;
  For J := Low(TACBrECFModelo) to High(TACBrECFModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrECFModelo), integer(J) ) ) ;
  cbxModelo.Items[0] := 'Procurar' ;
  cbxModelo.ItemIndex := 0 ;

  cbxGP.Items.Clear ;
  For I := Low(TACBrTEFDTipo) to High(TACBrTEFDTipo) do
     cbxGP.Items.Add( GetEnumName(TypeInfo(TACBrTEFDTipo), integer(I) ) ) ;
  cbxGP.Items[0] := 'Todos' ;
  cbxGP.ItemIndex := 0 ;

  cbxGP1.Items.Assign(cbxGP.Items);
  cbxGP1.ItemIndex := 0 ;

  PageControl1.ActivePageIndex := 0 ;
  Memo1.Lines.Clear;

  ACBrTEFD1.TEFCliDTEF.NumVias := 1;
  ACBrTEFD1.TEFCliDTEF.NumeroTerminal := '001';
  ACBrTEFD1.TEFCliDTEF.ArqResp := 'C:\DPOS3x25\CUPONS\';

  pMensagem.Visible := False ;
  pMensagem.Align   := alClient ;

  ACBrTEFD1.TEFCliSiTef.OnObtemCampo := ACBrTEFD1CliSiTefObtemCampo;
end;

procedure TForm1.ckTEFDIALChange(Sender : TObject);
begin
   ACBrTEFD1.TEFDial.Habilitado := ckTEFDIAL.Checked;
end;

procedure TForm1.Memo1Change(Sender : TObject);
begin
  StatusBar1.Panels[0].Text := GetEnumName(TypeInfo(TACBrTEFDTipo), integer(ACBrTEFD1.GPAtual) ) ;
  StatusBar1.Panels[1].Text := '' ;
  StatusBar1.Panels[2].Text := '' ;
end;

procedure TForm1.AvaliaTEFs;
begin
  if ACBrTEFD1.TEFDial.Inicializado then
     sTEFDial.Brush.Color := clLime
  else
     sTEFDial.Brush.Color := clRed ;
  ckTEFDIAL.Checked := ACBrTEFD1.TEFDial.Habilitado;

  if ACBrTEFD1.TEFDisc.Inicializado then
     sTEFDisc.Brush.Color := clLime
  else
     sTEFDisc.Brush.Color := clRed ;
  ckTEFDISC.Checked := ACBrTEFD1.TEFDisc.Habilitado;

  if ACBrTEFD1.TEFHiper.Inicializado then
     sHiperTEF.Brush.Color := clLime
  else
     sHiperTEF.Brush.Color := clRed ;
  ckHIPERTEF.Checked := ACBrTEFD1.TEFHiper.Habilitado;

  if ACBrTEFD1.TEFCliSiTef.Inicializado then
     sCliSiTef.Brush.Color := clLime
  else
     sCliSiTef.Brush.Color := clRed ;
  ckCliSiTef.Checked := ACBrTEFD1.TEFCliSiTef.Habilitado;

  if ACBrTEFD1.TEFVeSPague.Inicializado then
     sVSPague.Brush.Color := clLime
  else
     sVSPague.Brush.Color := clRed ;
  ckVSPague.Checked := ACBrTEFD1.TEFVeSPague.Habilitado;


  if ACBrTEFD1.TEFAuttar.Inicializado then
     sAuttar.Brush.Color := clLime
  else
     sAuttar.Brush.Color := clRed ;
  ckAuttar.Checked := ACBrTEFD1.TEFAuttar.Habilitado;

  cbxGP.ItemIndex  := Integer( ACBrTEFD1.GPAtual ) ;
  cbxGP1.ItemIndex := cbxGP.ItemIndex ;
end;

procedure TForm1.MostraSaldoRestante;
begin
  Memo1.Lines.Add( 'Saldo Restante: '+FormatFloat('0.00',CalculaSaldoRestante)) ;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  pnlPagamentosAFazer.Visible := PageControl1.ActivePageIndex = 1;
  Splitter1.Visible := PageControl1.ActivePageIndex = 1;
end;

procedure TForm1.VerificaECFAtivo;
begin
   if not ACBrECF1.Ativo then
      Memo1.Lines.Add( ACBrStr( 'ATENÇÃO !! O ECF AINDA NÃO FOI ATIVADO' ) );
end;

function TForm1.CalculaTotalPago: Double;
var
   I: Integer;
   Linha: String;
   Valor: Double;
begin
  Result := 0 ;

  { Adicionando valores de Pagamentos a Fazer }
  For I := 0 to mPagamentos.Lines.Count-1 do
  begin
     Linha  := mPagamentos.Lines[I];
     Valor  := StringToFloatDef( copy( Linha, Pos('|',Linha)+1, Length(Linha)), 0 );
     Result := Result + Valor;
  end;
end;

function TForm1.CalculaSaldoRestante: Double;
begin
  // Lendo Valor do Saldo do ECF //
  Result := ACBrECF1.Subtotal + StringToFloatDef(edValorDescAcre.Text, 0);
  Result := Result - ACBrECF1.TotalPago ;
  Result := Result - CalculaTotalPago  ;

  { TEFD ainda não imprimiu os pagamentos ? Então adicionando o Total das
    Transaçoes TEF já efetuadas no TotalPago}
  if not ACBrTEFD1.AutoEfetuarPagamento then
     Result := Result - ACBrTEFD1.RespostasPendentes.TotalPago;
end;

procedure TForm1.bInicializarClick(Sender : TObject);
begin
  if bInicializar.Caption = 'Inicializar' then
   begin
     Memo1.Lines.Add('Inicializando: ' + cbxGP.Text );
     ACBrTEFD1.Inicializar( TACBrTEFDTipo( cbxGP.ItemIndex ) );
     Memo1.Lines.Add('ACBrTEFD.Inicializar Executado' );
     bInicializar.Caption := 'DesInicializar' ;
   end
  else
   begin
     ACBrTEFD1.DesInicializar( TACBrTEFDTipo( cbxGP.ItemIndex ) );
     Memo1.Lines.Add('ACBrTEFD.DesInicializar Executado' );
     bInicializar.Caption := 'Inicializar' ;
   end;

  AvaliaTEFs;
end;

procedure TForm1.bAtivarGPClick(Sender : TObject);
begin
  Memo1.Lines.Add('Ativando GP: ' + cbxGP.Text );
  ACBrTEFD1.AtivarGP( TACBrTEFDTipo( cbxGP.ItemIndex ) );
  Memo1.Lines.Add('ACBrTEFD.AtivarGP Executado' );

  AvaliaTEFs;
end;

procedure TForm1.bAtivarClick(Sender : TObject);
begin
  if bAtivar.Caption = 'Ativar' then
   begin
     try
        ACBrECF1.Porta := cbxPorta.Text ;

        if cbxModelo.ItemIndex = 0 then
           if not ACBrECF1.AcharECF(true,False) then
           begin
              MessageDlg('Nenhum ECF encontrado.',mtInformation,[mbOk],0) ;
              exit ;
           end ;

        ACBrECF1.Ativar ;

        Memo1.Lines.Add( 'Ativar ECF' );
     finally
        cbxModelo.ItemIndex := Integer(ACBrECF1.Modelo) ;
        cbxPorta.Text       := ACBrECF1.Porta ;
        lECFName.Caption    := GetEnumName(TypeInfo(TACBrECFModelo), cbxModelo.ItemIndex ) ;
        if ACBrECF1.Ativo then
         begin
           sECF.Brush.Color := clLime ;
           bAtivar.Caption := 'Desativar' ;
         end
        else
           sECF.Brush.Color := clRed;

        btSerial.Enabled   := not ACBrECF1.Ativo ;
        gbCupomECF.Enabled := ACBrECF1.Ativo ;
     end ;
   end
  else
   begin
     ACBrECF1.Desativar ;
     bAtivar.Caption := 'Ativar' ;
     Memo1.Lines.Add( 'Desativar ECF' );
     sECF.Brush.Color   := clRed;
     gbCupomECF.Enabled := False ;
     btSerial.Enabled   := True ;
   end;
end;

procedure TForm1.btSerialClick(Sender : TObject);
Var
  frConfiguraSerial : TfrConfiguraSerial ;
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    frConfiguraSerial.Device.Porta        := ACBrECF1.Device.Porta ;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text ;
    frConfiguraSerial.Device.ParamsString := ACBrECF1.Device.ParamsString ;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
       cbxPorta.Text                := frConfiguraSerial.Device.Porta ;
       ACBrECF1.Device.ParamsString := frConfiguraSerial.Device.ParamsString ;
    end ;
  finally
     FreeAndNil( frConfiguraSerial ) ;
  end ;
end;

procedure TForm1.bReducaoZClick(Sender : TObject);
begin
  if ACBrECF1.Estado <> estRequerZ then
  begin
     if MessageDlg( ACBrStr( 'A Redução Z pode Bloquear o seu ECF até a 12:00pm'+#10+#10+
                  'Continua assim mesmo ?'),mtWarning,mbYesNoCancel,0) <> mrYes then
        exit ;

     if MessageDlg( ACBrStr('Você tem certeza ?'),mtWarning,mbYesNoCancel,0) <> mrYes then
        exit ;
  end ;

   self.Enabled := False ;
   try
      ACBrECF1.ReducaoZ ;
      Memo1.Lines.Add('ACBrECF.ReducaoZ');
   finally
     self.Enabled := True ;
   end;
end;

procedure TForm1.bLeituraXClick(Sender : TObject);
begin
  self.Enabled := False ;
  try
    ACBrECF1.LeituraX;
    Memo1.Lines.Add('ACBrECF.LeituraX');
  finally
    self.Enabled := True ;
  end;
end;

procedure TForm1.bFechaRelatorioClick(Sender : TObject);
begin
   ACBrECF1.FechaRelatorio;
   Memo1.Lines.Add('ACBrECF.FechaRelatorio');
end;

procedure TForm1.bAbreCupomClick(Sender : TObject);
begin
  ACBrECF1.AbreCupom;
  Memo1.Lines.Add('ACBrECF.AbreCupom');
  spbLimpaPagamentos.Click;
end;

procedure TForm1.bVendeItemClick(Sender : TObject);
Var
  Valor : Double ;
begin
  Valor := StringToFloatDef(edValorECF.Text, 0);

  try
    bVendeItem.Enabled := False ;
    ACBrECF1.VendeItem('12345','PRODUTO TESTE','NN',1,Valor);
    Memo1.Lines.Add('ACBrECF.VendeItem');
  finally
    bVendeItem.Enabled := True ;
  end;
end;

procedure TForm1.bSubTotalizaClick(Sender : TObject);
begin
  ACBrECF1.SubtotalizaCupom ;
  Memo1.Lines.Add('ACBrECF.SubtotalizaCupom');
  MostraSaldoRestante;
end;

procedure TForm1.bFecharClick(Sender : TObject);
begin
   ACBrECF1.FechaCupom('Projeto ACBr|http://acbr.sf.net');
   Memo1.Lines.Add('ACBrECF.FechaCupom');
end;

procedure TForm1.bFPGClick(Sender : TObject);
Var
  A : Integer ;
begin
  ACBrECF1.CarregaFormasPagamento ;

  for A := 0 to ACBrECF1.FormasPagamento.Count -1 do
  begin
     if ACBrECF1.FormasPagamento[A].Descricao <> '' then
        Memo1.Lines.Add( 'Forma Pagto: '+ACBrECF1.FormasPagamento[A].Indice+' -> '+
           ACBrECF1.FormasPagamento[A].Descricao+'  Permite Vinculado: '+
           IfThen( ACBrECF1.FormasPagamento[A].PermiteVinculado,'S','N'));
  end ;
  Memo1.Lines.Add('---------------------------------');
end;

procedure TForm1.bNCNClick(Sender : TObject);
Var
  AForm : TForm3 ;
begin
  VerificaECFAtivo;

  AForm := TForm3.Create(self);

  try
    AForm.IsNCN := True ;
    if AForm.ShowModal = mrOK then
    begin
      Memo1.Lines.Add( ACBrStr('Inicio de NCN - Rede: '+
                       AForm.cbxRede.Text+' NSU: '+AForm.edNSU.Text+
                       ' Finalização: '+AForm.edFinalizacao.Text+
                       ' Valor: '+AForm.edValor.Text ) );
      ACBrTEFD1.NCN( AForm.cbxRede.Text,
                     AForm.edNSU.Text,
                     AForm.edFinalizacao.Text,
                     StringToFloatDef( AForm.edValor.Text, 0 ) );
      Memo1.Lines.Add('NCN executado com sucesso');
    end;
  finally
    AForm.Free ;
  end;
end;

procedure TForm1.bCancelarClick(Sender : TObject);
begin
   spbLimpaPagamentos.Click;
   ACBrECF1.CancelaCupom;
   Memo1.Lines.Add('ACBrECF.CancelaCupom');
   ACBrTEFD1.CancelarTransacoesPendentes;
   Memo1.Lines.Add('ACBrTEFD1.CancelaTransacoesPendentes');
end;

procedure TForm1.cbxModeloChange(Sender : TObject);
begin
  try
     ACBrECF1.Modelo := TACBrECFModelo( cbxModelo.ItemIndex ) ;
  except
     cbxModelo.ItemIndex := Integer( ACBrECF1.Modelo ) ;
     raise ;
  end ;
end;

procedure TForm1.cbxPortaChange(Sender : TObject);
begin
  try
    ACBrECF1.Porta := cbxPorta.Text ;
  finally
     cbxPorta.Text := ACBrECF1.Porta ;
  end ;
end;

procedure TForm1.ckAutoEfetuarPagamentoChange(Sender : TObject);
begin
  ACBrTEFD1.AutoEfetuarPagamento := ckAutoEfetuarPagamento.Checked;
end;

procedure TForm1.ckAutoFinalizarCupomChange(Sender : TObject);
begin
  ACBrTEFD1.AutoFinalizarCupom := ckAutoFinalizarCupom.Checked;
end;

procedure TForm1.ckMultiplosCartoesChange(Sender : TObject);
begin
  try
    ACBrTEFD1.MultiplosCartoes := TCheckBox(Sender).Checked ;
  finally
    ckMultiplosCartoes.Checked := ACBrTEFD1.MultiplosCartoes ;
  end;
end;

procedure TForm1.ckAutoAtivarChange(Sender : TObject);
begin
   ACBrTEFD1.AutoAtivarGP := ckAutoAtivar.Checked;
end;

procedure TForm1.ckHIPERTEFChange(Sender : TObject);
begin
  ACBrTEFD1.TEFHiper.Habilitado := ckHIPERTEF.Checked;
end;

procedure TForm1.ckTEFDISCChange(Sender : TObject);
begin
  ACBrTEFD1.TEFDisc.Habilitado := ckTEFDISC.Checked;
end;

procedure TForm1.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
   CanClose := self.Enabled ;
end;

procedure TForm1.bATVClick(Sender : TObject);
begin
  Memo1.Lines.Add('Inicio de ATV');
  ACBrTEFD1.ATV( TACBrTEFDTipo( cbxGP1.ItemIndex ) );
  Memo1.Lines.Add('ATV executado com sucesso');
end;

procedure TForm1.bADMClick(Sender : TObject);
begin
  bCancelarResp.Visible := False;
  fCancelado := False;

  VerificaECFAtivo;

  Memo1.Lines.Add('Inicio de ADM');
  if ACBrTEFD1.ADM( TACBrTEFDTipo( cbxGP1.ItemIndex ) ) then
     Memo1.Lines.Add('ADM executado com sucesso')
  else
     Memo1.Lines.Add('Falha ao executar ADM') ;
end;

procedure TForm1.ACBrTEFD1ExibeMsg(Operacao : TACBrTEFDOperacaoMensagem;
   Mensagem : String; var AModalResult : TModalResult);
var
   Fim : TDateTime;
   OldMensagem : String;
begin
  StatusBar1.Panels[1].Text := '' ;
  StatusBar1.Panels[2].Text := '' ;

  case Operacao of

    opmOK :
       AModalResult := MessageDlg( Mensagem, mtInformation, [mbOK], 0);

    opmYesNo :
       AModalResult := MessageDlg( Mensagem, mtConfirmation, [mbYes,mbNo], 0);

    opmExibirMsgOperador, opmRemoverMsgOperador :
         lMensagemOperador.Caption := Mensagem ;

    opmExibirMsgCliente, opmRemoverMsgCliente :
         lMensagemCliente.Caption := Mensagem ;

    opmDestaqueVia :
       begin
         OldMensagem := lMensagemOperador.Caption ;
         try
            lMensagemOperador.Caption := Mensagem ;
            pMensagemOperador.Visible := True ;
            pMensagem.Visible         := True ;

            { Aguardando 3 segundos }
            Fim := IncSecond( now, 3)  ;
            repeat
               sleep(200) ;
               lMensagemOperador.Caption := Mensagem + ' ' + IntToStr(SecondsBetween(Fim,now));
               Application.ProcessMessages;
            until (now > Fim) ;

         finally
            lMensagemOperador.Caption := OldMensagem ;
         end;
       end;
  end;

  pMensagemOperador.Visible := (lMensagemOperador.Caption <> '') ;
  pMensagemCliente.Visible  := (lMensagemCliente.Caption <> '') ;

  pMensagem.Visible := pMensagemOperador.Visible or pMensagemCliente.Visible;
  Application.ProcessMessages;
end;

procedure TForm1.ACBrTEFD1InfoECF(Operacao : TACBrTEFDInfoECF;
   var RetornoECF : String );
var
   ASubTotal: Double;
begin
   try
      if not ACBrECF1.Ativo then
         ACBrECF1.Ativar ;
   except
      { Para CliSiTEF ou V&SPague aplique o IF abaixo em sua aplicação, que
        permite saber se o Cupom foi concluido mesmo com o ECF desligado }

      if (not ACBrTEFD1.TEF.Inicializado) and   { Está na inicialização ? }
         (Operacao = ineEstadoECF) and          { Quer Saber o estado do ECF ? (mas se chegou aqui é pq o ECF já está com problemas) }
         (ACBrTEFD1.GPAtual in [gpCliSiTef,gpVeSPague]) then
      begin
         { Leia o último Documento Gravado no seu Banco de Dados, e verifique
           se o Cupom já foi finalizado,ou se já foi aberto um CCD ou Gerencial...
           Exemplo:

           Documento.Le(0);

           if (Documento.Finalizado) or (pos(Documento.Denominacao,'CC|RG') > 0) then
              RetornoECF := 'R'
           else
              RetornoECF := 'O' ;
         }

         //RetornoECF := 'O';    // Executará CancelarTransacoesPendentes;
         RetornoECF := 'R';    // Executará ConfirmarESolicitarImpressaoTransacoesPendentes;
         exit ;
      end ;

      raise ;
   end;

   case Operacao of
     ineSubTotal :
       begin
         ASubTotal := ACBrECF1.Subtotal ;
         ASubTotal := ASubTotal - ACBrECF1.TotalPago +
                      StringToFloatDef(edValorDescAcre.Text, 0);

         RetornoECF := FloatToStr( ASubTotal ) ;
       end;

     ineTotalAPagar :
       RetornoECF := FloatToStr( CalculaTotalPago );

     ineEstadoECF :
       begin
         Case ACBrECF1.Estado of
           estLivre     : RetornoECF := 'L' ;
           estVenda     : RetornoECF := 'V' ;
           estPagamento : RetornoECF := 'P' ;
           estRelatorio : RetornoECF := 'R' ;
           estNaoFiscal : RetornoECF := 'N' ;
         else
           RetornoECF := 'O' ;
         end;
       end;
   end;
end;

procedure TForm1.ACBrTEFD1MudaEstadoReq(EstadoReq : TACBrTEFDReqEstado);
begin
   StatusBar1.Panels[1].Text := GetEnumName(TypeInfo(TACBrTEFDReqEstado), Integer(EstadoReq) ) ;
   fCancelado := False ;
end;

procedure TForm1.ACBrTEFD1MudaEstadoResp(EstadoResp : TACBrTEFDRespEstado);
begin
  StatusBar1.Panels[1].Text := GetEnumName(TypeInfo(TACBrTEFDRespEstado), Integer(EstadoResp) ) ;
  bCancelarResp.Visible     := (EstadoResp = respAguardandoResposta) ;
end;

procedure TForm1.ACBrTEFD1RestauraFocoAplicacao(var Tratado : Boolean);
begin
  Application.BringToFront;

  Tratado := False ;  { Deixa executar o código de Foco do ACBrTEFD }
end;

procedure TForm1.bAbreVendeSubTotaliza1Click(Sender : TObject);
begin
   ACBrTEFD1.CancelarTransacoesPendentes;
end;

procedure TForm1.bAbreVendeSubTotaliza2Click(Sender : TObject);
begin
   ACBrTEFD1.ConfirmarTransacoesPendentes;
end;

procedure TForm1.bAbreVendeSubTotaliza3Click(Sender : TObject);
begin
   ACBrTEFD1.ImprimirTransacoesPendentes;
end;

procedure TForm1.bAbreVendeSubTotaliza4Click(Sender : TObject);
begin
   ACBrTEFD1.FinalizarCupom;
end;

procedure TForm1.bCancelarRespClick(Sender : TObject);
begin
   fCancelado := True ;
   bCancelarResp.Visible := False;
end;

procedure TForm1.bPagamentoClick(Sender: TObject);
Var
  CodFormaPagamento : String;
begin
  CodFormaPagamento := '01' ;

  if not InputQuery('Pagamento de R$ '+edValorECF.Text,'Digite o Cod.Forma Pagamento',CodFormaPagamento ) then
     exit ;

  if StringToFloatDef(edValorECF.Text,0) = 0 then
     exit ;

  ACBrECF1.EfetuaPagamento( CodFormaPagamento, StringToFloatDef(edValorECF.Text, 0) );
  Memo1.Lines.Add('ACBrECF.EfetuaPagamento');
  Memo1.Lines.Add( 'Pagamento: '+CodFormaPagamento+' no valor: R$ '+edValorECF.Text+
                   ' registrado');
  MostraSaldoRestante;
end;

procedure TForm1.cbxGPChange(Sender : TObject);
begin
  if not (Sender is TComboBox) then exit ;

  try
    ACBrTEFD1.GPAtual := TACBrTEFDTipo(  TComboBox(Sender).ItemIndex ) ;
  finally
    AvaliaTEFs;
  end;
end;

procedure TForm1.ckCHQemGerencialChange(Sender: TObject);
begin
  ACBrTEFD1.CHQEmGerencial := ckCHQemGerencial.Checked;
end;

procedure TForm1.ckAuttarChange(Sender : TObject) ;
begin
  ACBrTEFD1.TEFAuttar.Habilitado := ckAuttar.Checked;
end;

procedure TForm1.ckCliSiTefChange(Sender : TObject);
begin
  ACBrTEFD1.TEFCliSiTef.Habilitado := ckCliSiTef.Checked;
end;

procedure TForm1.ckViaClienteReduzidaClick(Sender: TObject);
begin
   try
    ACBrTEFD1.ImprimirViaClienteReduzida := TCheckBox(Sender).Checked ;
   finally
    ckViaClienteReduzida.Checked := ACBrTEFD1.ImprimirViaClienteReduzida;
   end;
end;

procedure TForm1.ckVSPagueChange(Sender : TObject) ;
begin
  ACBrTEFD1.TEFVeSPague.Habilitado := ckVSPague.Checked;
end;

procedure TForm1.CliDTEFExibeMenu(Titulo : String ; Opcoes : TStringList ;
   var ItemSelecionado : Integer ; var VoltarMenu : Boolean) ;
var
  AForm : TForm7 ;
  MR    : TModalResult ;
begin
  AForm := TForm7.Create(self);
  try
    AForm.pnlInformacao.Caption := Titulo;
    AForm.ListBox1.Items.AddStrings(Opcoes);

    MR := AForm.ShowModal ;

    VoltarMenu := (MR = mrRetry) ;

    if (MR = mrOK) then
      ItemSelecionado := AForm.ListBox1.ItemIndex;
  finally
    AForm.Free;
  end;
end;

procedure TForm1.CliDTEFObtemInformacao(var ItemSelecionado : Integer) ;
begin
  case ComboBox1.ItemIndex of
    1: ItemSelecionado := 1;
    2: ItemSelecionado := 2;
    3: ItemSelecionado := 3;
    4: ItemSelecionado := 4;
    5: ItemSelecionado := 5;
    6: ItemSelecionado := 10;
  end;
end;

procedure TForm1.edEsperaSleepChange(Sender : TObject);
begin
   ACBrTEFD1.EsperaSleep := StrToInt(edEsperaSleep.Text);
end;

procedure TForm1.edEsperaSTSChange(Sender : TObject);
begin
   ACBrTEFD1.EsperaSTS := StrToInt(edEsperaSTS.Text);
end;

procedure TForm1.pMensagemOperadorClick(Sender: TObject);
begin
   pMensagem.Visible := False ;
end;

procedure TForm1.pMensagemResize(Sender : TObject);
begin
   pMensagemCliente.Height := Trunc( pMensagem.Height / 2 ) ;
end;

procedure TForm1.spbAdicionaPagamento1Click(Sender: TObject);
begin
   MostraSaldoRestante;
end;

procedure TForm1.spbAdicionaPagamentoClick(Sender: TObject);
Var
  CodFormaPagamento, ValorStr : String;
begin
  if CalculaSaldoRestante <= 0 then
  begin
    Memo1.Lines.Add('Total do Cupom já foi atingido');
    exit ;
  end;

  CodFormaPagamento := ACBrECF1.FormasPagamento[0].Indice ;
  ValorStr          := '1' ;

  if not InputQuery('Pagamento','Digite o Cod.Forma Pagamento',CodFormaPagamento ) then
     exit ;

  if not InputQuery('Pagamento','Digite o Valor a Pagar',ValorStr ) then
     exit ;

  if StringToFloatDef(ValorStr,0) = 0 then
  begin
     Memo1.Lines.Add('Valor: '+ValorStr+' inválido');
     exit ;
  end;

  CodFormaPagamento := Trim(CodFormaPagamento);
  if (CodFormaPagamento = '') or
     (ACBrECF1.AchaFPGIndice(CodFormaPagamento) = Nil) then
  begin
    Memo1.Lines.Add('Cod.Forma Pagto: ['+CodFormaPagamento+'] inválido');
    exit ;
  end;

  mPagamentos.Lines.Add( CodFormaPagamento + '|' + ValorStr );
  Memo1.Lines.Add( 'Pagamento: '+CodFormaPagamento+' no valor: '+ValorStr+
                   ' acumulado');

  MostraSaldoRestante;
end;

procedure TForm1.spbLimpaPagamentosClick(Sender: TObject);
begin
   mPagamentos.Lines.Clear;
   edValorDescAcre.Text := '0,00';
end;

procedure TForm1.spRemovePagamentoClick(Sender: TObject);
var
   LineNumber: Integer;
begin
  LineNumber := mPagamentos.CaretPos.y;
  if LineNumber >= 0 then
     mPagamentos.Lines.Delete( LineNumber );
end;

procedure TForm1.TrataErros(Sender : TObject; E : Exception);
begin
  Memo1.Lines.Add( E.Message );
  StatusBar1.Panels[1].Text := 'Exception' ;
  StatusBar1.Panels[2].Text := StringReplace( E.Message, sLineBreak, ' ', [rfReplaceAll] ) ;
  pMensagem.Visible := False ;
end;

procedure TForm1.bAbreVendeSubTotalizaClick(Sender : TObject);
Var
  Valor : Double ;
begin
  Valor := StringToFloatDef(edValorECF.Text, 0);

  try
    self.Enabled := False ;
    ACBrECF1.AbreCupom;
    Memo1.Lines.Add('ACBrECF.AbreCupom');

    ACBrECF1.VendeItem('12345','PRODUTO TESTE','NN',1,Valor);
    Memo1.Lines.Add('ACBrECF.VendeItem');

    ACBrECF1.SubtotalizaCupom ;
    Memo1.Lines.Add('ACBrECF.SubtotalizaCupom');
    MostraSaldoRestante;
  finally
    self.Enabled := True ;
  end;
end;

procedure TForm1.bCHQClick(Sender : TObject);
begin
  bCancelarResp.Visible := False;
  fCancelado := False;
  ACBrTEFD1.CHQ( StringToFloatDef(edValorTEF.Text, 0) ,edFPGCheque.Text, ACBrECF1.NumCOO);
  MostraSaldoRestante;
end;

procedure TForm1.bCNCClick(Sender : TObject);
Var
  AForm : TForm2 ;
  DT    : TDateTime ;
begin
  VerificaECFAtivo;

  AForm := TForm2.Create(self);

  try
    if AForm.ShowModal = mrOK then
    begin
      DT := EncodeDateTime( StrToInt(copy(AForm.edData.Text,7,4)),
                            StrToInt(copy(AForm.edData.Text,4,2)),
                            StrToInt(copy(AForm.edData.Text,1,2)),
                            StrToInt(copy(AForm.meHora.Text,1,2)),
                            StrToInt(copy(AForm.meHora.Text,4,2)),
                            StrToInt(copy(AForm.meHora.Text,7,2)), 0) ;

      Memo1.Lines.Add( ACBrStr( 'Inicio de CNC - Rede: '+
                       AForm.cbxRede.Text+' NSU: '+AForm.edNSU.Text+
                       ' DataHora: '+DateTimeToStr(DT)+
                       ' Valor: '+AForm.edValor.Text ) );
      ACBrTEFD1.CNC( AForm.cbxRede.Text,
                     AForm.edNSU.Text,
                     DT,
                     StringToFloatDef( AForm.edValor.Text, 0 ) );
      Memo1.Lines.Add('CNC executado com sucesso');

    end;
  finally
    AForm.Free ;
  end;
end;

procedure TForm1.bCNFClick(Sender : TObject);
Var
  AForm : TForm3 ;
begin
  VerificaECFAtivo;

  AForm := TForm3.Create(self);

  try
    AForm.IsNCN := False ;
    if AForm.ShowModal = mrOK then
    begin
      Memo1.Lines.Add( ACBrStr( 'Inicio de CNF - Rede: '+
                       AForm.cbxRede.Text+' NSU: '+AForm.edNSU.Text+
                       ' Finalização: '+AForm.edFinalizacao.Text) );
      ACBrTEFD1.CNF( AForm.cbxRede.Text,
                     AForm.edNSU.Text,
                     AForm.edFinalizacao.Text);
      Memo1.Lines.Add('CNF executado com sucesso');
    end;
  finally
    AForm.Free ;
  end;
end;

procedure TForm1.bCRTClick(Sender : TObject);
begin
   bCancelarResp.Visible := False;
   fCancelado := False;
   ACBrTEFD1.CRT( StringToFloatDef(edValorTEF.Text, 0) ,edFPGCartao.Text, ACBrECF1.NumCOO);
   MostraSaldoRestante;
end;

procedure TForm1.bEstadoClick(Sender : TObject);
begin
   Memo1.Lines.Add('Estado: '+GetEnumName(TypeInfo(TACBrECFEstado), Integer(ACBrECF1.Estado) )) ;
end;

procedure TForm1.ACBrTEFD1AguardaResp(Arquivo : String;
   SegundosTimeOut : Integer; var Interromper : Boolean);
var
  Msg : String ;
begin
  Msg := '' ;
  if (ACBrTEFD1.GPAtual in [gpCliSiTef, gpVeSPague]) then   // É TEF dedicado ?
   begin
     if (Arquivo = '23') and (not bCancelarResp.Visible) then  // Está aguardando Pin-Pad ?
     begin
        if ACBrTEFD1.TecladoBloqueado then
        begin
           ACBrTEFD1.BloquearMouseTeclado(False);  // Desbloqueia o Teclado
           // TODO: nesse ponto é necessário desbloquear o Teclado, mas permitir
           //       um clique apenas no botão cancelar.... FALTA CORRIGIR NO DEMO
        end ;

        Msg := 'Tecle "ESC" para cancelar.';
        bCancelarResp.Visible := True ;
        fCancelado := False;
     end;
   end
  else
     Msg := 'Aguardando: '+Arquivo+' '+IntToStr(SegundosTimeOut) ;

  if Msg <> '' then
     StatusBar1.Panels[2].Text := Msg;
  Application.ProcessMessages;

  if fCancelado then
     Interromper := True ;
end;

procedure TForm1.ACBrECF1MsgPoucoPapel(Sender : TObject) ;
Var
  OldTecladoBloqueado : Boolean ;
begin
  OldTecladoBloqueado := ACBrTEFD1.TecladoBloqueado;
  ACBrTEFD1.BloquearMouseTeclado(False);
  try
     ShowMessage( ACBrStr('ATENÇÃO. Detectada proximadade do fim da Bobina') );
  finally
    ACBrTEFD1.BloquearMouseTeclado(OldTecladoBloqueado);
  end ;
end;

procedure TForm1.ACBrTEFD1AntesCancelarTransacao(RespostaPendente: TACBrTEFDResp);
var
   Est: TACBrECFEstado;
begin
   Est := ACBrECF1.Estado;

   case Est of
      estVenda, estPagamento :
        ACBrECF1.CancelaCupom;

      estRelatorio :
          ACBrECF1.FechaRelatorio;
   else
      if not ( Est in [estLivre, estDesconhecido, estNaoInicializada] ) then
         ACBrECF1.CorrigeEstadoErro( False ) ;
   end;
end;

procedure TForm1.ACBrTEFD1AntesFinalizarRequisicao(Req : TACBrTEFDReq);
begin
   if Req.Header = 'CRT' then
      Req.GravaInformacao(777,777,'TESTE REDECARD');
   Memo1.Lines.Add('Enviando: '+Req.Header+' ID: '+IntToStr( Req.ID ) );
end;

procedure TForm1.ACBrTEFD1BaneseObtemInformacao(var ItemSelecionado: Integer);
begin
  Form6 := TForm6.Create(Self);
  Form6.ShowModal;

  if Form6.RadioButton1.Checked then
    ItemSelecionado := 1;
  if Form6.RadioButton2.Checked then
    ItemSelecionado := 2;
  if Form6.RadioButton3.Checked then
    ItemSelecionado := 3;

  Form6.Free;
end;

procedure TForm1.ACBrTEFD1BaneseObtemOpcaoAdm(var opcao: Integer);
begin
  opcao := 2;
end;

procedure TForm1.ACBrTEFD1BloqueiaMouseTeclado(Bloqueia : Boolean;
   var Tratado : Boolean);
begin
  self.Enabled := not Bloqueia ;
  Memo1.Lines.Add('BloqueiaMouseTeclado = '+IfThen(Bloqueia,'SIM', 'NAO'));

  Tratado := False ;  { Deixa executar o código de Bloqueio do ACBrTEFD }
end;

procedure TForm1.ACBrTEFD1ComandaECF(Operacao : TACBrTEFDOperacaoECF;
   Resp : TACBrTEFDResp; var RetornoECF : Integer );
Var
   Est : TACBrECFEstado ;
   P   : Integer;
   Linha, CodFPG : String ;
   ValorFPG : Double ;
begin
  Memo1.Lines.Add('ComandaECF: '+GetEnumName( TypeInfo(TACBrTEFDOperacaoECF),
                                              integer(Operacao) ));

  try
    Est := ACBrECF1.Estado;

    case Operacao of
      opeAbreGerencial :
         ACBrECF1.AbreRelatorioGerencial ;

      opeCancelaCupom :
         if Est = estNaoFiscal then
            ACBrECF1.CancelaNaoFiscal
         else
            ACBrECF1.CancelaCupom;

      opeFechaCupom :
         if Est = estNaoFiscal then
            ACBrECF1.FechaNaoFiscal('Projeto ACBr|http://acbr.sf.net')
         else
            ACBrECF1.FechaCupom('Projeto ACBr|http://acbr.sf.net');

      opeSubTotalizaCupom :
         if Est = estNaoFiscal then
            ACBrECF1.SubtotalizaNaoFiscal( 0, 'Projeto ACBr|http://acbr.sf.net' )
         else
            ACBrECF1.SubtotalizaCupom( 0, 'Projeto ACBr|http://acbr.sf.net' );

      opeFechaGerencial, opeFechaVinculado :
        ACBrECF1.FechaRelatorio ;

      opePulaLinhas :
        begin
          ACBrECF1.PulaLinhas( ACBrECF1.LinhasEntreCupons );
          ACBrECF1.CortaPapel( True );
          Sleep(200);
        end;

      opeImprimePagamentos :
        begin
          while mPagamentos.Lines.Count > 0 do
          begin
             Linha := mPagamentos.Lines[0] ;
             P     := pos('|',Linha) ;
             if P > 0 then
             begin
                CodFPG   := Trim(copy(Linha,1,P-1)) ;
                ValorFPG := StringToFloatDef( copy(Linha, P+1, Length(Linha) ), 0 );
                if (CodFPG <> '') and (ValorFPG > 0) then
                   ACBrECF1.EfetuaPagamento( CodFPG, ValorFPG );
             end;
             mPagamentos.Lines.Delete(0);
          end;
        end;
    end;

    RetornoECF := 1 ;
  except
    RetornoECF := 0 ;
  end;
end;

procedure TForm1.ACBrTEFD1ComandaECFAbreVinculado(COO, IndiceECF : String;
   Valor : Double; var RetornoECF : Integer);
begin
  try
     Memo1.Lines.Add( 'ACBrTEFD1ComandaECFAbreVinculado, COO:'+COO+
        ' IndiceECF: '+IndiceECF+' Valor: '+FormatFloat('0.00',Valor) ) ;
     ACBrECF1.AbreCupomVinculado( COO, IndiceECF, Valor );
     RetornoECF := 1 ;
  except
     RetornoECF := 0 ;
  end;
end;

procedure TForm1.ACBrTEFD1ComandaECFImprimeVia(
   TipoRelatorio : TACBrTEFDTipoRelatorio; Via : Integer;
   ImagemComprovante : TStringList; var RetornoECF : Integer);
begin
  Memo1.Lines.Add( 'ACBrTEFD1ComandaECFImprimeVia, Tipo: '+
     IfThen(TipoRelatorio = trGerencial, 'trGerencial','trVinculado') +
     ' Via: '+IntToStr(Via) );
  Memo1.Lines.AddStrings( ImagemComprovante );

  { *** Se estiver usando ACBrECF... Lembre-se de configurar ***
    ACBrECF1.MaxLinhasBuffer   := 3; // Os homologadores permitem no máximo
                                     // Impressao de 3 em 3 linhas
    ACBrECF1.LinhasEntreCupons := 7; // (ajuste conforme o seu ECF)

    NOTA: ACBrECF nao possui comando para imprimir a 2a via do CCD }

  try
     case TipoRelatorio of
       trGerencial :
         ACBrECF1.LinhaRelatorioGerencial( ImagemComprovante.Text ) ;

       trVinculado :
         ACBrECF1.LinhaCupomVinculado( ImagemComprovante.Text )
     end;

     RetornoECF := 1 ;
  except
     RetornoECF := 0 ;
  end;
end;

procedure TForm1.ACBrTEFD1ComandaECFPagamento(IndiceECF : String;
   Valor : Double; var RetornoECF : Integer);
var
   Est : TACBrECFEstado ;
begin
  try
     Memo1.Lines.Add( 'ComandaECFPagamento, IndiceECF: '+IndiceECF+
        ' Valor: '+FormatFloat('0.00',Valor) );
     Est := ACBrECF1.Estado;

     if Est = estNaoFiscal then
        ACBrECF1.EfetuaPagamentoNaoFiscal(IndiceECF, Valor)
     else
        ACBrECF1.EfetuaPagamento(IndiceECF, Valor);

     RetornoECF := 1 ;
  except
     RetornoECF := 0 ;
  end;
end;

procedure TForm1.ACBrTEFD1ComandaECFSubtotaliza(DescAcre: Double;
   var RetornoECF: Integer);
Var
   Est : TACBrECFEstado ;
   MeuAcresDesc : Double ;
begin
  Memo1.Lines.Add('ComandaECFSubtotaliza: DescAcre: ' + FormatFloat('0.00',DescAcre) );

  MeuAcresDesc := StringToFloatDef( edValorDescAcre.Text, 0 );
  try
    Est := ACBrECF1.Estado;

    if Est = estNaoFiscal then
       ACBrECF1.SubtotalizaNaoFiscal( DescAcre + MeuAcresDesc, 'Projeto ACBr|http://acbr.sf.net' )
    else
       ACBrECF1.SubtotalizaCupom( DescAcre + MeuAcresDesc, 'Projeto ACBr|http://acbr.sf.net' );

    { Remove o Desconto pois já foi aplicado, caso contrário iria influenciar o
      retorno de ineSubTotal }
    edValorDescAcre.Text := '0,00';
    RetornoECF := 1 ;
  except
    RetornoECF := 0 ;
  end;
end;

procedure TForm1.ACBrTEFD1DepoisConfirmarTransacoes(
   RespostasPendentes: TACBrTEFDRespostasPendentes);
var
  I : Integer;
begin
  for I := 0 to RespostasPendentes.Count-1  do
  begin
     with RespostasPendentes[I] do
     begin
        Memo1.Lines.Add('Confirmado: '+Header+' ID: '+IntToStr( ID ) );

        Memo1.Lines.Add( 'Rede: '  + Rede +
                         ' NSU: '  + NSU  +
                         ' Valor: '+ FormatFloat('###,###,##0.00',ValorTotal)) ;
        Memo1.Lines.Add('Campo 11: ' + LeInformacao(11,0).AsString );
     end;
  end;
end;

procedure TForm1.ACBrTEFD1CliSiTefExibeMenu(Titulo: String;
  Opcoes: TStringList; var ItemSelecionado: Integer;
  var VoltarMenu: Boolean);
Var
  AForm : TForm4 ;
  MR    : TModalResult ;
begin
  AForm := TForm4.Create(self);
  try
    AForm.Panel1.Caption := Titulo;
    AForm.ListBox1.Items.AddStrings(Opcoes);

    MR := AForm.ShowModal ;

    VoltarMenu := (MR = mrRetry) ;

    if (MR = mrOK) then
      ItemSelecionado := AForm.ListBox1.ItemIndex;
  finally
    AForm.Free;
  end;
end;

procedure TForm1.ACBrTEFD1CliSiTefObtemCampo(Titulo: String; TamanhoMinimo,
  TamanhoMaximo, TipoCampo: Integer;
  Operacao: TACBrTEFDCliSiTefOperacaoCampo; var Resposta: AnsiString;
  var Digitado, VoltarMenu: Boolean);
Var
  AForm : TForm5 ;
  MR    : TModalResult ;
begin
  AForm := TForm5.Create(self);
  try
    AForm.Panel1.Caption := Titulo;
    AForm.TamanhoMaximo  := TamanhoMaximo;
    AForm.TamanhoMinimo  := TamanhoMinimo;
    AForm.Operacao       := Operacao;
    AForm.TipoCampo      := TipoCampo;
    AForm.Edit1.Text     := Resposta; { Para usar Valores Previamente informados }

    MR := AForm.ShowModal ;

    Digitado   := (MR = mrOK) ;
    VoltarMenu := (MR = mrRetry) ;

    if Digitado then
       Resposta := AForm.Edit1.Text;
  finally
    AForm.Free;
  end;
end;

procedure TForm1.ACBrTEFD1VeSPagueExibeMenu(Titulo: string; Opcoes,
   Memo: TStringList; var ItemSelecionado: Integer);
Var
  AForm : TForm4 ;
  MR    : TModalResult ;
begin
  AForm := TForm4.Create(self);
  try
    if Trim(Titulo) = '' then
       Titulo := ACBrStr('Escolha uma opção') ;

    AForm.Panel1.Caption := Titulo;
    AForm.ListBox1.Items.AddStrings(Opcoes);
    AForm.BitBtn3.Visible := False ;
    AForm.Memo1.Lines.Assign(Memo) ;

    MR := AForm.ShowModal ;

    if (MR = mrOK) then
      ItemSelecionado := AForm.ListBox1.ItemIndex;
  finally
    AForm.Free;
  end;
end;

procedure TForm1.ACBrTEFD1VeSPagueObtemCampo(Titulo, Mascara: string;
  Tipo: AnsiChar; var Resposta: string; var Digitado: Boolean);
Var
  AForm : TForm5 ;
  MR    : TModalResult ;
begin
  AForm := TForm5.Create(self);
  try
    AForm.Panel1.Caption  := Titulo;
    AForm.BitBtn3.Visible := False ;
    AForm.Edit1.Text      := Resposta; { Para usar Valores Previamente informados }

    MR := AForm.ShowModal ;

    Digitado := (MR = mrOK) ;

    if Digitado then
       Resposta := AForm.Edit1.Text;
  finally
    AForm.Free;
  end;
end;

procedure TForm1.BaneseObtemInformacao(var ItemSelecionado : Integer);
Var
  AForm : TForm6 ;
  MR    : TModalResult ;
begin
  AForm := TForm6.Create(self);
  try
    MR := AForm.ShowModal ;

    if MR = mrOK then
      begin
        if AForm.RadioButton1.Checked then
          ItemSelecionado := 1;
        if AForm.RadioButton2.Checked then
          ItemSelecionado := 2;
        if AForm.RadioButton3.Checked then
          ItemSelecionado := 3;
      end;
  finally
    AForm.Free;
  end;
end;

end.

