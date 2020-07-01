{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
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

unit FormTelaPrincipal;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, Buttons, Grids, DBGrids,
  ACBrTEFPayGoComum, ACBrPOS, ACBrPOSPGWebAPI, ImgList, ACBrBase;

type
  TItemPedido = record
    CodItem: Integer;
    Descricao: String;
    UN: String;
    Qtd: Double;
    PrecoUnit: Double;
  end;

  TPedido = record
    NumPedido: Integer;
    Nome: String;
    DataHora: TDateTime;
    ValorTotal: Double;
    Items: array of TItemPedido;
  end;

  TPedidos = array of TPedido;

  { TfrPOSTEFServer }

  TfrPOSTEFServer = class(TForm)
    ACBrPOS1: TACBrPOS;
    btLerParametros: TBitBtn;
    btOperacao: TBitBtn;
    btIniciarParaServidor: TBitBtn;
    btConfiguracao: TBitBtn;
    btLimparLog: TBitBtn;
    btSalvarParametros: TBitBtn;
    cbImprimirViaReduzida: TCheckBox;
    cbSuportaDesconto: TCheckBox;
    cbUtilizarSaldoTotalVoucher: TCheckBox;
    cbSuportaSaque: TCheckBox;
    cbTipoAplicacao: TComboBox;
    edAplicacaoNome: TEdit;
    edMensagemBoasVindas: TEdit;
    edAplicacaoVersao: TEdit;
    edLog: TEdit;
    edRazaoSocial: TEdit;
    edRegistro: TEdit;
    gbConfigTEF: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label7: TLabel;
    lURLTEF: TLabel;
    mLog: TMemo;
    pBotaoTerminais: TPanel;
    pBotaoTerminais1: TPanel;
    pLogs: TPanel;
    pTerminais: TPanel;
    pConfiguracao: TPanel;
    pgPrincipal: TPageControl;
    SbArqLog: TSpeedButton;
    sePortaTCP: TSpinEdit;
    seMaxConexoes: TSpinEdit;
    sgTerminais: TStringGrid;
    Splitter1: TSplitter;
    tsConfiguracao: TTabSheet;
    tsOperacao: TTabSheet;
    ImageList1: TImageList;
    procedure ACBrPOS1GravarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ACBrPOS1MudaEstadoTerminal(const TerminalId: String; EstadoAtual,
      EstadoAnterior: TACBrPOSPGWebEstadoTerminal);
    procedure ACBrPOS1NovaConexao(const TerminalId: String; const Model: String;
      const MAC: String; const SerNo: String);
    procedure btConfiguracaoClick(Sender: TObject);
    procedure btIniciarParaServidorClick(Sender: TObject);
    procedure btOperacaoClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btLimparLogClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lURLTEFClick(Sender: TObject);
    procedure SbArqLogClick(Sender: TObject);
  private
    fPedidos: TPedidos;

    function GetNomeArquivoConfiguracao: String;
    procedure TratarException(Sender : TObject; E : Exception);
    procedure AdicionarLinhaLog(AMensagem: String);
    function EstadoTerminal(AEstado: TACBrPOSPGWebEstadoTerminal): String;

  protected
    procedure IrParaOperacaoPOS;
    procedure IrParaConfiguracao;
    procedure ConfigurarACBrPOS;
    procedure IniciarServidorPOS;
    procedure PararServidorPOS;

    procedure ExecutarFluxoEntrega(const TerminalId: String);
    procedure ExibirMenuPedidosEntrega(const TerminalId: String);
    procedure ImprimirPedido(const TerminalId: String; IndicePedido: Integer);

    procedure ExecutarFluxoFechamentoMesa(const TerminalId: String);

    procedure ExecutarFluxoFechamentoBomba(const TerminalId: String);
    procedure ExecutarReimpressao(const TerminalId: String);

    procedure EfetuarPagamento(const TerminalId: String; const Descricao: String;
      ValorTotal: Double);
    procedure IncluirPedidosSimulados;
  public
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

  end;

var
  frPOSTEFServer: TfrPOSTEFServer;

implementation

uses
  IniFiles, math, dateutils, strutils,
  ACBrUtil;

{$R *.dfm}

{ TfrPOSTEFServer }

procedure TfrPOSTEFServer.btLimparLogClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TfrPOSTEFServer.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrPOSTEFServer.btOperacaoClick(Sender: TObject);
begin
  IrParaOperacaoPOS;
end;

procedure TfrPOSTEFServer.btConfiguracaoClick(Sender: TObject);
begin
  IrParaConfiguracao;
end;

procedure TfrPOSTEFServer.ACBrPOS1GravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TfrPOSTEFServer.ACBrPOS1MudaEstadoTerminal(const TerminalId: String;
  EstadoAtual, EstadoAnterior: TACBrPOSPGWebEstadoTerminal);
var
  Linha, i: Integer;
  EstadoStr: String;
begin
  EstadoStr := EstadoTerminal(EstadoAtual);
  AdicionarLinhaLog('- MudaEstadoTerminal: '+TerminalId+', '+EstadoStr);
  Linha := -1;
  For i := 0 to sgTerminais.RowCount-1 do
  begin
    if sgTerminais.Cells[0,i] = TerminalId then
    begin
      Linha := i;
      Break;
    end;
  end;

  if (Linha < 0) then
  begin
    Linha := sgTerminais.RowCount-1;
    if (Trim(sgTerminais.Rows[Linha][0]) <> '') then
    begin
      sgTerminais.RowCount := sgTerminais.RowCount + 1;
      Inc(Linha);
    end;
  end;

  sgTerminais.Rows[Linha][0] := TerminalId;
  sgTerminais.Rows[Linha][1] := EstadoStr;
end;

procedure TfrPOSTEFServer.ACBrPOS1NovaConexao(const TerminalId: String;
  const Model: String; const MAC: String; const SerNo: String);
begin
  case cbTipoAplicacao.ItemIndex of
    1: ExecutarFluxoFechamentoMesa(TerminalId);
    2: ExecutarFluxoFechamentoBomba(TerminalId)
  else
    ExecutarFluxoEntrega(TerminalId)
  end;
end;

procedure TfrPOSTEFServer.btIniciarParaServidorClick(Sender: TObject);
begin
  if ACBrPOS1.Inicializada then
    PararServidorPOS
  else
    IniciarServidorPOS;
end;

procedure TfrPOSTEFServer.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrPOSTEFServer.FormCreate(Sender: TObject);
begin
  ImageList1.GetBitmap(0, btConfiguracao.Glyph);
  ImageList1.GetBitmap(1, btOperacao.Glyph);
  ImageList1.GetBitmap(2, btSalvarParametros.Glyph);
  ImageList1.GetBitmap(3, btLerParametros.Glyph);
  ImageList1.GetBitmap(6, btLimparLog.Glyph);
  ImageList1.GetBitmap(8, btIniciarParaServidor.Glyph);

  with sgTerminais do
  begin
    Cells[0, 0] := 'Terminal ID';
    Cells[1, 0] := 'Status';
  end;

  tsConfiguracao.TabVisible := False;
  tsOperacao.TabVisible := False;
  pgPrincipal.ActivePageIndex := 0;

  LerConfiguracao;
  Application.OnException := TratarException;
  IncluirPedidosSimulados;
end;

procedure TfrPOSTEFServer.lURLTEFClick(Sender: TObject);
begin
  OpenURL('https://projetoacbr.com.br/tef/');
end;

procedure TfrPOSTEFServer.SbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim,edLog.Text) = 0 then
    AFileLog := ExtractFilePath( Application.ExeName ) + edLog.Text
  else
    AFileLog := edLog.Text;

  OpenURL( AFileLog );
end;

function TfrPOSTEFServer.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName,'.ini') ;
end;

procedure TfrPOSTEFServer.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');
end;

procedure TfrPOSTEFServer.AdicionarLinhaLog(AMensagem: String);
begin
  mLog.Lines.Add(AMensagem);
end;

function TfrPOSTEFServer.EstadoTerminal(AEstado: TACBrPOSPGWebEstadoTerminal
  ): String;
begin
  case AEstado of
    statConectado: Result := 'Conectado';
    statOcupado: Result := 'Ocupado';
    statDesconectado: Result := 'Desconectado';
    statEsperaConexao: Result := 'Reconectando';
  else
    Result := 'Desconhecido';
  end;
end;

procedure TfrPOSTEFServer.IrParaOperacaoPOS;
begin
  AdicionarLinhaLog('- IrParaOperacaoPOS');
  GravarConfiguracao;
  ConfigurarACBrPOS;
  sgTerminais.RowCount := 2;
  pgPrincipal.ActivePage := tsOperacao;
end;

procedure TfrPOSTEFServer.IrParaConfiguracao;
begin
  AdicionarLinhaLog('- IrParaConfiguracao');
  PararServidorPOS;
  pgPrincipal.ActivePage := tsConfiguracao;
end;

procedure TfrPOSTEFServer.ConfigurarACBrPOS;
begin
  ACBrPOS1.DesInicializar;
  ACBrPOS1.PortaTCP := sePortaTCP.Value;
  ACBrPOS1.MaximoTerminaisConectados := seMaxConexoes.Value;
  ACBrPOS1.ArqLOG := edLog.Text;
  ACBrPOS1.NomeAplicacao := edAplicacaoNome.Text;
  ACBrPOS1.VersaoAplicacao := edAplicacaoVersao.Text;
  ACBrPOS1.SoftwareHouse := edRazaoSocial.Text;
  ACBrPOS1.MensagemBoasVindas := edMensagemBoasVindas.Text;
  ACBrPOS1.ImprimirViaClienteReduzida := cbImprimirViaReduzida.Checked;
  ACBrPOS1.SuportaDesconto := cbSuportaDesconto.Checked;
  ACBrPOS1.SuportaSaque := cbSuportaSaque.Checked;
  ACBrPOS1.UtilizaSaldoTotalVoucher := cbUtilizarSaldoTotalVoucher.Checked;
end;

procedure TfrPOSTEFServer.IniciarServidorPOS;
begin
  AdicionarLinhaLog('- IniciarServidorPOS');

  sgTerminais.RowCount := 2;
  ConfigurarACBrPOS;
  ACBrPOS1.Inicializar;
  btIniciarParaServidor.Caption := 'Parar Servidor';
  ImageList1.GetBitmap(9, btIniciarParaServidor.Glyph);
end;

procedure TfrPOSTEFServer.PararServidorPOS;
begin
  AdicionarLinhaLog('- PararServidorPOS');
  ACBrPOS1.DesInicializar;
  btIniciarParaServidor.Caption := 'Iniciar Servidor';
  ImageList1.GetBitmap(8, btIniciarParaServidor.Glyph);
end;

procedure TfrPOSTEFServer.ExecutarFluxoEntrega(const TerminalId: String);
var
  OP: SmallInt;
  OK: Boolean;
begin
  OK := False;
  repeat
    OP := ACBrPOS1.ExecutarMenu( TerminalId,
                                 'VER PEDIDOS|REIMPRESSAO|ADMINISTRATIVO|SAIR',
                                 'ACBR - DEMO ENTREGA' );
    case OP of
      0: ExibirMenuPedidosEntrega(TerminalId);
      1: ExecutarReimpressao(TerminalId);
      2: ACBrPOS1.ExecutarTransacaoTEF(TerminalId, operAdmin);
    else
      Exit;
    end;
  until OK;
end;

procedure TfrPOSTEFServer.ExibirMenuPedidosEntrega(const TerminalId: String);
var
  SL: TStringList;
  OP: SmallInt;
  i, IndicePedido, ColDis: Integer;
begin
  SL := TStringList.Create;
  try
    // *** NOTA *** Aqui você deve ler os Pedidos Pendentes no Seu Banco de Dados, com algum Filtro
    for i := 0 to Length(fPedidos)-1 do
      SL.Add(LeftStr(fPedidos[i].Nome,8)+' R$ '+FormatFloatBr(fPedidos[i].ValorTotal,'###0.00') );

    SL.Add('V O L T A R');
    OP := ACBrPOS1.ExecutarMenu(TerminalId, SL, 'ESCOLHA O PEDIDO');

    if (OP >= 0) and (OP < SL.Count-1) then
    begin
      ColDis := CACBrPOSPGWebColunasDisplay;
      IndicePedido := OP;
      ImprimirPedido(TerminalId, IndicePedido);
      ACBrPOS1.ExibirMensagem(TerminalId,
        PadCenter('PEDIDO '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4)+' IMPRESSO', ColDis)+
        PadCenter('CONFIRA O PEDIDO', ColDis), 5);

      EfetuarPagamento( TerminalId,
                        'PEDIDO: '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4),
                        fPedidos[IndicePedido].ValorTotal );
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.ImprimirPedido(const TerminalId: String;
  IndicePedido: Integer);
var
  SL: TStringList;
  ColImp, ColExp, i: Integer;
  TotalItem: Currency;
begin
  // *** NOTA *** Aqui você deve ler os Itens do Pedido Selecionado, no Seu Banco de Dados

  ColImp := CACBrPOSPGWebColunasImpressora;
  ColExp := Trunc(ColImp/2)-1;

  // Montando relatório com o Pedido
  SL := TStringList.Create;
  try
    SL.Add( '<e>'+PadCenter('PROJETO ACBR', ColExp) );
    SL.Add( PadCenter(' https://projetoacbr.com.br/tef ', ColImp, '-')  );
    SL.Add( '' );
    SL.Add( PadSpace( 'Pedido: '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4)+'|'+
                      'Data: '+FormatDateTimeBr(fPedidos[IndicePedido].DataHora),
              ColImp, '|') );
    SL.Add( '<e>Nome: '+fPedidos[IndicePedido].Nome );
    SL.Add( StringOfChar('-', ColImp) );
    for i := 0 to Length(fPedidos[IndicePedido].Items)-1 do
    begin
      TotalItem := fPedidos[IndicePedido].Items[i].Qtd * fPedidos[IndicePedido].Items[i].PrecoUnit;
      SL.Add( IntToStrZero(fPedidos[IndicePedido].Items[i].CodItem, 4)   + ' '+
              PadRight(fPedidos[IndicePedido].Items[i].Descricao,35) );
      SL.Add( PadSpace( FormatFloatBr(fPedidos[IndicePedido].Items[i].Qtd, '##0.00')+ ' '+
              PadRight(fPedidos[IndicePedido].Items[i].UN,2)+ ' x ' +
                        FormatFloatBr(fPedidos[IndicePedido].Items[i].PrecoUnit, '#,##0.00') + '|' +
                        FormatFloatBr( TotalItem, '#,##0.00'),
                ColImp, '|') );
    end;
    SL.Add( StringOfChar('-', ColImp) );
    SL.Add( '<e>'+PadSpace( 'TOTAL:|'+
                            FormatFloatBr( fPedidos[IndicePedido].ValorTotal, '#,##0.00'),
                    ColExp, '|') );

    ACBrPOS1.ImprimirTexto(TerminalId, SL.Text);
    ACBrPOS1.AvancarPapel(TerminalId);
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.ExecutarFluxoFechamentoMesa(const TerminalId: String);
begin
  raise Exception.Create('Exemplo ainda não implementado');
end;

procedure TfrPOSTEFServer.ExecutarFluxoFechamentoBomba(const TerminalId: String);
begin
  raise Exception.Create('Exemplo ainda não implementado');
end;

procedure TfrPOSTEFServer.ExecutarReimpressao(const TerminalId: String);
begin
  try
    ACBrPOS1.ImprimirComprovantesTEF(TerminalId, prnAmbas, False);
  except
    On E: Exception do
    begin
      ACBrPOS1.Beep(TerminalId, beepAlerta);
      ACBrPOS1.ExibirMensagem(TerminalId, E.Message, 5);
    end;
  end;
end;

procedure TfrPOSTEFServer.EfetuarPagamento(const TerminalId: String;
  const Descricao: String; ValorTotal: Double);
var
  OP: SmallInt;
  ColDis: Integer;
begin
  ColDis := CACBrPOSPGWebColunasDisplay;
  OP := ACBrPOS1.ExecutarMenu(TerminalId, 'CARTAO|DINHEIRO|CANCELAR', 'EFETUAR O PAGAMENTO');

  case OP of
    0:
      ACBrPOS1.ExecutarTransacaoPagamento(TerminalId, ValorTotal);
    1:
    begin
      ACBrPOS1.ExibirMensagem(TerminalId,
        LeftStr(Descricao, ColDis) + sLineBreak +
        'TOTAL.: '+FormatFloatBr(ValorTotal) + sLineBreak +
        '* D I N H E I R O *'+ sLineBreak +
        '* CONFIRA O TROCO *', 5);
    end
  else
    raise Exception.Create('PAGAMENTO CANCELADO');
  end;
end;

procedure TfrPOSTEFServer.IncluirPedidosSimulados;
begin
  SetLength(fPedidos, 4);
  fPedidos[0].NumPedido := 98;
  fPedidos[0].DataHora := IncMinute(Now, -40);
  fPedidos[0].Nome := 'DANIEL SIMOES';
  fPedidos[0].ValorTotal := 25;
  SetLength(fPedidos[0].Items, 1);
  fPedidos[0].Items[0].CodItem := 100;
  fPedidos[0].Items[0].Descricao := 'PIZZA BROTO PEPPERONI';
  fPedidos[0].Items[0].PrecoUnit := 25;
  fPedidos[0].Items[0].Qtd := 1;
  fPedidos[0].Items[0].UN := 'UN';

  fPedidos[1].NumPedido := 102;
  fPedidos[1].DataHora := IncMinute(Now, -30);
  fPedidos[1].Nome := 'INDIANA JONES';
  fPedidos[1].ValorTotal := 75;
  SetLength(fPedidos[1].Items, 2);
  fPedidos[1].Items[0].CodItem := 100;
  fPedidos[1].Items[0].Descricao := 'PIZZA BROTO PEPPERONI';
  fPedidos[1].Items[0].PrecoUnit := 25;
  fPedidos[1].Items[0].Qtd := 1;
  fPedidos[1].Items[0].UN := 'UN';
  fPedidos[1].Items[1].CodItem := 201;
  fPedidos[1].Items[1].Descricao := 'PIZZA GRANDE MARGUERITA';
  fPedidos[1].Items[1].PrecoUnit := 50;
  fPedidos[1].Items[1].Qtd := 1;
  fPedidos[1].Items[1].UN := 'PC';

  fPedidos[2].NumPedido := 113;
  fPedidos[2].DataHora := IncMinute(Now, -20);
  fPedidos[2].Nome := 'ANA MARIA';
  fPedidos[2].ValorTotal := 115;
  SetLength(fPedidos[2].Items, 3);
  fPedidos[2].Items[0].CodItem := 200;
  fPedidos[2].Items[0].Descricao := 'PIZZA GRANDE PEPPERONI';
  fPedidos[2].Items[0].PrecoUnit := 50;
  fPedidos[2].Items[0].Qtd := 1;
  fPedidos[2].Items[0].UN := 'UN';
  fPedidos[2].Items[1].CodItem := 201;
  fPedidos[2].Items[1].Descricao := 'PIZZA GRANDE MARGUERITA';
  fPedidos[2].Items[1].PrecoUnit := 50;
  fPedidos[2].Items[1].Qtd := 1;
  fPedidos[2].Items[1].UN := 'UN';
  fPedidos[2].Items[2].CodItem := 300;
  fPedidos[2].Items[2].Descricao := 'COCA PET 1,5';
  fPedidos[2].Items[2].PrecoUnit := 15;
  fPedidos[2].Items[2].Qtd := 1;
  fPedidos[2].Items[2].UN := 'LT';

  fPedidos[3].NumPedido := 212;
  fPedidos[3].DataHora := IncMinute(Now, -10);
  fPedidos[3].Nome := 'JUCA PATO';
  fPedidos[3].ValorTotal := 25.50;
  SetLength(fPedidos[3].Items, 2);
  fPedidos[3].Items[0].CodItem := 101;
  fPedidos[3].Items[0].Descricao := 'PIZZA BROTO MARGUERITA';
  fPedidos[3].Items[0].PrecoUnit := 25;
  fPedidos[3].Items[0].Qtd := 1;
  fPedidos[3].Items[0].UN := 'UN';
  fPedidos[3].Items[1].CodItem := 500;
  fPedidos[3].Items[1].Descricao := 'CHICLETE ADAMS';
  fPedidos[3].Items[1].PrecoUnit := 0.50;
  fPedidos[3].Items[1].Qtd := 1;
  fPedidos[3].Items[1].UN := 'UN';
 end;

procedure TfrPOSTEFServer.LerConfiguracao;
Var
  INI : TIniFile ;
begin
  AdicionarLinhaLog('- LerConfiguracao');
  INI := TIniFile.Create(NomeArquivoConfiguracao);
  try
    sePortaTCP.Value := INI.ReadInteger('POS', 'PortaTCP', sePortaTCP.Value);
    seMaxConexoes.Value := INI.ReadInteger('POS', 'MaxConexoes.', seMaxConexoes.Value);
    edLog.Text := INI.ReadString('POS', 'Log', '');
    edMensagemBoasVindas.Text := INI.ReadString('POS', 'MensagemBoasVindas', edMensagemBoasVindas.Text);
    cbImprimirViaReduzida.Checked := INI.ReadBool('POS', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    cbSuportaDesconto.Checked := INI.ReadBool('POS', 'SuportaDesconto', cbSuportaDesconto.Checked);
    cbSuportaSaque.Checked := INI.ReadBool('POS', 'SuportaSaque', cbSuportaSaque.Checked);
    cbUtilizarSaldoTotalVoucher.Checked := INI.ReadBool('POS', 'UtilizarSaldoTotalVoucher', cbUtilizarSaldoTotalVoucher.Checked);

    cbTipoAplicacao.ItemIndex := INI.ReadInteger('Aplicacao', 'TipoDemo', cbTipoAplicacao.ItemIndex);
    edRazaoSocial.Text := INI.ReadString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    edRegistro.Text := INI.ReadString('Aplicacao', 'Registro', edRegistro.Text);
    edAplicacaoNome.Text := INI.ReadString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    edAplicacaoVersao.Text := INI.ReadString('Aplicacao', 'Versao', edAplicacaoVersao.Text);
  finally
     INI.Free ;
  end ;
end;

procedure TfrPOSTEFServer.GravarConfiguracao;
Var
  INI : TIniFile ;
begin
  AdicionarLinhaLog('- GravarConfiguracao');
  INI := TIniFile.Create(NomeArquivoConfiguracao);
  try
    INI.WriteInteger('POS', 'PortaTCP', sePortaTCP.Value);
    INI.WriteInteger('POS', 'MaxConexoes.', seMaxConexoes.Value);
    INI.WriteString('POS', 'Log', edLog.Text);
    INI.WriteString('POS', 'MensagemBoasVindas', edMensagemBoasVindas.Text);
    INI.WriteBool('POS', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    INI.WriteBool('POS', 'SuportaDesconto', cbSuportaDesconto.Checked);
    INI.WriteBool('POS', 'SuportaSaque', cbSuportaSaque.Checked);
    INI.WriteBool('POS', 'UtilizarSaldoTotalVoucher', cbUtilizarSaldoTotalVoucher.Checked);

    INI.WriteInteger('Aplicacao', 'TipoDemo', cbTipoAplicacao.ItemIndex);
    INI.WriteString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    INI.WriteString('Aplicacao', 'Registro', edRegistro.Text);
    INI.WriteString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    INI.WriteString('Aplicacao', 'Versao', edAplicacaoVersao.Text);
  finally
     INI.Free ;
  end ;
end;

end.

