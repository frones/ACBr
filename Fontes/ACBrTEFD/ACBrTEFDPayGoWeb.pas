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

{$I ACBr.inc}

unit ACBrTEFDPayGoWeb;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrTEFDClass, ACBrTEFPayGoWebComum, ACBrTEFComum;

type

  { TACBrTEFDRespPayGoWeb }

  TACBrTEFDRespPayGoWeb = class( TACBrTEFDResp )
  private
    procedure ConteudoToComprovantes;
    procedure ConteudoToParcelas;
  public
    procedure ConteudoToProperty; override;
  end;

  { TACBrTEFDPayGoWeb }

  TACBrTEFDPayGoWeb = class( TACBrTEFDClass )
  private
    fOperacaoADM: Word;
    fOperacaoATV: Word;
    fOperacaoCHQ: Word;
    fOperacaoCNC: Word;
    fOperacaoCRT: Word;
    fOperacaoPRE: Word;
    fsPGWebAPI: TACBrTEFPGWebAPI;
    procedure GravaLogAPI(const ALogLine: String; var Tratado: Boolean);

    function GetCNPJEstabelecimento: String;
    function GetDiretorioTrabalho: String;
    function GetOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
    function GetOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
    function GetOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
    function GetOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
    function GetPathDLL: string;
    function GetPontoCaptura: String;
    function GetSuportaViasDiferenciadas: Boolean;
    function GetUtilizaSaldoTotalVoucher: Boolean;
    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetOnAguardaPinPad(AValue: TACBrTEFPGWebAPIAguardaPinPad);
    procedure SetOnExibeMensagem(AValue: TACBrTEFPGWebAPIExibeMensagem);
    procedure SetOnExibeMenu(AValue: TACBrTEFPGWebAPIExibeMenu);
    procedure SetOnObtemCampo(AValue: TACBrTEFPGWebAPIObtemCampo);
    procedure SetPathDLL(AValue: string);
    procedure SetPontoCaptura(AValue: String);
    procedure SetSuportaViasDiferenciadas(AValue: Boolean);
    procedure SetUtilizaSaldoTotalVoucher(AValue: Boolean);
  protected
    procedure FazerRequisicao(PWOPER: Word; AHeader: String = '';
        Valor: Double = 0; Documento: String = '');
    function ContinuarRequisicao: Boolean;
    procedure ObterDadosTransacao;
  public
    property PathDLL: string read GetPathDLL write SetPathDLL;
    property DiretorioTrabalho: String read GetDiretorioTrabalho write SetDiretorioTrabalho;

    constructor Create( AOwner : TComponent ) ; override;
    destructor Destroy ; override;

    procedure Inicializar ; override;
    procedure DesInicializar ; override;

    procedure AtivarGP ; override;
    procedure VerificaAtivo ; override;

    Procedure ATV ; override;
    Function ADM : Boolean ; override;
    Function CRT( Valor : Double; IndiceFPG_ECF : String;
       DocumentoVinculado : String = ''; Moeda : Integer = 0 ) : Boolean; override;
    Function CHQ( Valor : Double; IndiceFPG_ECF : String;
       DocumentoVinculado : String = ''; CMC7 : String = '';
       TipoPessoa : AnsiChar = 'F'; DocumentoPessoa : String = '';
       DataCheque : TDateTime = 0; Banco   : String = '';
       Agencia    : String = ''; AgenciaDC : String = '';
       Conta      : String = ''; ContaDC   : String = '';
       Cheque     : String = ''; ChequeDC  : String = '';
       Compensacao: String = '' ) : Boolean ; override;
    Procedure NCN(Rede, NSU, Finalizacao : String;
       Valor : Double = 0; DocumentoVinculado : String = ''); override;
    Procedure CNF(Rede, NSU, Finalizacao : String;
       DocumentoVinculado : String = ''); override;
    Function CNC(Rede, NSU : String; DataHoraTransacao : TDateTime;
       Valor : Double) : Boolean; overload; override;
    Function PRE(Valor : Double; DocumentoVinculado : String = '';
       Moeda : Integer = 0) : Boolean; override;
    function CDP(const EntidadeCliente: string; out Resposta: string): Boolean; override;
  published
    property CNPJEstabelecimento: String read GetCNPJEstabelecimento write SetCNPJEstabelecimento;
    property PontoCaptura: String read GetPontoCaptura write SetPontoCaptura;
    property SuportaViasDiferenciadas: Boolean read GetSuportaViasDiferenciadas
      write SetSuportaViasDiferenciadas;
    property UtilizaSaldoTotalVoucher: Boolean read GetUtilizaSaldoTotalVoucher
      write SetUtilizaSaldoTotalVoucher;

    property OperacaoATV: Word read fOperacaoATV write fOperacaoATV default PWOPER_INITIALIZ;
    property OperacaoADM: Word read fOperacaoADM write fOperacaoADM default PWOPER_ADMIN;
    property OperacaoCRT: Word read fOperacaoCRT write fOperacaoCRT default PWOPER_SALE;
    property OperacaoCHQ: Word read fOperacaoCHQ write fOperacaoCHQ default PWOPER_CHECKINQ;
    property OperacaoCNC: Word read fOperacaoCNC write fOperacaoCNC default PWOPER_SALEVOID;
    property OperacaoPRE: Word read fOperacaoPRE write fOperacaoPRE default PWOPER_PREPAID;

    property OnExibeMenu: TACBrTEFPGWebAPIExibeMenu read GetOnExibeMenu write SetOnExibeMenu;
    property OnObtemCampo: TACBrTEFPGWebAPIObtemCampo read GetOnObtemCampo write SetOnObtemCampo;
    property OnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem read GetOnExibeMensagem write SetOnExibeMensagem;
    property OnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad read GetOnAguardaPinPad write SetOnAguardaPinPad;
  end;

implementation

uses
  strutils, math, dateutils,
  ACBrTEFD, ACBrUtil;

{ TACBrTEFDRespPayGoWeb }

procedure TACBrTEFDRespPayGoWeb.ConteudoToComprovantes;
var
  ViasDeComprovante: Integer;
  ImprimirViaCliente, ImprimirViaEstabelecimento: Boolean;
  ViaCompleta, ViaDiferenciada: String;
begin
  {
  PWINFO_RCPTPRN      F4h 1 Indica quais vias de comprovante devem ser impressas:
                          0: não há comprovante, 1: imprimir somente a via do Cliente,
                          2: imprimir somente a via do Estabelecimento, 3: imprimir ambas as vias do Cliente e do Estabelecimento
  PWINFO_RCPTFULL     52h Comprovante para impressão – Via completa.
                          Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
  PWINFO_RCPTMERCH    53h Comprovante para impressão – Via diferenciada para o Estabelecimento.
                          Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
  PWINFO_RCPTCHOLDER  54h Comprovante para impressão – Via diferenciada para o Cliente.
                          Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
  PWINFO_RCPTCHSHORT  55h Comprovante para impressão – Cupom reduzido (para o Cliente).
                          Até 40 colunas, quebras de linha identificadas pelo caractere 0Dh.
  }

  ViasDeComprovante := LeInformacao(PWINFO_RCPTPRN, 0).AsInteger;
  ImprimirViaCliente := (ViasDeComprovante = 1) or (ViasDeComprovante = 3);
  ImprimirViaEstabelecimento := (ViasDeComprovante = 2) or (ViasDeComprovante = 3);

  ViaCompleta := LeInformacao(PWINFO_RCPTFULL, 0).AsString;

  // Verificando Via do Estabelecimento
  if ImprimirViaEstabelecimento then
  begin
    ViaDiferenciada := LeInformacao(PWINFO_RCPTMERCH, 0).AsString;
    if (ViaDiferenciada <> '') then
      fpImagemComprovante2aVia.Text := ViaDiferenciada
    else
      fpImagemComprovante2aVia.Text := ViaCompleta;
  end
  else
    fpImagemComprovante2aVia.Clear;

  // Verificando Via do Cliente
  if ImprimirViaCliente then
  begin
    ViaDiferenciada := LeInformacao(PWINFO_RCPTCHSHORT, 0).AsString;
    if (ViaDiferenciada = '') then
      ViaDiferenciada := LeInformacao(PWINFO_RCPTCHOLDER, 0).AsString;

    if (ViaDiferenciada = '') then
      fpImagemComprovante1aVia.Text := ViaDiferenciada
    else
      fpImagemComprovante1aVia.Text := ViaCompleta;
  end
  else
    fpImagemComprovante1aVia.Clear;

  fpQtdLinhasComprovante := max(fpImagemComprovante1aVia.Count, fpImagemComprovante2aVia.Count);
end;

procedure TACBrTEFDRespPayGoWeb.ConteudoToParcelas;
var
  DataParcela: TDateTime;
  ValorParcela: Double;
  I: Integer;
  Parc: TACBrTEFRespParcela;
begin
  fpParcelas.Clear;

  fpQtdParcelas := LeInformacao(PWINFO_INSTALLMENTS, 0).AsInteger;
  if (fpQtdParcelas > 0) then
  begin
    DataParcela := LeInformacao(PWINFO_INSTALLMDATE, 0).AsDate;
    ValorParcela := LeInformacao(PWINFO_INSTALLM1AMT, 0).AsFloat;

    for I := 1 to fpQtdParcelas do
    begin
      Parc := TACBrTEFRespParcela.create;
      Parc.Vencimento := DataParcela;
      Parc.Valor := ValorParcela;
      Parc.NSUParcela := fpNSU;
      fpParcelas.Add(Parc);

      ValorParcela := LeInformacao(PWINFO_INSTALLMAMNT, 0).AsFloat;
      DataParcela := IncDay(DataParcela,30);
    end;
  end;
end;

procedure TACBrTEFDRespPayGoWeb.ConteudoToProperty;
var
  I, AInt: Integer;
  LinStr: String;
  Linha: TACBrTEFLinha;
begin
  fpImagemComprovante1aVia.Clear;
  fpImagemComprovante2aVia.Clear;
  fpDebito := False;
  fpCredito := False;
  fpDigitado := False;
  fpTaxaServico := 0;
  fpDataHoraTransacaoCancelada := 0;

  for I := 0 to Conteudo.Count - 1 do
  begin
    Linha := Conteudo.Linha[I];
    LinStr := StringToBinaryString(Linha.Informacao.AsString);

    case Linha.Identificacao of
      PWINFO_TOTAMNT:
        fpValorTotal := Linha.Informacao.AsFloat;

      PWINFO_DISCOUNTAMT:
        fpDesconto := Linha.Informacao.AsFloat;

      PWINFO_CASHBACKAMT:
        fpSaque := Linha.Informacao.AsFloat;

      PWINFO_CURRENCY:
      begin
        AInt := Linha.Informacao.AsInteger;
        if AInt = 986 then
          fpMoeda := 0
        else if AInt = 840 then
          fpMoeda := 2
        else if AInt = 978 then
          fpMoeda := 2
        else
          fpMoeda := AInt;
      end;

      PWINFO_CNFREQ:
        fpConfirmar := (Trim(Linstr)='1');

      PWINFO_FISCALREF:
        fpDocumentoVinculado := LinStr;

      PWINFO_CARDTYPE:
      begin
        // 1: crédito, 2: débito, 4: voucher/PAT, 8: private label, 16: frota, 128: outros
        AInt := Linha.Informacao.AsInteger;
        fpCredito := (AInt = 1);
        fpDebito := (AInt = 2);
      end;

      PWINFO_CARDENTMODE:
      begin
        // 1: digitado, 2: tarja magnética, 4: chip com contato, 16: fallback de chip para tarja,
        // 32: chip sem contato simulando tarja (cliente informa tipo efetivamente utilizado),
        // 64: chip sem contato EMV (cliente informa tipo efetivamente, utilizado),
        // 256: fallback de tarja para digitado
        AInt := Linha.Informacao.AsInteger;
        fpDigitado := (AInt = 1) or (AInt = 256);
      end;

      PWINFO_CARDFULLPAN:
      begin
        fpBin := LinStr;
        fpNFCeSAT.UltimosQuatroDigitos := RightStr(LinStr,4);
      end;

      PWINFO_CARDPARCPAN:
      begin
        if (fpNFCeSAT.UltimosQuatroDigitos = '') then
          fpNFCeSAT.UltimosQuatroDigitos := RightStr(LinStr,4);
      end;

      PWINFO_DEFAULTCARDPARCPAN:
        fpNFCeSAT.UltimosQuatroDigitos := RightStr(LinStr,4);

      PWINFO_CARDEXPDATE:
        fpNFCeSAT.DataExpiracao := LinStr;

      PWINFO_DATETIME:
        fpDataHoraTransacaoLocal := Linha.Informacao.AsTimeStampSQL;

      PWINFO_AUTDATETIME:
        fpDataHoraTransacaoHost :=  Linha.Informacao.AsTimeStampSQL;

      PWINFO_AUTHSYST:
        fpNomeAdministradora := LinStr;

      PWINFO_CARDNAME:
      begin
        fpCodigoBandeiraPadrao := LinStr;
        if (fpNFCeSAT.Bandeira = '') then
          fpNFCeSAT.Bandeira := LinStr;
      end;

      PWINFO_CARDNAMESTD:
      begin
        fpRede := LinStr;
        fpNFCeSAT.Bandeira := LinStr;
      end;

      PWINFO_CHOLDERNAME:
        fpNFCeSAT.DonoCartao := LinStr;

      PWINFO_AUTLOCREF:
        fpFinalizacao := LinStr;

      PWINFO_AUTEXTREF:
      begin
        fpNSU := LinStr;
        fpNFCeSAT.Autorizacao := fpNSU;
      end;

      PWINFO_AUTHCODE:
        fpCodigoAutorizacaoTransacao := LinStr;

      PWINFO_AUTRESPCODE:
        fpAutenticacao := LinStr;

      PWINFO_FINTYPE:
      begin
        // 1: à vista, 2: parcelado pelo emissor, 4: parcelado pelo estabelecimento, 8: pré-datado, 16: crédito emissor
        AInt := Linha.Informacao.AsInteger;
        if (AInt = 2) then
        begin
          fpParceladoPor := parcADM;
          fpTipoOperacao := opParcelado;
        end
        else if (AInt = 4) then
        begin
          fpParceladoPor := parcLoja;
          fpTipoOperacao := opParcelado;
        end
        else if (AInt = 8) then
        begin
          fpParceladoPor := parcNenhum;
          fpTipoOperacao := opPreDatado;
        end
        else if (AInt = 16) then
        begin
          fpParceladoPor := parcNenhum;
          fpTipoOperacao := opOutras;
        end
        else
        begin
          fpParceladoPor := parcNenhum;
          fpTipoOperacao := opAvista;
        end
      end;

      PWINFO_INSTALLMDATE:
        fpDataPreDatado := Linha.Informacao.AsDate;

      PWINFO_BOARDINGTAX, PWINFO_TIPAMOUNT:
        fpTaxaServico := fpTaxaServico + Linha.Informacao.AsFloat;

      PWINFO_TRNORIGDATE:
        fpDataHoraTransacaoCancelada := fpDataHoraTransacaoCancelada + StringToDateTime(LinStr, 'DDMMYY');

      PWINFO_TRNORIGTIME:
        fpDataHoraTransacaoCancelada := fpDataHoraTransacaoCancelada + StringToDateTime(LinStr, 'HHNNSS');

      PWINFO_TRNORIGNSU:
        fpNSUTransacaoCancelada := LinStr;

      PWINFO_TRNORIGAMNT:
        fpValorOriginal := Linha.Informacao.AsFloat;

      PWINFO_AUTHPOSQRCODE:
        fpQRCode := LinStr;

    else
      ProcessarTipoInterno(Linha);
    end;
  end;

  ConteudoToComprovantes;
  ConteudoToParcelas;

  if (fpTipoOperacao <> opPreDatado) then
    fpDataPreDatado := 0;
end;

{ TACBrTEFDPayGoWeb }

constructor TACBrTEFDPayGoWeb.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fsPGWebAPI := TACBrTEFPGWebAPI.Create;
  fsPGWebAPI.OnGravarLog := GravaLogAPI;

  ArqReq := '';
  ArqResp := '';
  ArqSTS := '';
  ArqTemp := '';
  GPExeName := '';
  fpTipo := gpPayGoWeb;
  Name := 'PayGoWeb';

  fOperacaoATV := PWOPER_INITIALIZ;
  fOperacaoADM := PWOPER_ADMIN;
  fOperacaoCRT := PWOPER_SALE;
  fOperacaoCHQ := PWOPER_CHECKINQ;
  fOperacaoCNC := PWOPER_SALEVOID;
  fOperacaoPRE := PWOPER_PREPAID;

  if Assigned(fpResp) then
    fpResp.Free ;

  fpResp := TACBrTEFDRespPayGoWeb.Create;
  fpResp.TipoGP := fpTipo;
end;

destructor TACBrTEFDPayGoWeb.Destroy;
begin
  fsPGWebAPI.Free;
  inherited Destroy;
end;

procedure TACBrTEFDPayGoWeb.Inicializar;
begin
  if Inicializado then
    Exit;

  fsPGWebAPI.SoftwareHouse := TACBrTEFD(Owner).Identificacao.SoftwareHouse;
  fsPGWebAPI.NomeAplicacao := TACBrTEFD(Owner).Identificacao.NomeAplicacao;
  fsPGWebAPI.VersaoAplicacao := TACBrTEFD(Owner).Identificacao.VersaoAplicacao;
  fsPGWebAPI.SuportaDesconto := TACBrTEFD(Owner).SuportaDesconto;
  fsPGWebAPI.SuportaSaque := TACBrTEFD(Owner).SuportaSaque;
  fsPGWebAPI.ImprimirViaClienteReduzida := TACBrTEFD(Owner).ImprimirViaClienteReduzida;

  fsPGWebAPI.Inicializada := True;
  GravaLog( Name +' Inicializado '+Name );

  VerificarTransacoesPendentesClass(True);
  fpInicializado := True;
end;

procedure TACBrTEFDPayGoWeb.DesInicializar;
begin
  fsPGWebAPI.Inicializada := False; ;
  inherited DesInicializar;
end;

procedure TACBrTEFDPayGoWeb.AtivarGP;
begin
  raise EACBrTEFDErro.Create( ACBrStr( 'AtivarGP não se aplica a '+Name )) ;
end;

procedure TACBrTEFDPayGoWeb.VerificaAtivo;
begin
  inherited VerificaAtivo;
end;

procedure TACBrTEFDPayGoWeb.ATV;
begin
  FazerRequisicao(fOperacaoADM, 'ATV');
  if ContinuarRequisicao then
    ProcessarResposta;
end;

function TACBrTEFDPayGoWeb.ADM: Boolean;
begin
  FazerRequisicao(fOperacaoADM, 'ADM');
  if ContinuarRequisicao then
    ProcessarResposta;
end;

function TACBrTEFDPayGoWeb.CRT(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; Moeda: Integer): Boolean;
begin
  Result := inherited CRT(Valor, IndiceFPG_ECF, DocumentoVinculado, Moeda);
end;

function TACBrTEFDPayGoWeb.CHQ(Valor: Double; IndiceFPG_ECF: String;
  DocumentoVinculado: String; CMC7: String; TipoPessoa: AnsiChar;
  DocumentoPessoa: String; DataCheque: TDateTime; Banco: String;
  Agencia: String; AgenciaDC: String; Conta: String; ContaDC: String;
  Cheque: String; ChequeDC: String; Compensacao: String): Boolean;
begin
  Result := inherited CHQ(Valor, IndiceFPG_ECF, DocumentoVinculado, CMC7,
    TipoPessoa, DocumentoPessoa, DataCheque, Banco, Agencia, AgenciaDC, Conta,
    ContaDC, Cheque, ChequeDC, Compensacao);
end;

procedure TACBrTEFDPayGoWeb.NCN(Rede, NSU, Finalizacao: String; Valor: Double;
  DocumentoVinculado: String);
begin
  inherited NCN(Rede, NSU, Finalizacao, Valor, DocumentoVinculado);
end;

procedure TACBrTEFDPayGoWeb.CNF(Rede, NSU, Finalizacao: String;
  DocumentoVinculado: String);
begin
  inherited CNF(Rede, NSU, Finalizacao, DocumentoVinculado);
end;

function TACBrTEFDPayGoWeb.CNC(Rede, NSU: String; DataHoraTransacao: TDateTime;
  Valor: Double): Boolean;
begin
  Result := inherited CNC(Rede, NSU, DataHoraTransacao, Valor);
end;

function TACBrTEFDPayGoWeb.PRE(Valor: Double; DocumentoVinculado: String;
  Moeda: Integer): Boolean;
begin
  Result := inherited PRE(Valor, DocumentoVinculado, Moeda);
end;

function TACBrTEFDPayGoWeb.CDP(const EntidadeCliente: string; out
  Resposta: string): Boolean;
begin
  Result := inherited CDP(EntidadeCliente, Resposta);
end;

function TACBrTEFDPayGoWeb.GetPathDLL: string;
begin
  Result := fsPGWebAPI.PathDLL;
end;

function TACBrTEFDPayGoWeb.GetPontoCaptura: String;
begin
  Result := fsPGWebAPI.PontoCaptura;
end;

function TACBrTEFDPayGoWeb.GetSuportaViasDiferenciadas: Boolean;
begin
  Result := fsPGWebAPI.SuportaViasDiferenciadas;
end;

function TACBrTEFDPayGoWeb.GetUtilizaSaldoTotalVoucher: Boolean;
begin
  Result := fsPGWebAPI.UtilizaSaldoTotalVoucher;
end;

procedure TACBrTEFDPayGoWeb.SetCNPJEstabelecimento(AValue: String);
begin
  fsPGWebAPI.CNPJEstabelecimento := AValue;
end;

function TACBrTEFDPayGoWeb.GetDiretorioTrabalho: String;
begin
  Result := fsPGWebAPI.DiretorioTrabalho;
end;

function TACBrTEFDPayGoWeb.GetCNPJEstabelecimento: String;
begin
  Result := fsPGWebAPI.CNPJEstabelecimento;
end;

function TACBrTEFDPayGoWeb.GetOnAguardaPinPad: TACBrTEFPGWebAPIAguardaPinPad;
begin
  Result := fsPGWebAPI.OnAguardaPinPad;
end;

function TACBrTEFDPayGoWeb.GetOnExibeMensagem: TACBrTEFPGWebAPIExibeMensagem;
begin
  Result := fsPGWebAPI.OnExibeMensagem;
end;

function TACBrTEFDPayGoWeb.GetOnExibeMenu: TACBrTEFPGWebAPIExibeMenu;
begin
  Result := fsPGWebAPI.OnExibeMenu;
end;

function TACBrTEFDPayGoWeb.GetOnObtemCampo: TACBrTEFPGWebAPIObtemCampo;
begin
  Result := fsPGWebAPI.OnObtemCampo;
end;

procedure TACBrTEFDPayGoWeb.SetDiretorioTrabalho(AValue: String);
begin
  fsPGWebAPI.DiretorioTrabalho := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnAguardaPinPad(AValue: TACBrTEFPGWebAPIAguardaPinPad);
begin
  fsPGWebAPI.OnAguardaPinPad := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnExibeMensagem(AValue: TACBrTEFPGWebAPIExibeMensagem);
begin
  fsPGWebAPI.OnExibeMensagem := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnExibeMenu(AValue: TACBrTEFPGWebAPIExibeMenu);
begin
  fsPGWebAPI.OnExibeMenu := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetOnObtemCampo(AValue: TACBrTEFPGWebAPIObtemCampo);
begin
  fsPGWebAPI.OnObtemCampo := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPathDLL(AValue: string);
begin
  fsPGWebAPI.PathDLL := AValue;
end;

procedure TACBrTEFDPayGoWeb.GravaLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  GravaLog(ALogLine);
end;

procedure TACBrTEFDPayGoWeb.SetSuportaViasDiferenciadas(AValue: Boolean);
begin
  fsPGWebAPI.SuportaViasDiferenciadas := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetUtilizaSaldoTotalVoucher(AValue: Boolean);
begin
  fsPGWebAPI.UtilizaSaldoTotalVoucher := AValue;
end;

procedure TACBrTEFDPayGoWeb.SetPontoCaptura(AValue: String);
begin
  fsPGWebAPI.PontoCaptura := AValue;
end;

procedure TACBrTEFDPayGoWeb.FazerRequisicao(PWOPER: Word; AHeader: String;
  Valor: Double; Documento: String);
begin
  GravaLog('FazerRequisicao: Oper:'+PWINFOToString(PWOPER)+', Header'+AHeader+
           ', Valor:'+FloatToStr(Valor)+', Documento:'+Documento);

  fsPGWebAPI.IniciarTransacao(PWOPER);

  { Adiciona Campos já conhecidos em Resp, para processa-los em
    métodos que manipulam "RespostasPendentes" (usa códigos do G.P.)  }
  Resp.Clear;
  with TACBrTEFDRespPayGoWeb( Resp ) do
  begin
    fpIDSeq := fpIDSeq + 1 ;
    if Documento = '' then
      Documento := IntToStr(fpIDSeq) ;

    Conteudo.GravaInformacao(899,100, AHeader ) ;
    Conteudo.GravaInformacao(899,101, IntToStr(fpIDSeq) ) ;
    Conteudo.GravaInformacao(899,102, Documento ) ;
    Conteudo.GravaInformacao(899,103, IntToStr(Trunc(SimpleRoundTo( Valor * 100 ,0))) );

    Resp.TipoGP := fpTipo;
  end;
end;

function TACBrTEFDPayGoWeb.ContinuarRequisicao: Boolean;
begin
  Result := fsPGWebAPI.ExecutarTransacao;
  if Result then
    ObterDadosTransacao;
end;

procedure TACBrTEFDPayGoWeb.ObterDadosTransacao;
var
  i, p, AInfo: Integer;
  Lin, AValue: String;
begin
  fsPGWebAPI.ObterDadosDaTransacao;

  with TACBrTEFDRespPayGoWeb(Resp) do
  begin
    for i := 0 to fsPGWebAPI.DadosDaTransacao.Count-1 do
    begin
      Lin := fsPGWebAPI.DadosDaTransacao[i];
      p := pos('=', Lin);
      if (p > 0) then
      begin
        AInfo := StrToIntDef(copy(Lin, 1, p-1), -1);
        if (AInfo >= 0) then
        begin
          AValue := copy(Lin, P+1, Length(Lin));
          Conteudo.GravaInformacao(Ainfo, 0, AValue);
        end;
      end;
    end;
    
    //DEBUG
    //Conteudo.Conteudo.SaveToFile('c:\temp\PGWeb.txt');
    ConteudoToProperty;
  end;
end;

end.

(* 
PWINFO_PRODUCTID   3Eh até 8 Identificação do produto utilizado, de acordo com a nomenclatura do Provedor.
PWINFO_PRODUCTNAME 2Ah até 20 Nome/tipo do produto utilizado, na nomenclatura do Provedor
PWINFO_REQNUM      32h até 10 Referência local da transação

PWINFO_VIRTMERCH   36h até 9 Identificador do Estabelecimento.
PWINFO_AUTMERCHID  38h até 50 Identificador do estabelecimento para o Provedor (código de afiliação)


PWINFO_RESULTMSG
PWINFO_TRANSACDESCRIPT 1F40h Até 80 Descritivo da transação realizada, por exemplo, CREDITO A VISTA ou VENDA PARCELADA EM DUAS VEZES.
*)