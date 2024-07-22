{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrTEFComum;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase;

type

  TACBrTEFTipoCartao = ( teftcNaoDefinido,
                         teftcCredito,
                         teftcDebito,
                         teftcVoucher,
                         teftcFrota,
                         teftcPrivateLabel,
                         teftcOutros );
  TACBrTEFTiposCartao = set of TACBrTEFTipoCartao;

  TACBrTEFModalidadePagamento = ( tefmpNaoDefinido,
                                  tefmpCartao,
                                  tefmpDinheiro,
                                  tefmpCheque,
                                  tefmpCarteiraVirtual );

  TACBrTEFModalidadeFinanciamento = ( tefmfNaoDefinido,
                                      tefmfAVista,
                                      tefmfParceladoEmissor,
                                      tefmfParceladoEstabelecimento,
                                      tefmfPredatado,
                                      tefmfCreditoEmissor );

  TACBrTEFStatusTransacao = ( tefstsSucessoAutomatico,
                              tefstsSucessoManual,
                              tefstsErroImpressao,
                              tefstsErroDispesador,
                              tefstsErroEnergia,
                              tefstsErroDiverso );

  TACBrTEFOperacao = ( tefopNenhuma,
                       tefopPagamento,
                       tefopAdministrativo,
                       tefopTesteComunicacao,
                       tefopVersao,
                       tefopFechamento,
                       tefopCancelamento,
                       tefopReimpressao,
                       tefopPrePago,
                       tefopPreAutorizacao,
                       tefopConsultaSaldo,
                       tefopConsultaCheque,
                       tefopPagamentoConta,
                       tefopRelatResumido,
                       tefopRelatSintetico,
                       tefopRelatDetalhado );

  TACBrTEFTratamentoTransacaoPendente = ( tefpenConfirmar,
                                          tefpenEstornar,
                                          tefpenPerguntar );

  TACBrTEFTratamentoTransacaoInicializacao = ( tefopiNenhum,
                                               tefopiProcessarPendentes,
                                               tefopiCancelarOuEstornar);

  EACBrTEFErro = class(Exception);
  EACBrTEFArquivo = class(EACBrTEFErro);

  { TACBrTEFParametros }

  TACBrTEFParametros = class(TStringList)
  private
    function GetValueInfo(AInfo: Word): string;
    procedure SetValueInfo(AInfo: Word; const AValue: string);
  public
    property ValueInfo[AInfo: Word]: string read GetValueInfo write SetValueInfo;
  end;


  { TACBrTEFLinha }

  TACBrTEFLinha = class
  private
    FIdentificacao: Integer;
    FACBrTEFDLinhaInformacao: TACBrInformacao;
    FLinha: String;
    FSequencia: smallint;
    function GetChave: String;
  protected
    function GetLinha: String; virtual;
    procedure SetLinha(const AValue: String); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Linha: String read GetLinha write SetLinha;

    property Identificacao: Integer read FIdentificacao;
    property Sequencia: smallint read FSequencia;
    property Chave: String read GetChave;
    property Informacao: TACBrInformacao read FACBrTEFDLinhaInformacao;
  end;

  { TACBrTEFArquivo }

  TACBrTEFArquivo = class
  private
    FStringList: TStringList;
    FACBrTEFDLinha: TACBrTEFLinha;
    function GetCount: Integer;
    function GetLinha(Index: Integer): TACBrTEFLinha;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property Conteudo: TStringList read FStringList;
    property Count: Integer read GetCount;
    property Linha[Index: Integer]: TACBrTEFLinha read GetLinha;

    procedure GravarArquivo(const NomeArquivo: String; DoFlushToDisk: Boolean = False);
    procedure LeArquivo(const NomeArquivo: String);

    procedure GravaInformacao(const Chave, Informacao: AnsiString); overload; virtual;
    procedure GravaInformacao(const Chave: AnsiString; const Informacao: TACBrInformacao); overload; virtual;
    procedure GravaInformacao(const Identificacao: Integer; const Sequencia: Integer; const Informacao: AnsiString); overload; virtual;
    procedure GravaInformacao(const Identificacao: Integer; const Sequencia: Integer; const Informacao: TACBrInformacao); overload; virtual;
    function LeInformacao(const Identificacao: Integer; const Sequencia: Integer = 0): TACBrInformacao; virtual;

    function AchaLinha(const Identificacao: Integer; const Sequencia: Integer = 0): Integer;
    function LeLinha(const Identificacao: Integer; const Sequencia: Integer = 0): TACBrTEFLinha; virtual;
  end;

  TACBrTEFRespParceladoPor = (parcNenhum, parcADM, parcLoja);

  TACBrTEFRespTipoOperacao = (opOutras, opAvista, opParcelado, opPreDatado);

  { TACBrTEFRespCB - Armazena dados dos Correspondente Bancário }

  TACBrTEFRespCB = class
  private
    FAcrescimo: Double;
    FDataPagamento: TDateTime;
    FDataVencimento: TDateTime;
    FDesconto: Double;
    FDocumento: String;
    FNSUCancelamento: String;
    FNSUTransacaoCB: String;
    FTipoDocumento: Integer;
    FValorOriginal: Double;
    FValorPago: Double;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrTEFRespCB);

    property Acrescimo: Double read FAcrescimo write FAcrescimo;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property Desconto: Double read FDesconto write FDesconto;
    property Documento: String read FDocumento write FDocumento;
    property NSUCancelamento: String read FNSUCancelamento write FNSUCancelamento;
    property NSUTransacaoCB: String read FNSUTransacaoCB write FNSUTransacaoCB;
    property TipoDocumento: Integer read FTipoDocumento write FTipoDocumento;
    property ValorOriginal: Double read FValorOriginal write FValorOriginal;
    property ValorPago: Double read FValorPago write FValorPago;
  end;

  { TACBrTEFRespListaCB - Lista para armazenar Objetos do tipo TACBrTEFRespCB }

  TACBrTEFRespListaCB = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TObject>{$EndIf})
  protected
    FTotalTitulos: Double;
    FTotalTitulosNaoPago: Double;

    procedure SetObject(Index: Integer; Item: TACBrTEFRespCB);
    function GetObject(Index: Integer): TACBrTEFRespCB;
  public
    constructor Create(FreeObjects: Boolean);
    procedure Assign(Source: TACBrTEFRespListaCB);

    function Add(Obj: TACBrTEFRespCB): Integer;
    procedure Insert(Index: Integer; Obj: TACBrTEFRespCB);
    property Objects[Index: Integer]: TACBrTEFRespCB read GetObject write SetObject; default;

    property TotalTitulos: Double read FTotalTitulos write FTotalTitulos;
    property TotalTitulosNaoPago: Double read FTotalTitulosNaoPago write FTotalTitulosNaoPago;
  end;

  { TACBrTEFRespParcela - Definindo novo tipo para armazenar as Parcelas }

  TACBrTEFRespParcela = class
  private
    FVencimentoParcela: TDateTime;
    FValorParcela: Double;
    FNSUParcela: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrTEFRespParcela);

    property Vencimento: TDateTime read FVencimentoParcela write FVencimentoParcela;
    property Valor: Double read FValorParcela write FValorParcela;
    property NSUParcela: String read FNSUParcela write FNSUParcela;
  end;


  { TACBrTEFRespParcelas }
  { Lista de Objetos do tipo TACBrTEFParcela }

  TACBrTEFRespParcelas = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TObject>{$EndIf})
  protected
    procedure SetObject(Index: Integer; Item: TACBrTEFRespParcela);
    function GetObject(Index: Integer): TACBrTEFRespParcela;
  public
    procedure Assign(Source: TACBrTEFRespParcelas);

    function Add(Obj: TACBrTEFRespParcela): Integer;
    procedure Insert(Index: Integer; Obj: TACBrTEFRespParcela);
    property Objects[Index: Integer]: TACBrTEFRespParcela read GetObject write SetObject; default;
  end;

  { TACBrTEFRespNFCeSAT }

  TACBrTEFRespNFCeSAT = class
  private
    FCodCredenciadora: String;
    FAutorizacao: String;
    FBandeira: String;
    FCNPJCredenciadora: String;
    FDonoCartao: String;
    FDataExpiracao: String;
    FUltimosQuatroDigitos: String;

  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrTEFRespNFCeSAT);

    property CodCredenciadora: String read FCodCredenciadora write FCodCredenciadora;
    property CNPJCredenciadora: String read FCNPJCredenciadora write FCNPJCredenciadora;
    property Bandeira: String read FBandeira write FBandeira;
    property Autorizacao: String read FAutorizacao write FAutorizacao;
    property DonoCartao: String read FDonoCartao write FDonoCartao;
    property DataExpiracao: String read FDataExpiracao write FDataExpiracao;
    property UltimosQuatroDigitos: String read FUltimosQuatroDigitos write FUltimosQuatroDigitos;
  end;


  { TACBrTEFResp }

  TACBrTEFResp = class
  protected
    fpAgencia: String;
    fpAgenciaDC: String;
    fpAutenticacao: String;
    fpArqBackup: String;
    fpArqRespPendente: String;
    fpViaClienteReduzida: Boolean;
    fpBanco: String;
    fpCheque: String;
    fpChequeDC: String;
    fpCMC7: String;
    fpCNFEnviado: Boolean;
    fpCodigoAutorizacaoTransacao: String;  // Dial-13, SiTef-135. Contém o Código de Autorização para as transações de crédito (15 posições no máximo)
    fpCodigoOperadoraCelular: String;
    fpConta: String;
    fpContaDC: String;
    fpConteudo: TACBrTEFArquivo;
    fpCorrespBancarios: TACBrTEFRespListaCB;
    fpDataCheque: TDateTime;
    fpDataHoraTransacaoCancelada: TDateTime;
    fpDataHoraTransacaoComprovante: TDateTime;
    fpDataHoraTransacaoHost: TDateTime;
    fpDataHoraTransacaoLocal: TDateTime;
    fpDataPreDatado: TDateTime;
    fpDocumentoPessoa: String;
    fpFinalizacao: String;
    fpHeader: String;
    fpID: Integer;
    fpMoeda: Integer;     //  0: Real, 1: Dólar americano, 2: Euro
    fpNomeAdministradora: String;
    fpNomeOperadoraCelular: String;
    fpNSU: String;
    fpNSUTransacaoCancelada: String;
    fpNumeroLoteTransacao: Int64;
    fpNumeroRecargaCelular: String;
    fpQtdLinhasComprovante: Integer;
    fpQtdParcelas: Integer;
    fpRede: String;
    fpStatusTransacao: String;
    fpTextoEspecialCliente: String;
    fpTextoEspecialOperador: String;
    fpTipoPessoa: AnsiChar;
    fpTipoTransacao: Integer;
    fpTrailer: String;
    fpBin: String;
    fpValorTotal: Double;     // Valor Total da Transação, já considerando Descontos/ Acréscimos e Saque
    fpValorOriginal: Double;
    fpValorRecargaCelular: Double;
    fpSaque: Double;
    fpDesconto: Double;
    fpTaxaServico: Double;
    fpDocumentoVinculado: String;    // Número do documento fiscal ao qual a operação de TEF está vinculada.
    fpTipoParcelamento: Integer;
    fpParcelas: TACBrTEFRespParcelas;
    fpImagemComprovante1aVia: TStringList;
    fpImagemComprovante2aVia: TStringList;
    fpDataVencimento: TDateTime;     // Data Vencimento do Cheque
    fpInstituicao: String;     // Sitef-131, Contém um índice que indica qual a instituição que irá processar a transação segundo a tabela presente no final do documento (até 5 dígitos significativos)
    fpModalidadePagto: String;
    fpModalidadePagtoDescrita: String;
    fpModalidadePagtoExtenso: String;
    fpCodigoRedeAutorizada: String;     // 739-000 Índice da Rede Adquirente
    fpDebito: Boolean;      // Se True, foi usado Cartão de Débito
    fpCredito: Boolean;     // Se True, foi usado Cartão de Crédito
    fpVoucher: Boolean;     // Se True, foi usado Cartão Voucher
    fpDigitado: Boolean;    // Se True, foi Digitado o número do Cartão de Débito
    fpParceladoPor: TACBrTEFRespParceladoPor;
    fpValorEntradaCDC: Double;
    fpDataEntradaCDC: TDateTime;
    fpTipoOperacao: TACBrTEFRespTipoOperacao;
    fpNFCeSAT: TACBrTEFRespNFCeSAT;
    fpIdPagamento: Integer;          // 899-500 - VFP Integrador CE
    fpIdRespostaFiscal: Integer;     // 899-501 - VFP Integrador CE
    fpSerialPOS: String;             // 899-502 - VFP Integrador CE
    fpEstabelecimento: String;       // 899-503  - VFP Integrador CE
    fpCodigoBandeiraPadrao: String;  // SiTef-132, Contém um índice que indica qual o tipo do cartão quando esse tipo for identificável, segundo uma tabela a ser fornecida (5 posições)
    fpConfirmar: Boolean;     // Se Verdadeiro, deve confirmar essa transação
    fpQRCode: String;       // String com o Conteúdo do QRCode
    fpIdCarteiraDigital : String; // Código de identificação da carteira digital
    fpNomeCarteiraDigital : String; // Nome da carteira digital
    fpCodigoPSP : String; // Código do PSP - PIX
    fpNSU_TEF : String; // NSU interno, do Gerenciador TEF

    fpSucesso: Boolean;       // Se True a Transação ocorreu com sucesso

    procedure SetCNFEnviado(const AValue: Boolean);
    procedure SetArqBackup(const AValue: String);

    function GetTransacaoAprovada: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TACBrTEFResp); virtual;
    procedure ConteudoToProperty; virtual;
    procedure ProcessarTipoInterno(ALinha: TACBrTEFLinha); virtual;

    procedure Clear; virtual;
    procedure LeArquivo(const NomeArquivo: String);
    function LeInformacao(const Identificacao: Integer; const Sequencia: Integer = 0): TACBrInformacao;

    property Conteudo: TACBrTEFArquivo read fpConteudo;

    property Header: String read fpHeader;
    property ID: Integer read fpID;
    property DocumentoVinculado: String read fpDocumentoVinculado write fpDocumentoVinculado;
    property ValorTotal: Double read fpValorTotal write fpValorTotal;
    property ValorOriginal: Double read fpValorOriginal write fpValorOriginal;
    property Saque: Double read fpSaque write fpSaque;
    property Desconto: Double read fpDesconto write fpDesconto;
    property TaxaServico: Double read fpTaxaServico write fpTaxaServico;
    property Moeda: Integer read fpMoeda write fpMoeda;
    property CMC7: String read fpCMC7 write fpCMC7;
    property TipoPessoa: AnsiChar read fpTipoPessoa write fpTipoPessoa;
    property DocumentoPessoa: String read fpDocumentoPessoa write fpDocumentoPessoa;
    property DataCheque: TDateTime read fpDataCheque write fpDataCheque;
    property Rede: String read fpRede write fpRede;
    property NSU: String read fpNSU write fpNSU;
    property Finalizacao: String read fpFinalizacao write fpFinalizacao;
    property StatusTransacao: String read fpStatusTransacao write fpStatusTransacao;
    property TransacaoAprovada: Boolean read GetTransacaoAprovada;
    property TipoTransacao: Integer read fpTipoTransacao write fpTipoTransacao;
    property CodigoAutorizacaoTransacao: String read fpCodigoAutorizacaoTransacao write fpCodigoAutorizacaoTransacao;
    property NumeroLoteTransacao: Int64 read fpNumeroLoteTransacao write fpNumeroLoteTransacao;
    property DataHoraTransacaoHost: TDateTime read fpDataHoraTransacaoHost write fpDataHoraTransacaoHost;
    property DataHoraTransacaoLocal: TDateTime read fpDataHoraTransacaoLocal write fpDataHoraTransacaoLocal;
    property TipoParcelamento: Integer read fpTipoParcelamento write fpTipoParcelamento;
    property QtdParcelas: Integer read fpQtdParcelas write fpQtdParcelas;
    property DataPreDatado: TDateTime read fpDataPreDatado write fpDataPreDatado;
    property NSUTransacaoCancelada: String read fpNSUTransacaoCancelada write fpNSUTransacaoCancelada;
    property DataHoraTransacaoCancelada: TDateTime read fpDataHoraTransacaoCancelada write fpDataHoraTransacaoCancelada;
    property QtdLinhasComprovante: Integer read fpQtdLinhasComprovante write fpQtdLinhasComprovante;
    property TextoEspecialOperador: String read fpTextoEspecialOperador write fpTextoEspecialOperador;
    property TextoEspecialCliente: String read fpTextoEspecialCliente write fpTextoEspecialCliente;
    property Autenticacao: String read fpAutenticacao write fpAutenticacao;
    property Banco: String read fpBanco write fpBanco;
    property Agencia: String read fpAgencia write fpAgencia;
    property AgenciaDC: String read fpAgenciaDC write fpAgenciaDC;
    property Conta: String read fpConta write fpConta;
    property ContaDC: String read fpContaDC write fpContaDC;
    property Cheque: String read fpCheque write fpCheque;
    property ChequeDC: String read fpChequeDC write fpChequeDC;
    property NomeAdministradora: String read fpNomeAdministradora write fpNomeAdministradora;
    property DataHoraTransacaoComprovante: TDateTime read fpDataHoraTransacaoComprovante write fpDataHoraTransacaoComprovante;
    property Trailer: String read fpTrailer write fpTrailer;
    property BIN: String read fpBin write fpBin;
    property CodigoBandeiraPadrao: String read fpCodigoBandeiraPadrao write fpCodigoBandeiraPadrao;

    property CorrespBancarios: TACBrTEFRespListaCB read fpCorrespBancarios;

    property CodigoOperadoraCelular: String read fpCodigoOperadoraCelular write fpCodigoOperadoraCelular;
    property NomeOperadoraCelular: String read fpNomeOperadoraCelular write fpNomeOperadoraCelular;
    property ValorRecargaCelular: Double read fpValorRecargaCelular write fpValorRecargaCelular;
    property NumeroRecargaCelular: String read fpNumeroRecargaCelular write fpNumeroRecargaCelular;

    property Parcelas: TACBrTEFRespParcelas read fpParcelas;

    property ImagemComprovante1aVia: TStringList read fpImagemComprovante1aVia;
    property ImagemComprovante2aVia: TStringList read fpImagemComprovante2aVia;

    property ArqBackup: String read fpArqBackup write SetArqBackup;
    property ArqRespPendente: String read fpArqRespPendente write fpArqRespPendente;

    property Confirmar: Boolean read fpConfirmar write fpConfirmar;
    property CNFEnviado: Boolean read fpCNFEnviado write SetCNFEnviado;

    property ViaClienteReduzida: Boolean read fpViaClienteReduzida write fpViaClienteReduzida;

    property DataVencimento: TDateTime read fpDataVencimento write fpDataVencimento;
    property Instituicao: String read fpInstituicao write fpInstituicao;
    property ModalidadePagto: String read fpModalidadePagto write fpModalidadePagto;
    property ModalidadePagtoDescrita: String read fpModalidadePagtoDescrita write fpModalidadePagtoDescrita;
    property ModalidadePagtoExtenso: String read fpModalidadePagtoExtenso write fpModalidadePagtoExtenso;
    property CodigoRedeAutorizada: String read fpCodigoRedeAutorizada write fpCodigoRedeAutorizada;
    property Debito: Boolean read fpDebito write fpDebito;
    property Credito: Boolean read fpCredito write fpCredito;
    property Voucher: Boolean read fpVoucher write fpVoucher;
    property Digitado: Boolean read fpDigitado write fpDigitado;
    property ParceladoPor: TACBrTEFRespParceladoPor read fpParceladoPor write fpParceladoPor;
    property ValorEntradaCDC: Double read fpValorEntradaCDC write fpValorEntradaCDC;
    property DataEntradaCDC: TDateTime read fpDataEntradaCDC write fpDataEntradaCDC;
    property TipoOperacao: TACBrTEFRespTipoOperacao read fpTipoOperacao write fpTipoOperacao;
    property QRCode: String read fpQRCode write fpQRCode;
    property IdCarteiraDigital: String read fpIdCarteiraDigital write fpIdCarteiraDigital;
    property NomeCarteiraDigital: String read fpNomeCarteiraDigital write fpNomeCarteiraDigital;
    property CodigoPSP: String read fpCodigoPSP write fpCodigoPSP;
    property NSU_TEF: String read fpNSU_TEF write fpNSU_TEF;

    property NFCeSAT: TACBrTEFRespNFCeSAT read fpNFCeSAT;

    property IdPagamento: Integer read fpIdPagamento write fpIdPagamento;
    property IdRespostaFiscal: Integer read fpIdRespostaFiscal write fpIdRespostaFiscal;
    property SerialPOS: String read fpSerialPOS write fpSerialPOS;
    property Estabelecimento: String read fpEstabelecimento write fpEstabelecimento;

    property Sucesso: Boolean read fpSucesso write fpSucesso;
  end;

  { TACBrTEFRespostas }

  TACBrTEFRespostas = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TObject>{$EndIf})
  protected
    procedure SetObject(Index: Integer; Item: TACBrTEFResp);
    function GetObject(Index: Integer): TACBrTEFResp;
  public
    function Add(Obj: TACBrTEFResp): Integer;
    procedure Insert(Index: Integer; Obj: TACBrTEFResp);
    property Objects[Index: Integer]: TACBrTEFResp read GetObject write SetObject; default;
  end;

  { TACBrTEFRespostasPendentes }

  TACBrTEFRespostasPendentes = class(TACBrTEFRespostas)
  private
    function GetTotalPago: Double;
    function GetTotalDesconto: Double;
  public
    property TotalPago: Double read GetTotalPago;
    property TotalDesconto: Double read GetTotalDesconto;
  end;

  TACBrTEFRespClass = class of TACBrTEFResp;

function NomeCampo(const Identificacao: Integer; const Sequencia: Integer): String;

implementation

uses
  Math, strutils,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrConsts;

function NomeCampo(const Identificacao: Integer; const Sequencia: Integer): String;
var
  Casas: Integer;
begin
  Casas := max(Length(IntToStr(Identificacao)), 3);
  Result := IntToStrZero(Identificacao, Casas) + '-' + IntToStrZero(Sequencia, 3);
end;

{ TACBrTEFParametros }

function TACBrTEFParametros.GetValueInfo(AInfo: Word): string;
begin
  Result := Values[IntToStr(AInfo)];
end;

procedure TACBrTEFParametros.SetValueInfo(AInfo: Word; const AValue: string);
begin
  Values[IntToStr(AInfo)] := AValue;
end;

{ TACBrTEFLinha }

constructor TACBrTEFLinha.Create;
begin
  inherited;

  FACBrTEFDLinhaInformacao := TACBrInformacao.Create;
  FLinha := '';
  FIdentificacao := 0;
  FSequencia := 0;
end;

destructor TACBrTEFLinha.Destroy;
begin
  FACBrTEFDLinhaInformacao.Free;
  inherited Destroy;
end;

function TACBrTEFLinha.GetChave: String;
var
  P: Integer;
begin
  Result := '';

  if (FLinha <> '') then
  begin
    P := pos(' = ', FLinha + ' ');  // +' ' serve para que o POS funcione em Strings que nao tenham o ultimo espaço " ="
    Result := copy(FLinha, 1, P - 1);
  end
  else if (FIdentificacao <> 0) then
    Result := NomeCampo(FIdentificacao, FSequencia);
end;

function TACBrTEFLinha.GetLinha: String;
begin
  Result := '';

  if (FLinha <> '') then
    Result := FLinha
  else if (FIdentificacao <> 0) then
    Result := NomeCampo(FIdentificacao, FSequencia) + ' = ' + Informacao.AsString;
end;

procedure TACBrTEFLinha.SetLinha(const AValue: String);
var
  P: Integer;
  Chave, Valor: AnsiString;
begin
  if (FLinha = AValue) then
    exit;

  FLinha := AValue;

  P := pos(' = ', FLinha + ' ');   // +' ' serve para que o POS funcione em Strings que nao tenham o ultimo espaço " ="
  if (P = 0) then
  begin
    Informacao.AsString := '';
    FIdentificacao := 0;
    FSequencia := 0;
  end
  else
  begin
    Chave := copy(FLinha, 1, P - 1);
    Valor := copy(FLinha, P + 3, Length(FLinha));

    P := max(pos('-', Chave), 4);
    Informacao.AsString := Valor;
    FIdentificacao := StrToIntDef(copy(Chave, 1, P - 1), 0);
    FSequencia := StrToIntDef(copy(Chave, P + 1, 3), 0);
  end;
end;

{ TACBrTEFArquivo }

constructor TACBrTEFArquivo.Create;
begin
  inherited Create;

  FACBrTEFDLinha := TACBrTEFLinha.Create;
  FStringList := TStringList.Create;
  FStringList.Sorted := True;
  Clear;
end;

destructor TACBrTEFArquivo.Destroy;
begin
  FStringList.Free;
  FACBrTEFDLinha.Free;
  inherited;
end;

procedure TACBrTEFArquivo.Clear;
begin
  FStringList.Clear;
end;

procedure TACBrTEFArquivo.GravarArquivo(const NomeArquivo: String; DoFlushToDisk: Boolean = False);
begin
  //FStringList.SaveToFile(NomeArquivo);
  // em discos SD o arquivo se perdia ao reiniciar a maquina
  WriteToTXT(NomeArquivo, FStringList.Text, False, False);

  if DoFlushToDisk then
    FlushFileToDisk(NomeArquivo);
end;

procedure TACBrTEFArquivo.LeArquivo(const NomeArquivo: String);
begin
  FStringList.Clear;
  if not FilesExists(NomeArquivo) then
    raise EACBrTEFArquivo.CreateFmt( cACBrArquivoNaoEncontrado, [NomeArquivo] );

  FStringList.LoadFromFile(NomeArquivo, {$IfDef POSIX}TEncoding.Unicode{$EndIf});
end;

procedure TACBrTEFArquivo.GravaInformacao(const Chave, Informacao: AnsiString);
var
  I, IndChave: Integer;
begin
  IndChave := -1;
  I := 0;
  while (IndChave < 0) and (I < FStringList.Count) do
  begin
    if copy(FStringList[I], 1, Length(Chave) + 3) = Chave + ' = ' then
      IndChave := I
    else
      Inc(I);
  end;

  if IndChave >= 0 then
    FStringList.Delete(I);  // Remove o Antigo

  if Informacao <> '' then
    FStringList.Add(Chave + ' = ' + Informacao);
end;

procedure TACBrTEFArquivo.GravaInformacao(const Chave: AnsiString; const Informacao: TACBrInformacao);
begin
  GravaInformacao(Chave, Informacao.AsString);
end;

procedure TACBrTEFArquivo.GravaInformacao(const Identificacao: Integer; const Sequencia: Integer; const Informacao: AnsiString);
var
  I: Integer;
begin
  I := AchaLinha(Identificacao, Sequencia);
  if I >= 0 then
    FStringList.Delete(I);  // Remove o Antigo

  if Informacao <> '' then
    FStringList.Add(NomeCampo(Identificacao, Sequencia) + ' = ' + Informacao);
end;

procedure TACBrTEFArquivo.GravaInformacao(const Identificacao: Integer; const Sequencia: Integer; const Informacao: TACBrInformacao);
begin
  GravaInformacao(Identificacao, Sequencia, Informacao.AsString);
end;

function TACBrTEFArquivo.AchaLinha(const Identificacao: Integer; const Sequencia: Integer = 0): Integer;
var
  Campo: String;
  I: Integer;
begin
  Campo := NomeCampo(Identificacao, Sequencia);

  Result := -1;
  I := 0;
  while (Result < 0) and (I < FStringList.Count) do
  begin
    if copy(FStringList[I], 1, Length(Campo) + 3) = Campo + ' = ' then
      Result := I;
    Inc(I);
  end;
end;

function TACBrTEFArquivo.GetCount: Integer;
begin
  Result := Conteudo.Count;
end;

function TACBrTEFArquivo.GetLinha(Index: Integer): TACBrTEFLinha;
begin
  FACBrTEFDLinha.Linha := Conteudo[Index];
  Result := FACBrTEFDLinha;
end;

function TACBrTEFArquivo.LeLinha(const Identificacao: Integer; const Sequencia: Integer = 0): TACBrTEFLinha;
var
  I: Integer;
begin
  I := AchaLinha(Identificacao, Sequencia);

  if I > -1 then
    FACBrTEFDLinha.Linha := FStringList[I]
  else
    FACBrTEFDLinha.Linha := NomeCampo(Identificacao, Sequencia) + ' =  ';

  Result := FACBrTEFDLinha;
end;

function TACBrTEFArquivo.LeInformacao(const Identificacao: Integer; const Sequencia: Integer = 0): TACBrInformacao;
begin
  Result := LeLinha(Identificacao, Sequencia).Informacao;
end;

{ TACBrTEFRespCB }

constructor TACBrTEFRespCB.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFRespCB.Clear;
begin
  FAcrescimo := 0;
  FDataPagamento := 0;
  FDataVencimento := 0;
  FDesconto := 0;
  FDocumento := '';
  FNSUTransacaoCB := '';
  FTipoDocumento := 0;
  FValorOriginal := 0;
  FValorPago := 0;
end;

procedure TACBrTEFRespCB.Assign(Source: TACBrTEFRespCB);
begin
  FAcrescimo := Source.Acrescimo;
  FDataPagamento := Source.DataPagamento;
  FDataVencimento := Source.DataVencimento;
  FDesconto := Source.Desconto;
  FDocumento := Source.Documento;
  FNSUTransacaoCB := Source.NSUTransacaoCB;
  FTipoDocumento := Source.TipoDocumento;
  FValorOriginal := Source.ValorOriginal;
  FValorPago := Source.ValorPago;
end;

{ TACBrTEFRespListaCB }

constructor TACBrTEFRespListaCB.Create(FreeObjects: Boolean);
begin
  inherited Create(FreeObjects);

  FTotalTitulos := 0;
  FTotalTitulosNaoPago := 0;
end;

procedure TACBrTEFRespListaCB.Assign(Source: TACBrTEFRespListaCB);
var
  i: Integer;
  AItem: TACBrTEFRespCB;
begin
  Clear;
  for i := 0 to Source.Count-1 do
  begin
    AItem := TACBrTEFRespCB.Create;
    AItem.Assign(Source[i]);
    Add(AItem);
  end;
end;

procedure TACBrTEFRespListaCB.SetObject(Index: Integer; Item: TACBrTEFRespCB);
begin
  inherited Items[Index] := Item;
end;

function TACBrTEFRespListaCB.GetObject(Index: Integer): TACBrTEFRespCB;
begin
  Result := TACBrTEFRespCB(inherited Items[Index]);
end;

function TACBrTEFRespListaCB.Add(Obj: TACBrTEFRespCB): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TACBrTEFRespListaCB.Insert(Index: Integer; Obj: TACBrTEFRespCB);
begin
  inherited Insert(Index, Obj);
end;

{ TACBrTEFRespParcela }

constructor TACBrTEFRespParcela.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFRespParcela.Clear;
begin
  FValorParcela := 0;
  FVencimentoParcela := 0;
  FNSUParcela := '';
end;

procedure TACBrTEFRespParcela.Assign(Source: TACBrTEFRespParcela);
begin
  FValorParcela := Source.Valor;
  FVencimentoParcela := Source.Vencimento;
  FNSUParcela := Source.NSUParcela;
end;

{ TACBrTEFRespParcelas }

procedure TACBrTEFRespParcelas.SetObject(Index: Integer; Item: TACBrTEFRespParcela);
begin
  inherited Items[Index] := Item;
end;

procedure TACBrTEFRespParcelas.Assign(Source: TACBrTEFRespParcelas);
var
  i: Integer;
  AItem: TACBrTEFRespParcela;
begin
  Clear;
  for i := 0 to Source.Count-1 do
  begin
    AItem := TACBrTEFRespParcela.Create;
    AItem.Assign(Source[i]);
    Add(AItem);
  end;
end;

function TACBrTEFRespParcelas.GetObject(Index: Integer): TACBrTEFRespParcela;
begin
  Result := TACBrTEFRespParcela(inherited Items[Index]);
end;

function TACBrTEFRespParcelas.Add(Obj: TACBrTEFRespParcela): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TACBrTEFRespParcelas.Insert(Index: Integer; Obj: TACBrTEFRespParcela);
begin
  inherited Insert(Index, Obj);
end;

{ TACBrTEFRespNFCeSAT }

constructor TACBrTEFRespNFCeSAT.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFRespNFCeSAT.Clear;
begin
  FCodCredenciadora := '';
  FAutorizacao := '';
  FBandeira := '';
  FCNPJCredenciadora := '';
  FDonoCartao := '';
  FDataExpiracao := '';
  FUltimosQuatroDigitos := '';
end;

procedure TACBrTEFRespNFCeSAT.Assign(Source: TACBrTEFRespNFCeSAT);
begin
  FCodCredenciadora := Source.CodCredenciadora;
  FAutorizacao := Source.Autorizacao;
  FBandeira := Source.Bandeira;
  FCNPJCredenciadora := Source.CNPJCredenciadora;
  FDonoCartao := Source.DonoCartao;
  FDataExpiracao := Source.DataExpiracao;
  FUltimosQuatroDigitos := Source.UltimosQuatroDigitos;
end;

{ TACBrTEFResp }

constructor TACBrTEFResp.Create;
begin
  inherited Create;

  fpConteudo := TACBrTEFArquivo.Create;
  fpParcelas := TACBrTEFRespParcelas.Create(True);
  fpCorrespBancarios := TACBrTEFRespListaCB.Create(True);

  fpImagemComprovante1aVia := TStringList.Create;
  fpImagemComprovante2aVia := TStringList.Create;

  fpNFCeSAT := TACBrTEFRespNFCeSAT.Create;

  // Inicializa as variáveis internas //
  Clear;
end;

destructor TACBrTEFResp.Destroy;
begin
  fpConteudo.Free;
  fpParcelas.Free;
  fpCorrespBancarios.Free;

  fpImagemComprovante1aVia.Free;
  fpImagemComprovante2aVia.Free;

  fpNFCeSAT.Free;

  inherited;
end;

procedure TACBrTEFResp.Assign(Source: TACBrTEFResp);
begin
  fpConteudo.Clear;
  fpConteudo.Conteudo.Assign(Source.Conteudo.Conteudo);

  fpAgencia := Source.Agencia;
  fpAgenciaDC := Source.AgenciaDC;
  fpAutenticacao := Source.Autenticacao;
  fpArqBackup := Source.ArqBackup;
  fpArqRespPendente := Source.ArqRespPendente;
  fpViaClienteReduzida := Source.ViaClienteReduzida;
  fpBanco := Source.Banco;
  fpCheque := Source.Cheque;
  fpChequeDC := Source.ChequeDC;
  fpCMC7 := Source.CMC7;
  fpCNFEnviado := Source.CNFEnviado;
  fpCodigoAutorizacaoTransacao := Source.CodigoAutorizacaoTransacao;
  fpCodigoOperadoraCelular := Source.CodigoOperadoraCelular;
  fpConta := Source.Conta;
  fpContaDC := Source.ContaDC;
  fpDataCheque := Source.DataCheque;
  fpDataHoraTransacaoCancelada := Source.DataHoraTransacaoCancelada;
  fpDataHoraTransacaoComprovante := Source.DataHoraTransacaoComprovante;
  fpDataHoraTransacaoHost := Source.DataHoraTransacaoHost;
  fpDataHoraTransacaoLocal := Source.DataHoraTransacaoLocal;
  fpDataPreDatado := Source.DataPreDatado;
  fpDocumentoPessoa := Source.DocumentoPessoa;
  fpFinalizacao := Source.Finalizacao;
  fpHeader := Source.Header;
  fpID := Source.ID;
  fpMoeda := Source.Moeda;
  fpNomeAdministradora := Source.NomeAdministradora;
  fpNomeOperadoraCelular := Source.NomeOperadoraCelular;
  fpNSU := Source.NSU;
  fpNSUTransacaoCancelada := Source.NSUTransacaoCancelada;
  fpNumeroLoteTransacao := Source.NumeroLoteTransacao;
  fpNumeroRecargaCelular := Source.NumeroRecargaCelular;
  fpQtdLinhasComprovante := Source.QtdLinhasComprovante;
  fpQtdParcelas := Source.QtdParcelas;
  fpRede := Source.Rede;
  fpStatusTransacao := Source.StatusTransacao;
  fpTextoEspecialCliente := Source.TextoEspecialCliente;
  fpTextoEspecialOperador := Source.TextoEspecialOperador;
  fpTipoPessoa := Source.TipoPessoa;
  fpTipoTransacao := Source.TipoTransacao;
  fpTrailer := Source.Trailer;
  fpBin := Source.BIN;
  fpValorTotal := Source.ValorTotal;
  fpValorOriginal := Source.ValorOriginal;
  fpValorRecargaCelular := Source.ValorRecargaCelular;
  fpSaque := Source.Saque;
  fpDesconto := Source.Desconto;
  fpTaxaServico := Source.TaxaServico;
  fpDocumentoVinculado := Source.DocumentoVinculado;
  fpTipoParcelamento := Source.TipoParcelamento;
  fpDataVencimento := Source.DataVencimento;
  fpInstituicao := Source.Instituicao;
  fpModalidadePagto := Source.ModalidadePagto;
  fpModalidadePagtoDescrita := Source.ModalidadePagtoDescrita;
  fpModalidadePagtoExtenso := Source.ModalidadePagtoExtenso;
  fpCodigoRedeAutorizada := Source.CodigoRedeAutorizada;
  fpDebito := Source.Debito;
  fpCredito := Source.Credito;
  fpDigitado := Source.Digitado;
  fpParceladoPor := Source.ParceladoPor;
  fpValorEntradaCDC := Source.ValorEntradaCDC;
  fpDataEntradaCDC := Source.DataEntradaCDC;
  fpTipoOperacao := Source.TipoOperacao;
  fpIdPagamento := Source.IdPagamento;
  fpIdRespostaFiscal := Source.IdRespostaFiscal;
  fpSerialPOS := Source.SerialPOS;
  fpEstabelecimento := Source.Estabelecimento;
  fpCodigoBandeiraPadrao := Source.CodigoBandeiraPadrao;
  fpConfirmar := Source.Confirmar;
  fpQRCode := Source.QRCode;
  fpIdCarteiraDigital := Source.IdCarteiraDigital;
  fpNomeCarteiraDigital := Source.NomeCarteiraDigital;
  fpCodigoPSP := Source.CodigoPSP;
  fpNSU_TEF := Source.NSU_TEF;
  fpSucesso := Source.Sucesso;

  fpImagemComprovante1aVia.Text := Source.ImagemComprovante1aVia.Text;
  fpImagemComprovante2aVia.Text := Source.ImagemComprovante2aVia.Text;

  fpCorrespBancarios.Assign(Source.CorrespBancarios);
  fpNFCeSAT.Assign(Source.NFCeSAT);
  fpParcelas.Assign(Source.Parcelas);

  // O Método abaixo, se existis, dá preferencia aos valores de "Conteudo"
  ConteudoToProperty;
end;

procedure TACBrTEFResp.ConteudoToProperty;
begin
  { abstract }
end;

procedure TACBrTEFResp.Clear;
begin
  fpConteudo.Clear;
  fpParcelas.Clear;
  fpCorrespBancarios.Clear;
  fpImagemComprovante1aVia.Clear;
  fpImagemComprovante2aVia.Clear;

  fpHeader := '';
  fpID := 0;
  fpAgencia := '';
  fpAgenciaDC := '';
  fpAutenticacao := '';
  fpBanco := '';
  fpCheque := '';
  fpChequeDC := '';
  fpCMC7 := '';
  fpCodigoAutorizacaoTransacao := '';
  fpConta := '';
  fpContaDC := '';
  fpDataCheque := 0;
  fpDataHoraTransacaoCancelada := 0;
  fpDataHoraTransacaoComprovante := 0;
  fpDataHoraTransacaoHost := 0;
  fpDataHoraTransacaoLocal := 0;
  fpDataPreDatado := 0;
  fpDocumentoPessoa := '';
  fpFinalizacao := '';
  fpMoeda := 0;
  fpNomeAdministradora := '';
  fpNSU := '';
  fpNSUTransacaoCancelada := '';
  fpNumeroLoteTransacao := 0;
  fpQtdLinhasComprovante := 0;
  fpQtdParcelas := 0;
  fpRede := '';
  fpStatusTransacao := '';
  fpTextoEspecialCliente := '';
  fpTextoEspecialOperador := '';
  fpTipoPessoa := ' ';
  fpTipoTransacao := 0;
  fpTrailer := '';
  fpBin := '';
  fpValorTotal := 0;
  fpValorOriginal := 0;
  fpSaque := 0;
  fpDesconto := 0;
  fpTaxaServico := 0;
  fpDocumentoVinculado := '';
  fpTipoParcelamento := 0;
  fpValorEntradaCDC := 0;
  fpDataEntradaCDC := 0;
  fpCodigoBandeiraPadrao := '';
  fpQRCode := '';

  fpCodigoOperadoraCelular := '';
  fpNomeOperadoraCelular := '';
  fpNumeroRecargaCelular := '';
  fpValorRecargaCelular := 0;

  fpParceladoPor := parcNenhum;
  fpTipoOperacao := opOutras;

  fpCredito := False;
  fpDebito := False;
  fpDigitado := False;

  fpConfirmar := False;
  fpCNFEnviado := False;

  fpArqBackup := '';
  fpArqRespPendente := '';
  fpViaClienteReduzida := False;

  fpNFCeSAT.Clear;
  fpIdPagamento := 0;
  fpIdRespostaFiscal := 0;
  fpSerialPOS := '';
  fpEstabelecimento := '';

  fpIdCarteiraDigital :=  '';
  fpNomeCarteiraDigital := '';
  fpCodigoPSP :=  '';
  fpNSU_TEF :=  '';
  fpDataVencimento := 0;
  fpInstituicao := '';
  fpModalidadePagto := '';
  fpModalidadePagtoDescrita := '';
  fpModalidadePagtoExtenso := '';
  fpCodigoRedeAutorizada := '';
  fpSucesso := False;
end;

procedure TACBrTEFResp.LeArquivo(const NomeArquivo: String);
begin
  Clear;
  Conteudo.LeArquivo(NomeArquivo);

  ConteudoToProperty;
end;

procedure TACBrTEFResp.SetCNFEnviado(const AValue: Boolean);
begin
  fpConteudo.GravaInformacao(899, 1, IfThen(AValue, 'S', 'N'));
  fpCNFEnviado := AValue;
end;

procedure TACBrTEFResp.SetArqBackup(const AValue: String);
begin
  fpArqBackup := Trim(AValue);
end;

function TACBrTEFResp.GetTransacaoAprovada: Boolean;
begin
  Result := Self.Sucesso;   { Abstrata }
end;

procedure TACBrTEFResp.ProcessarTipoInterno(ALinha: TACBrTEFLinha);
begin
  if (ALinha.Identificacao = 899) then
  begin
    case ALinha.Sequencia of
        1 : fpCNFEnviado         := (UpperCase( ALinha.Informacao.AsString ) = 'S' );
      100 : fpHeader             := ALinha.Informacao.AsString;
      101 : fpID                 := ALinha.Informacao.AsInteger;
      102 : fpDocumentoVinculado := ALinha.Informacao.AsString;
      104 : fpRede               := ALinha.Informacao.AsString ;
      103 : fpValorTotal         := fpValorTotal + ALinha.Informacao.AsFloat;
      500 : fpIdPagamento        := ALinha.Informacao.AsInteger ;
      501 : fpIdRespostaFiscal   := ALinha.Informacao.AsInteger ;
      502 : fpSerialPOS          := ALinha.Informacao.AsString ;
      503 : fpEstabelecimento    := ALinha.Informacao.AsString ;
    end;
  end;
end;

function TACBrTEFResp.LeInformacao(const Identificacao: Integer; const Sequencia: Integer): TACBrInformacao;
begin
  Result := Conteudo.LeInformacao(Identificacao, Sequencia);
end;

{ TACBrTEFRespostas }

function TACBrTEFRespostas.Add(Obj: TACBrTEFResp): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TACBrTEFRespostas.Insert(Index: Integer; Obj: TACBrTEFResp);
begin
  inherited Insert(Index, Obj);
end;

function TACBrTEFRespostas.GetObject(Index: Integer): TACBrTEFResp;
begin
  Result := TACBrTEFResp(inherited Items[Index]);
end;

procedure TACBrTEFRespostas.SetObject(Index: Integer; Item: TACBrTEFResp);
begin
  inherited Items[Index] := Item;
end;

{ TACBrTEFRespostasPendentes }

function TACBrTEFRespostasPendentes.GetTotalPago: Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    with TACBrTEFResp(Items[I]) do
    begin
      Result := Result + (ValorTotal - Saque);
    end;
  end;

  Result := RoundTo(Result, -2);
end;

function TACBrTEFRespostasPendentes.GetTotalDesconto: Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    with TACBrTEFResp(Items[I]) do
    begin
      Result := Result + Desconto;
    end;
  end;

  Result := RoundTo(Result, -2) * -1;
end;

end.



