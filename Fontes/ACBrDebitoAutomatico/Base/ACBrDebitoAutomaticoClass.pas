{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDebitoAutomaticoClass;

interface

uses
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  Controls, SysUtils, Classes, Contnrs,
  ACBrDebitoAutomaticoConversao;

type
  TGeral = class(TObject)
  private
    FTipoArquivo: TDebitoTipoArquivo;
    FBanco: TBanco;
    FLayoutVersao: TDebitoLayoutVersao;
  public
    property TipoArquivo: TDebitoTipoArquivo read FTipoArquivo write FTipoArquivo;
    property Banco: TBanco read FBanco write FBanco;
    property LayoutVersao: TDebitoLayoutVersao read FLayoutVersao write FLayoutVersao;
  end;

  TArquivoTXT = class(TObject)
  private
    FArquivoTXT: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property ArquivoTXT: TStringList read FArquivoTXT write FArquivoTXT;
  end;

  TArquivoTXTList = class(TObjectList)
  private
    function GetItem(Index: Integer): TArquivoTXT;
    procedure SetItem(Index: Integer; Value: TArquivoTXT);
  public
    function New: TArquivoTXT;
    function Last: TArquivoTXT;
    property Items[Index: Integer]: TArquivoTXT read GetItem write SetItem; default;
  end;

  TAviso = class(TObject)
  private
    FCodigoRetorno: string;
    FMensagemRetorno: string;
    FSegmento: string;
    FIdentificacao: string;
  public
    property CodigoRetorno: string read FCodigoRetorno write FCodigoRetorno;
    property MensagemRetorno: string read FMensagemRetorno write FMensagemRetorno;
    property Segmento: string read FSegmento write FSegmento;
    property Identificacao: string read FIdentificacao write FIdentificacao;
  end;

  TAvisoList = class(TObjectList)
  private
    function GetItem(Index: Integer): TAviso;
    procedure SetItem(Index: Integer; Value: TAviso);
  public
    function New: TAviso;
    function Last: TAviso;
    property Items[Index: Integer]: TAviso read GetItem write SetItem; default;
  end;

  TRegistroA = class(TObject)
  private
    FCodigoRemessa: TDebitoTipoArquivo;
    FCodigoConvenio: string;
    FNomeEmpresa: string; // Versão 8 = Nome da Destinatária
    FCodigoBanco: Integer; // Versão 8 = Código da Depositaria
    FNomeBanco: string; // Versão 8 = Nome da Depositaria
    FGeracao: TDateTime;
    FNSA: Integer;
    FLayoutVersao: TDebitoLayoutVersao;
    FAviso: TAvisoList;
  public
    constructor Create;
    destructor Destroy; override;

    property CodigoRemessa: TDebitoTipoArquivo read FCodigoRemessa write FCodigoRemessa;
    property CodigoConvenio: string read FCodigoConvenio write FCodigoConvenio;
    property NomeEmpresa: string read FNomeEmpresa write FNomeEmpresa;
    property CodigoBanco: Integer read FCodigoBanco write FCodigoBanco;
    property NomeBanco: string read FNomeBanco write FNomeBanco;
    property Geracao: TDateTime read FGeracao write FGeracao;
    property NSA: Integer read FNSA write FNSA;
    property LayoutVersao: TDebitoLayoutVersao read FLayoutVersao write FLayoutVersao;
    property Aviso: TAvisoList read FAviso write FAviso;
  end;

  TRegistroB = class(TObject)
  private
    FIdentificacaoClienteEmpresa: string; // Versão 8 = Identificação do Cliente na Destinatária
    FAgenciaDebito: string;
    FIdentificacaoClienteBanco: string; // Versão 8 = Identificação do Cliente na Destinatária
    FDataOpcaoExclusao: TDateTime;
    FCodigoMovimento: TDebitoMovimentoCadastro;
  public
    property IdentificacaoClienteEmpresa: string read FIdentificacaoClienteEmpresa write FIdentificacaoClienteEmpresa;
    property AgenciaDebito: string read FAgenciaDebito write FAgenciaDebito;
    property IdentificacaoClienteBanco: string read FIdentificacaoClienteBanco write FIdentificacaoClienteBanco;
    property DataOpcaoExclusao: TDateTime read FDataOpcaoExclusao write FDataOpcaoExclusao;
    property CodigoMovimento: TDebitoMovimentoCadastro read FCodigoMovimento write FCodigoMovimento;
  end;

  TRegistroBList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroB;
    procedure SetItem(Index: Integer; Value: TRegistroB);
  public
    function New: TRegistroB;
    function First: TRegistroB;
    function Last: TRegistroB;
    property Items[Index: Integer]: TRegistroB read GetItem write SetItem; default;
  end;

  TRegistroC = class(TObject)
  private
    FIdentificacaoClienteEmpresa: string; // Versão 8 = Identificação do Cliente na Destinatária
    FAgenciaDebito: string;
    FIdentificacaoClienteBanco: string; // Versão 8 = Identificação do Cliente na Depositária
    FOcorrencia1: string;
    FOcorrencia2: string;
    FCodigoMovimento: TDebitoMovimentoCadastro;
  public
    property IdentificacaoClienteEmpresa: string read FIdentificacaoClienteEmpresa write FIdentificacaoClienteEmpresa;
    property AgenciaDebito: string read FAgenciaDebito write FAgenciaDebito;
    property IdentificacaoClienteBanco: string read FIdentificacaoClienteBanco write FIdentificacaoClienteBanco;
    property Ocorrencia1: string read FOcorrencia1 write FOcorrencia1;
    property Ocorrencia2: string read FOcorrencia2 write FOcorrencia2;
    property CodigoMovimento: TDebitoMovimentoCadastro read FCodigoMovimento write FCodigoMovimento;
  end;

  TRegistroCList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroC;
    procedure SetItem(Index: Integer; Value: TRegistroC);
  public
    function New: TRegistroC;
    function First: TRegistroC;
    function Last: TRegistroC;
    property Items[Index: Integer]: TRegistroC read GetItem write SetItem; default;
  end;

  TRegistroD = class(TObject)
  private
    FIdentificacaoClienteEmpresaAnterior: string; // Versão 8 = Identificação do Cliente na Destinatária - Anterior
    FAgenciaDebito: string;
    FIdentificacaoClienteBanco: string; // Versão 8 = Identificação do Cliente no Depositária
    FIdentificacaoClienteEmpresaAtual: string; // Versão 8 = Identificação do Cliente na Destinatária - Atual
    FOcorrencia: string;
    FNovaDataVencAutorizacao: TDateTime; // Versão 8 = Nova data de vencimento da autorização
    FAlteracaoOpcaoUsoChequeEspecial: TDebitoAlteracaoOpcao; // Versão 8 = Alteração da Opção de uso do cheque especial
    FAlteracaoOpcaoDebParcialIntegralPosVenc: TDebitoAlteracaoOpcao; // Campo da Versão 8 = Alteração da Opção de débito parcial ou integral após o vencimento
    FCodigoMovimento: TDebitoMovimentoAlteracao;
  public
    property IdentificacaoClienteEmpresaAnterior: string read FIdentificacaoClienteEmpresaAnterior write FIdentificacaoClienteEmpresaAnterior;
    property AgenciaDebito: string read FAgenciaDebito write FAgenciaDebito;
    property IdentificacaoClienteBanco: string read FIdentificacaoClienteBanco write FIdentificacaoClienteBanco;
    property IdentificacaoClienteEmpresaAtual: string read FIdentificacaoClienteEmpresaAtual write FIdentificacaoClienteEmpresaAtual;
    property Ocorrencia: string read FOcorrencia write FOcorrencia;
    property NovaDataVencAutorizacao: TDateTime read FNovaDataVencAutorizacao write FNovaDataVencAutorizacao;
    property AlteracaoOpcaoUsoChequeEspecial: TDebitoAlteracaoOpcao read FAlteracaoOpcaoUsoChequeEspecial write FAlteracaoOpcaoUsoChequeEspecial;
    property AlteracaoOpcaoDebParcialIntegralPosVenc: TDebitoAlteracaoOpcao read FAlteracaoOpcaoDebParcialIntegralPosVenc write FAlteracaoOpcaoDebParcialIntegralPosVenc;
    property CodigoMovimento: TDebitoMovimentoAlteracao read FCodigoMovimento write FCodigoMovimento;
  end;

  TRegistroDList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroD;
    procedure SetItem(Index: Integer; Value: TRegistroD);
  public
    function New: TRegistroD;
    function First: TRegistroD;
    function Last: TRegistroD;
    property Items[Index: Integer]: TRegistroD read GetItem write SetItem; default;
  end;

  TRegistroE = class(TObject)
  private
    FIdentificacaoClienteEmpresa: string; // Versão 8 = Identificação do Cliente na Destinatária
    FAgenciaDebito: string;
    FIdentificacaoClienteBanco: string; // Versão 8 = Identificação do Cliente na Depositária
    FVencimento: TDateTime; // Versão 8 = Data do Vencimento ou prazo de validade do contrato
    FValor: Double;
    FDebitoMoeda: TDebitoMoeda;
    FUsoEmpresa: string; // Versão 8 = Uso da Instituição Destinatária
    FUsoEmpresaValorTotalTributos: Double;
    FUsoEmpresaXY: TDebitoUsoEmpresaXY;
    FTipoIdentificacao: TDebitoCPFCNPJ;
    FCPFCNPJ: string;
    FTipoOperacao: TDebitoTipoOperacao;
    FUtilizacaoChequeEspecial: TDebitoSimNao;
    FOpcaoDebParcialIntegralPosVenc: TDebitoSimNao;
    FCodigoMovimento: TDebitoMovimentoDebito;
  public
    property IdentificacaoClienteEmpresa: string read FIdentificacaoClienteEmpresa write FIdentificacaoClienteEmpresa;
    property AgenciaDebito: string read FAgenciaDebito write FAgenciaDebito;
    property IdentificacaoClienteBanco: string read FIdentificacaoClienteBanco write FIdentificacaoClienteBanco;
    property Vencimento: TDateTime read FVencimento write FVencimento;
    property Valor: Double read FValor write FValor;
    property DebitoMoeda: TDebitoMoeda read FDebitoMoeda write FDebitoMoeda;
    property UsoEmpresa: string read FUsoEmpresa write FUsoEmpresa;
    property UsoEmpresaValorTotalTributos: Double read FUsoEmpresaValorTotalTributos write FUsoEmpresaValorTotalTributos;
    property UsoEmpresaXY: TDebitoUsoEmpresaXY read FUsoEmpresaXY write FUsoEmpresaXY;
    property TipoIdentificacao: TDebitoCPFCNPJ read FTipoIdentificacao write FTipoIdentificacao;
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property TipoOperacao: TDebitoTipoOperacao read FTipoOperacao write FTipoOperacao;
    property UtilizacaoChequeEspecial: TDebitoSimNao read FUtilizacaoChequeEspecial write FUtilizacaoChequeEspecial;
    property OpcaoDebParcialIntegralPosVenc: TDebitoSimNao read FOpcaoDebParcialIntegralPosVenc write FOpcaoDebParcialIntegralPosVenc;
    property CodigoMovimento: TDebitoMovimentoDebito read FCodigoMovimento write FCodigoMovimento;
  end;

  TRegistroEList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroE;
    procedure SetItem(Index: Integer; Value: TRegistroE);
  public
    function New: TRegistroE;
    function First: TRegistroE;
    function Last: TRegistroE;
    property Items[Index: Integer]: TRegistroE read GetItem write SetItem; default;
  end;

  TRegistroF = class(TObject)
  private
    FIdentificacaoClienteEmpresa: string; // Versão 8 = Identificação do Cliente na Destinatária
    FAgenciaDebito: string;
    FIdentificacaoClienteBanco: string; // Versão 8 = Identificação do Cliente na Depositária
    FVencimento: TDateTime;
    FValor: Double;
    FCodigoRetorno: TDebitoRetorno;
    FDescRetorno: string;
    FUsoEmpresa: string;
    FTipoIdentificacao: TDebitoCPFCNPJ;
    FCPFCNPJ: string;
    FCodigoMovimento: TDebitoMovimentoDebito;
  public
    property IdentificacaoClienteEmpresa: string read FIdentificacaoClienteEmpresa write FIdentificacaoClienteEmpresa;
    property AgenciaDebito: string read FAgenciaDebito write FAgenciaDebito;
    property IdentificacaoClienteBanco: string read FIdentificacaoClienteBanco write FIdentificacaoClienteBanco;
    property Vencimento: TDateTime read FVencimento write FVencimento;
    property Valor: Double read FValor write FValor;
    property CodigoRetorno: TDebitoRetorno read FCodigoRetorno write FCodigoRetorno;
    property DescRetorno: string read FDescRetorno write FDescRetorno;
    property UsoEmpresa: string read FUsoEmpresa write FUsoEmpresa;
    property TipoIdentificacao: TDebitoCPFCNPJ read FTipoIdentificacao write FTipoIdentificacao;
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property CodigoMovimento: TDebitoMovimentoDebito read FCodigoMovimento write FCodigoMovimento;
  end;

  TRegistroFList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroF;
    procedure SetItem(Index: Integer; Value: TRegistroF);
  public
    function New: TRegistroF;
    function First: TRegistroF;
    function Last: TRegistroF;
    property Items[Index: Integer]: TRegistroF read GetItem write SetItem; default;
  end;

  TRegistroH = class(TObject)
  private
    FIdentificacaoClienteEmpresaAnterior: string;
    FAgenciaDebito: string;
    FIdentificacaoClienteBanco: string;
    FIdentificacaoClienteEmpresaAtual: string;
    FOcorrencia: string;
    FOcorrenciaCancelamento: TDebitoRetorno;
    FOcorrenciaDataVigencia: TDebitoRetorno;
    FOcorrenciaAlteracaoOpcaoUsoChequeEspecial: TDebitoRetorno;
    FOcorrenciaAlteracaoOpcaoDebPosVenc: TDebitoRetorno;
    FCodigoMovimento: TDebitoMovimentoAlteracao;
    FDescRetornoCanc: string;
    FDescRetornoDatVig: string;
    FDescRetornoChequeEsp: string;
    FDescRetornoPosVenc: string;
  public
    property IdentificacaoClienteEmpresaAnterior: string read FIdentificacaoClienteEmpresaAnterior write FIdentificacaoClienteEmpresaAnterior;
    property AgenciaDebito: string read FAgenciaDebito write FAgenciaDebito;
    property IdentificacaoClienteBanco: string read FIdentificacaoClienteBanco write FIdentificacaoClienteBanco;
    property IdentificacaoClienteEmpresaAtual: string read FIdentificacaoClienteEmpresaAtual write FIdentificacaoClienteEmpresaAtual;
    property Ocorrencia: string read FOcorrencia write FOcorrencia;
    property OcorrenciaCancelamento: TDebitoRetorno read FOcorrenciaCancelamento write FOcorrenciaCancelamento;
    property OcorrenciaDataVigencia: TDebitoRetorno read FOcorrenciaDataVigencia write FOcorrenciaDataVigencia;
    property OcorrenciaAlteracaoOpcaoUsoChequeEspecial: TDebitoRetorno read FOcorrenciaAlteracaoOpcaoUsoChequeEspecial write FOcorrenciaAlteracaoOpcaoUsoChequeEspecial;
    property OcorrenciaAlteracaoOpcaoDebPosVenc: TDebitoRetorno read FOcorrenciaAlteracaoOpcaoDebPosVenc write FOcorrenciaAlteracaoOpcaoDebPosVenc;
    property CodigoMovimento: TDebitoMovimentoAlteracao read FCodigoMovimento write FCodigoMovimento;
    property DescRetornoCanc: string read FDescRetornoCanc write FDescRetornoCanc;
    property DescRetornoDatVig: string read FDescRetornoDatVig write FDescRetornoDatVig;
    property DescRetornoChequeEsp: string read FDescRetornoChequeEsp write FDescRetornoChequeEsp;
    property DescRetornoPosVenc: string read FDescRetornoPosVenc write FDescRetornoPosVenc;
  end;

  TRegistroHList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroH;
    procedure SetItem(Index: Integer; Value: TRegistroH);
  public
    function New: TRegistroH;
    function First: TRegistroH;
    function Last: TRegistroH;
    property Items[Index: Integer]: TRegistroH read GetItem write SetItem; default;
  end;

  TRegistroI = class(TObject)
  private
    FIdentificacaoClienteEmpresa: string;
    FTipoIdentificacao: TDebitoCPFCNPJ;
    FCPFCNPJ: string;
    FNome: string;
    FCidade: string;
    FUF: string;
  public
    property IdentificacaoClienteEmpresa: string read FIdentificacaoClienteEmpresa write FIdentificacaoClienteEmpresa;
    property TipoIdentificacao: TDebitoCPFCNPJ read FTipoIdentificacao write FTipoIdentificacao;
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
    property Nome: string read FNome write FNome;
    property Cidade: string read FCidade write FCidade;
    property UF: string read FUF write FUF;
  end;

  TRegistroIList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroI;
    procedure SetItem(Index: Integer; Value: TRegistroI);
  public
    function New: TRegistroI;
    function First: TRegistroI;
    function Last: TRegistroI;
    property Items[Index: Integer]: TRegistroI read GetItem write SetItem; default;
  end;

  TRegistroJ = class(TObject)
  private
    FNSA: Integer;
    FGeracao: TDateTime;
    FTotalRegistros: Integer;
    FValorTotal: Double;
    FProcessamento: TDateTime;
  public
    property NSA: Integer read FNSA write FNSA;
    property Geracao: TDateTime read FGeracao write FGeracao;
    property TotalRegistros: Integer read FTotalRegistros write FTotalRegistros;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property Processamento: TDateTime read FProcessamento write FProcessamento;
  end;

  TRegistroJList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroJ;
    procedure SetItem(Index: Integer; Value: TRegistroJ);
  public
    function New: TRegistroJ;
    function First: TRegistroJ;
    function Last: TRegistroJ;
    property Items[Index: Integer]: TRegistroJ read GetItem write SetItem; default;
  end;

  TRegistroK = class(TObject)
  private
    FIdentificacaoClienteEmpresa: string;
    FAgenciaDebito: string;
    FIdentificacaoClienteBanco: string;
    FTipoTratamento: Integer;
    FValor: Double;
    FCodigoReceita: Integer;
    FTipoIdentificacao: TDebitoCPFCNPJ;
    FCPFCNPJ: string;
  public
    property IdentificacaoClienteEmpresa: string read FIdentificacaoClienteEmpresa write FIdentificacaoClienteEmpresa;
    property AgenciaDebito: string read FAgenciaDebito write FAgenciaDebito;
    property IdentificacaoClienteBanco: string read FIdentificacaoClienteBanco write FIdentificacaoClienteBanco;
    property TipoTratamento: Integer read FTipoTratamento write FTipoTratamento;
    property Valor: Double read FValor write FValor;
    property CodigoReceita: Integer read FCodigoReceita write FCodigoReceita;
    property TipoIdentificacao: TDebitoCPFCNPJ read FTipoIdentificacao write FTipoIdentificacao;
    property CPFCNPJ: string read FCPFCNPJ write FCPFCNPJ;
  end;

  TRegistroKList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroK;
    procedure SetItem(Index: Integer; Value: TRegistroK);
  public
    function New: TRegistroK;
    function First: TRegistroK;
    function Last: TRegistroK;
    property Items[Index: Integer]: TRegistroK read GetItem write SetItem; default;
  end;

  TRegistroL = class(TObject)
  private
    FFaturamentoContas: TDateTime;
    FVencimentoFatura: TDateTime;
    FRemessaArquivoBanco: TDateTime;
    FRemessaContasFisicas: TDateTime;
  public
    property FaturamentoContas: TDateTime read FFaturamentoContas write FFaturamentoContas;
    property VencimentoFatura: TDateTime read FVencimentoFatura write FVencimentoFatura;
    property RemessaArquivoBanco: TDateTime read FRemessaArquivoBanco write FRemessaArquivoBanco;
    property RemessaContasFisicas: TDateTime read FRemessaContasFisicas write FRemessaContasFisicas;
  end;

  TRegistroLList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroL;
    procedure SetItem(Index: Integer; Value: TRegistroL);
  public
    function New: TRegistroL;
    function First: TRegistroL;
    function Last: TRegistroL;
    property Items[Index: Integer]: TRegistroL read GetItem write SetItem; default;
  end;

  TRegistroT = class(TObject)
  private
    FTotal: Integer;
    FValorTotal: Double;
  public
    property Total: Integer read FTotal write FTotal;
    property ValorTotal: Double read FValorTotal write FValorTotal;
  end;

  TRegistroTList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroT;
    procedure SetItem(Index: Integer; Value: TRegistroT);
  public
    function New: TRegistroT;
    function First: TRegistroT;
    function Last: TRegistroT;
    property Items[Index: Integer]: TRegistroT read GetItem write SetItem; default;
  end;

  TRegistroX = class(TObject)
  private
    FCodigoAgencia: string;
    FNomeAgencia: string;
    FEnderecoAgencia: string;
    FNumero: string;
    FCEP: string;
    FNomeCidade: string;
    FUF: string;
    FSituacaoAgencia: TDebitoSituacaoAgencia;
  public
    property CodigoAgencia: string read FCodigoAgencia write FCodigoAgencia;
    property NomeAgencia: string read FNomeAgencia write FNomeAgencia;
    property EnderecoAgencia: string read FEnderecoAgencia write FEnderecoAgencia;
    property Numero: string read FNumero write FNumero;
    property CEP: string read FCEP write FCEP;
    property NomeCidade: string read FNomeCidade write FNomeCidade;
    property UF: string read FUF write FUF;
    property SituacaoAgencia: TDebitoSituacaoAgencia read FSituacaoAgencia write FSituacaoAgencia;
  end;

  TRegistroXList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRegistroX;
    procedure SetItem(Index: Integer; Value: TRegistroX);
  public
    function New: TRegistroX;
    function First: TRegistroX;
    function Last: TRegistroX;
    property Items[Index: Integer]: TRegistroX read GetItem write SetItem; default;
  end;

  TRegistroZ = class(TObject)
  private
    FTotal: Integer;
    FValorTotal: Double;
  public
    property Total: Integer read FTotal write FTotal;
    property ValorTotal: Double read FValorTotal write FValorTotal;
  end;

  TDebitoAutomatico = class(TPersistent)
  private
    FAtivo: Boolean;
    FNomeArq: string;
    FGeral: TGeral;
    FRegistroA: TRegistroA;
    FRegistroB: TRegistroBList;
    FRegistroC: TRegistroCList;
    FRegistroD: TRegistroDList;
    FRegistroE: TRegistroEList;
    FRegistroF: TRegistroFList;
    FRegistroH: TRegistroHList;
    FRegistroI: TRegistroIList;
    FRegistroJ: TRegistroJList;
    FRegistroK: TRegistroKList;
    FRegistroL: TRegistroLList;
    FRegistroT: TRegistroTList;
    FRegistroX: TRegistroXList;
    FRegistroZ: TRegistroZ;

    procedure SetRegistroB(const Value: TRegistroBList);
    procedure SetRegistroC(const Value: TRegistroCList);
    procedure SetRegistroD(const Value: TRegistroDList);
    procedure SetRegistroE(const Value: TRegistroEList);
    procedure SetRegistroF(const Value: TRegistroFList);
    procedure SetRegistroH(const Value: TRegistroHList);
    procedure SetRegistroI(const Value: TRegistroIList);
    procedure SetRegistroJ(const Value: TRegistroJList);
    procedure SetRegistroK(const Value: TRegistroKList);
    procedure SetRegistroL(const Value: TRegistroLList);
    procedure SetRegistroT(const Value: TRegistroTList);
    procedure SetRegistroX(const Value: TRegistroXList);
  public
    constructor Create;
    destructor Destroy; override;

    property Ativo: Boolean read FAtivo write FAtivo;
    property NomeArq: string read FNomeArq write FNomeArq;
    property Geral: TGeral read FGeral write FGeral;

    property RegistroA: TRegistroA read FRegistroA write FRegistroA;
    property RegistroB: TRegistroBList read FRegistroB write SetRegistroB;
    property RegistroC: TRegistroCList read FRegistroC write SetRegistroC;
    property RegistroD: TRegistroDList read FRegistroD write SetRegistroD;
    property RegistroE: TRegistroEList read FRegistroE write SetRegistroE;
    property RegistroF: TRegistroFList read FRegistroF write SetRegistroF;
    property RegistroH: TRegistroHList read FRegistroH write SetRegistroH;
    property RegistroI: TRegistroIList read FRegistroI write SetRegistroI;
    property RegistroJ: TRegistroJList read FRegistroJ write SetRegistroJ;
    property RegistroK: TRegistroKList read FRegistroK write SetRegistroK;
    property RegistroL: TRegistroLList read FRegistroL write SetRegistroL;
    property RegistroT: TRegistroTList read FRegistroT write SetRegistroT;
    property RegistroX: TRegistroXList read FRegistroX write SetRegistroX;
    property RegistroZ: TRegistroZ read FRegistroZ write FRegistroZ;
  end;

implementation

{ TArquivoTXT }

constructor TArquivoTXT.Create;
begin
  inherited Create;

  FArquivoTXT := TStringList.Create;
end;

destructor TArquivoTXT.Destroy;
begin
  FArquivoTXT.Free;

  inherited Destroy;
end;

{ TArquivoTXTList }

function TArquivoTXTList.New: TArquivoTXT;
begin
  Result := TArquivoTXT.Create;
  Add(Result);
end;

function TArquivoTXTList.GetItem(Index: Integer): TArquivoTXT;
begin
  Result := TArquivoTXT(inherited GetItem(Index));
end;

function TArquivoTXTList.Last: TArquivoTXT;
begin
  Result := TArquivoTXT(inherited Last);
end;

procedure TArquivoTXTList.SetItem(Index: Integer; Value: TArquivoTXT);
begin
  inherited SetItem(Index, Value);
end;

{ TAvisoList }

function TAvisoList.New: TAviso;
begin
  Result := TAviso.Create;
  Add(Result);
end;

function TAvisoList.GetItem(Index: Integer): TAviso;
begin
  Result := TAviso(inherited GetItem(Index));
end;

function TAvisoList.Last: TAviso;
begin
  Result := TAviso(inherited Last);
end;

procedure TAvisoList.SetItem(Index: Integer; Value: TAviso);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroA }

constructor TRegistroA.Create;
begin
  inherited Create;

  FAviso   := TAvisoList.Create;
end;

destructor TRegistroA.Destroy;
begin
  FAviso.Free;

  inherited Destroy;
end;

{ TRegistroBList }

function TRegistroBList.First: TRegistroB;
begin
  Result := TRegistroB(inherited First);
end;

function TRegistroBList.GetItem(Index: Integer): TRegistroB;
begin
  Result := TRegistroB(inherited GetItem(Index));
end;

function TRegistroBList.Last: TRegistroB;
begin
  Result := TRegistroB(inherited Last);
end;

function TRegistroBList.New: TRegistroB;
begin
  Result := TRegistroB.Create;
  Add(Result);
end;

procedure TRegistroBList.SetItem(Index: Integer; Value: TRegistroB);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroCList }

function TRegistroCList.First: TRegistroC;
begin
  Result := TRegistroC(inherited First);
end;

function TRegistroCList.GetItem(Index: Integer): TRegistroC;
begin
  Result := TRegistroC(inherited GetItem(Index));
end;

function TRegistroCList.Last: TRegistroC;
begin
  Result := TRegistroC(inherited Last);
end;

function TRegistroCList.New: TRegistroC;
begin
  Result := TRegistroC.Create;
  Add(Result);
end;

procedure TRegistroCList.SetItem(Index: Integer; Value: TRegistroC);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroDList }

function TRegistroDList.First: TRegistroD;
begin
  Result := TRegistroD(inherited First);
end;

function TRegistroDList.GetItem(Index: Integer): TRegistroD;
begin
  Result := TRegistroD(inherited GetItem(Index));
end;

function TRegistroDList.Last: TRegistroD;
begin
  Result := TRegistroD(inherited Last);
end;

function TRegistroDList.New: TRegistroD;
begin
  Result := TRegistroD.Create;
  Add(Result);
end;

procedure TRegistroDList.SetItem(Index: Integer; Value: TRegistroD);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroEList }

function TRegistroEList.First: TRegistroE;
begin
  Result := TRegistroE(inherited First);
end;

function TRegistroEList.GetItem(Index: Integer): TRegistroE;
begin
  Result := TRegistroE(inherited GetItem(Index));
end;

function TRegistroEList.Last: TRegistroE;
begin
  Result := TRegistroE(inherited Last);
end;

function TRegistroEList.New: TRegistroE;
begin
  Result := TRegistroE.Create;
  Add(Result);
end;

procedure TRegistroEList.SetItem(Index: Integer; Value: TRegistroE);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroFList }

function TRegistroFList.First: TRegistroF;
begin
  Result := TRegistroF(inherited First);
end;

function TRegistroFList.GetItem(Index: Integer): TRegistroF;
begin
  Result := TRegistroF(inherited GetItem(Index));
end;

function TRegistroFList.Last: TRegistroF;
begin
  Result := TRegistroF(inherited Last);
end;

function TRegistroFList.New: TRegistroF;
begin
  Result := TRegistroF.Create;
  Add(Result);
end;

procedure TRegistroFList.SetItem(Index: Integer; Value: TRegistroF);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroHList }

function TRegistroHList.First: TRegistroH;
begin
  Result := TRegistroH(inherited First);
end;

function TRegistroHList.GetItem(Index: Integer): TRegistroH;
begin
  Result := TRegistroH(inherited GetItem(Index));
end;

function TRegistroHList.Last: TRegistroH;
begin
  Result := TRegistroH(inherited Last);
end;

function TRegistroHList.New: TRegistroH;
begin
  Result := TRegistroH.Create;
  Add(Result);
end;

procedure TRegistroHList.SetItem(Index: Integer; Value: TRegistroH);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroIList }

function TRegistroIList.First: TRegistroI;
begin
  Result := TRegistroI(inherited First);
end;

function TRegistroIList.GetItem(Index: Integer): TRegistroI;
begin
  Result := TRegistroI(inherited GetItem(Index));
end;

function TRegistroIList.Last: TRegistroI;
begin
  Result := TRegistroI(inherited Last);
end;

function TRegistroIList.New: TRegistroI;
begin
  Result := TRegistroI.Create;
  Add(Result);
end;

procedure TRegistroIList.SetItem(Index: Integer; Value: TRegistroI);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroJList }

function TRegistroJList.First: TRegistroJ;
begin
  Result := TRegistroJ(inherited First);
end;

function TRegistroJList.GetItem(Index: Integer): TRegistroJ;
begin
  Result := TRegistroJ(inherited GetItem(Index));
end;

function TRegistroJList.Last: TRegistroJ;
begin
  Result := TRegistroJ(inherited Last);
end;

function TRegistroJList.New: TRegistroJ;
begin
  Result := TRegistroJ.Create;
  Add(Result);
end;

procedure TRegistroJList.SetItem(Index: Integer; Value: TRegistroJ);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroKList }

function TRegistroKList.First: TRegistroK;
begin
  Result := TRegistroK(inherited First);
end;

function TRegistroKList.GetItem(Index: Integer): TRegistroK;
begin
  Result := TRegistroK(inherited GetItem(Index));
end;

function TRegistroKList.Last: TRegistroK;
begin
  Result := TRegistroK(inherited Last);
end;

function TRegistroKList.New: TRegistroK;
begin
  Result := TRegistroK.Create;
  Add(Result);
end;

procedure TRegistroKList.SetItem(Index: Integer; Value: TRegistroK);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroLList }

function TRegistroLList.First: TRegistroL;
begin
  Result := TRegistroL(inherited First);
end;

function TRegistroLList.GetItem(Index: Integer): TRegistroL;
begin
  Result := TRegistroL(inherited GetItem(Index));
end;

function TRegistroLList.Last: TRegistroL;
begin
  Result := TRegistroL(inherited Last);
end;

function TRegistroLList.New: TRegistroL;
begin
  Result := TRegistroL.Create;
  Add(Result);
end;

procedure TRegistroLList.SetItem(Index: Integer; Value: TRegistroL);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroTList }

function TRegistroTList.First: TRegistroT;
begin
  Result := TRegistroT(inherited First);
end;

function TRegistroTList.GetItem(Index: Integer): TRegistroT;
begin
  Result := TRegistroT(inherited GetItem(Index));
end;

function TRegistroTList.Last: TRegistroT;
begin
  Result := TRegistroT(inherited Last);
end;

function TRegistroTList.New: TRegistroT;
begin
  Result := TRegistroT.Create;
  Add(Result);
end;

procedure TRegistroTList.SetItem(Index: Integer; Value: TRegistroT);
begin
  inherited SetItem(Index, Value);
end;

{ TRegistroXList }

function TRegistroXList.First: TRegistroX;
begin
  Result := TRegistroX(inherited First);
end;

function TRegistroXList.GetItem(Index: Integer): TRegistroX;
begin
  Result := TRegistroX(inherited GetItem(Index));
end;

function TRegistroXList.Last: TRegistroX;
begin
  Result := TRegistroX(inherited Last);
end;

function TRegistroXList.New: TRegistroX;
begin
  Result := TRegistroX.Create;
  Add(Result);
end;

procedure TRegistroXList.SetItem(Index: Integer; Value: TRegistroX);
begin
  inherited SetItem(Index, Value);
end;

{ TDebitoAutomatico }

constructor TDebitoAutomatico.Create;
begin
  inherited Create;

  FGeral := TGeral.Create;
  FRegistroA := TRegistroA.Create;
  FRegistroB := TRegistroBList.Create;
  FRegistroC := TRegistroCList.Create;
  FRegistroD := TRegistroDList.Create;
  FRegistroE := TRegistroEList.Create;
  FRegistroF := TRegistroFList.Create;
  FRegistroH := TRegistroHList.Create;
  FRegistroI := TRegistroIList.Create;
  FRegistroJ := TRegistroJList.Create;
  FRegistroK := TRegistroKList.Create;
  FRegistroL := TRegistroLList.Create;
  FRegistroT := TRegistroTList.Create;
  FRegistroX := TRegistroXList.Create;
  FRegistroZ := TRegistroZ.Create;
end;

destructor TDebitoAutomatico.Destroy;
begin
  FGeral.Free;
  FRegistroA.Free;
  FRegistroB.Free;
  FRegistroC.Free;
  FRegistroD.Free;
  FRegistroE.Free;
  FRegistroF.Free;
  FRegistroH.Free;
  FRegistroI.Free;
  FRegistroJ.Free;
  FRegistroK.Free;
  FRegistroL.Free;
  FRegistroT.Free;
  FRegistroX.Free;
  FRegistroZ.Free;

  inherited Destroy;
end;

procedure TDebitoAutomatico.SetRegistroB(const Value: TRegistroBList);
begin
  FRegistroB := Value;
end;

procedure TDebitoAutomatico.SetRegistroC(const Value: TRegistroCList);
begin
  FRegistroC := Value;
end;

procedure TDebitoAutomatico.SetRegistroD(const Value: TRegistroDList);
begin
  FRegistroD := Value;
end;

procedure TDebitoAutomatico.SetRegistroE(const Value: TRegistroEList);
begin
  FRegistroE := Value;
end;

procedure TDebitoAutomatico.SetRegistroF(const Value: TRegistroFList);
begin
  FRegistroF := Value;
end;

procedure TDebitoAutomatico.SetRegistroH(const Value: TRegistroHList);
begin
  FRegistroH := Value;
end;

procedure TDebitoAutomatico.SetRegistroI(const Value: TRegistroIList);
begin
  FRegistroI := Value;
end;

procedure TDebitoAutomatico.SetRegistroJ(const Value: TRegistroJList);
begin
  FRegistroJ := Value;
end;

procedure TDebitoAutomatico.SetRegistroK(const Value: TRegistroKList);
begin
  FRegistroK := Value;
end;

procedure TDebitoAutomatico.SetRegistroL(const Value: TRegistroLList);
begin
  FRegistroL := Value;
end;

procedure TDebitoAutomatico.SetRegistroT(const Value: TRegistroTList);
begin
  FRegistroT := Value;
end;

procedure TDebitoAutomatico.SetRegistroX(const Value: TRegistroXList);
begin
  FRegistroX := Value;
end;

end.
