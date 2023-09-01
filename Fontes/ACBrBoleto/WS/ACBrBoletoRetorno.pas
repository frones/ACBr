{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  José M S Junior                                }
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

unit ACBrBoletoRetorno;

interface

uses
  Classes, SysUtils, contnrs, ACBrBoletoConversao;

type

  { TACBrBoletoHeader }

  TACBrBoletoHeader = class
  private
    FVersao: String;
    FAutenticacao: String;
    FUsuario_Servico: String;
    FUsuario: String;
    FOperacao: TOperacao;
    FIndice: Integer;
    FSistema_Origem: String;
    FAgencia: Integer;
    FId_Origem: String;
    FData_Hora: TDateTime;
    FId_Processo: String;
    FCNPJCPF_Beneficiario: String;
  public
    procedure Assign(DeACBrBoletoHeader: TACBrBoletoHeader); reintroduce; virtual;

    property Versao: String               read FVersao               write FVersao;
    property Autenticacao: String         read FAutenticacao         write FAutenticacao;
    property Usuario_Servico: String      read FUsuario_Servico      write FUsuario_Servico;
    property Usuario: String              read FUsuario              write FUsuario;
    property Operacao: TOperacao          read FOperacao             write FOperacao;
    property Indice: Integer              read FIndice               write FIndice;
    property Sistema_Origem: String       read FSistema_Origem       write FSistema_Origem;
    property Agencia: Integer             read FAgencia              write FAgencia;
    property Id_Origem: String            read FId_Origem            write FId_Origem;
    property Data_Hora: TDateTime         read FData_Hora            write FData_Hora;
    property Id_Processo: String          read FId_Processo          write FId_Processo;
    property CNPJCPF_Beneficiario: String read FCNPJCPF_Beneficiario write FCNPJCPF_Beneficiario;
  end;

  { TACBrBoletoControleNegocial }

  TACBrBoletoControleNegocial = class
  private
    FOriRetorno: String;
    FCodRetorno: String;
    FNSU: String;
    FRetorno: String;
  public
    procedure Assign(DeACBrBoletoControleNegocial: TACBrBoletoControleNegocial); reintroduce; virtual;

    property OriRetorno: String  read FOriRetorno write FOriRetorno;
    property CodRetorno: String  read FCodRetorno write FCodRetorno;
    property NSU: String         read FNSU        write FNSU;
    property Retorno: String     read FRetorno    write FRetorno;
  end;

  { TACBrBoletoComprovante }

  TACBrBoletoComprovante = class
  private
    FHora: String;
    FData: TDateTime;
  public
    procedure Assign(DeACBrBoletoComprovante: TACBrBoletoComprovante); reintroduce; virtual;

    property Data: TDateTime read FData write FData;
    property Hora: String    read FHora write FHora;
  end;

  { TACBrBoletoIDBoleto }

  TACBrBoletoIDBoleto = class
  private
    FCodBarras: String;
    FLinhaDig: String;
    FNossoNum: String;
    FURL: String;
    FIDBoleto: string;
    FURLPDF: String;
  public
    procedure Assign(DeACBrBoletoIDBoleto: TACBrBoletoIDBoleto); reintroduce; virtual;
    property IDBoleto: string  read FIDBoleto write FIDBoleto;
    property CodBarras: String read FCodBarras write FCodBarras;
    property LinhaDig: String  read FLinhaDig  write FLinhaDig;
    property NossoNum: String  read FNossoNum  write FNossoNum;
    property URL: String       read FURL       write FURL;
    property URLPDF: String    read FURLPDF    write FURLPDF;

  end;

  { TACBrBoletoSacadoAvalistaRet}

  TACBrBoletoSacadoAvalistaRet = class
  private
    FTipoPessoa: TACBrPessoa;
    FNomeAvalista: String;
    FCNPJCPF: String;

  public
    procedure Assign(DeACBrBoletoSacadoAvalistaRet: TACBrBoletoSacadoAvalistaRet); reintroduce; virtual;

    property Pessoa: TACBrPessoa  read FTipoPessoa   write FTipoPessoa;
    property NomeAvalista: String read FNomeAvalista write FNomeAvalista;
    property CNPJCPF: String      read FCNPJCPF      write FCNPJCPF;

  end;


  { TACBrBoletoSacadoRet }

  TACBrBoletoSacadoRet = class
  private
    FTipoPessoa: TACBrPessoa;
    FNomeSacado: String;
    FCNPJCPF: String;
    FLogradouro: String;
    FNumero: String;
    FComplemento: String;
    FBairro: String;
    FCidade: String;
    FUF: String;
    FCEP: String;
    FEmail: String;
    FFone: String;
  public
    procedure Assign(DeACBrBoletoSacadoRet: TACBrBoletoSacadoRet); reintroduce; virtual;

    property Pessoa: TACBrPessoa  read FTipoPessoa   write FTipoPessoa;
    property NomeSacado: String   read FNomeSacado   write FNomeSacado;
    property CNPJCPF: String      read FCNPJCPF      write FCNPJCPF;
    property Logradouro: String   read FLogradouro   write FLogradouro;
    property Numero: String       read FNumero       write FNumero;
    property Complemento: String  read FComplemento  write FComplemento;
    property Bairro: String       read FBairro       write FBairro;
    property Cidade: String       read FCidade       write FCidade;
    property UF: String           read FUF           write FUF;
    property CEP: String          read FCEP          write FCEP;
    property Email: String        read FEmail        write FEmail;
    property Fone: String         read FFone         write FFone;
  end;

   { TACBrBoletoTituloRet }

   TACBrBoletoTituloRet = class
  private
    FCodBarras: String;
    FLinhaDig: String;
    FURL: String;
    FInstrucao1: String;
    FInstrucao2: String;
    FInstrucao3: String;
    FParcela: Integer;
    FPercentualMulta: Double;
    FMultaValorFixo: Boolean;
    FSeuNumero: String;
    FTipoDiasProtesto: TACBrTipoDiasIntrucao;
    FVencimento: TDateTime;
    FDataDocumento: TDateTime;
    FNumeroDocumento: String;
    FEspecieDoc: String;
    FAceite: TACBrAceiteTitulo;
    FDataProcessamento: TDateTime;
    FNossoNumero: String;
    FUsoBanco: String;
    FCarteira: String;
    FEspecieMod: String;
    FValorDocumento: Currency;
    FMensagem: TStrings;
    FInformativo: TStrings;
    FInstrucoes: TStrings;
    FSacado: TACBrBoletoSacadoRet;
    FSacadoAvalista: TACBrBoletoSacadoAvalistaRet;
    FDataCredito: TDateTime;
    FDataAbatimento: TDateTime;
    FDataDesconto: TDateTime;
    FDataDesconto2: TDateTime;
    FDataDesconto3: TDateTime;
    FDataMoraJuros: TDateTime;
    FDataMulta: TDateTime;
    FDataProtesto: TDateTime;
    FDiasDeProtesto: Integer;
    FDataBaixa: TDateTime;
    FHoraBaixa: String;
    FDataLimitePagto: TDateTime;
    FValorDespesaCobranca: Currency;
    FValorAbatimento: Currency;
    FValorDesconto: Currency;
    FValorDesconto2: Currency;
    FValorDesconto3: Currency;
    FValorMoraJuros: Currency;
    FValorMulta: Currency;
    FValorIOF: Currency;
    FValorOutrasDespesas: Currency;
    FValorOutrosCreditos: Currency;
    FValorRecebido: Currency;
    FCodigoMora: String;
    FCarteiraEnvio: TACBrCarteiraEnvio;
    FCodigoNegativacao: TACBrCodigoNegativacao;
    FCodigoDesconto: TACBrCodigoDesconto;
    FCodigoMoraJuros: TACBrCodigoJuros;
    FCodigoMulta: TACBrCodigoMulta;
    FValorPago: Currency;
    FCaracTitulo: TACBrCaracTitulo;
    FTipoPagamento: TTipo_Pagamento;
    FQtdePagamentoParcial: Integer;
    FQtdeParcelas: Integer;
    FValorMinPagamento: Currency;
    FValorMaxPagamento: Currency;
    FPercentualMinPagamento: Currency;
    FPercentualMaxPagamento: Currency;
    FModalidade : Integer;
    FCodigoCliente : Extended;
    FDataRegistro: TDateTime;
    FValorAtual: Extended;
    FContrato: String;
    FCodigoEstadoTituloCobranca: String;
    FEstadoTituloCobranca: String;
    FDataMovimento: TDateTime;
    Femv: String;
    FurlPix: String;
    FtxId: String;
    FCodigoOcorrenciaCartorio: String;
    FCodigoCanalTituloCobranca: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(DeACBrBoletoTituloRet: TACBrBoletoTituloRet); reintroduce; virtual;

    property CodBarras: String read FCodBarras write FCodBarras;
    property LinhaDig: String read FLinhaDig write FLinhaDig;
    property URL: String read FURL write FURL;
    property Instrucao1: String read FInstrucao1 write FInstrucao1;
    property Instrucao2: String read FInstrucao2 write FInstrucao2 ;
    property Instrucao3: String read FInstrucao3 write FInstrucao3 ;
    property Parcela: Integer read FParcela write FParcela ;
    property PercentualMulta: Double read FPercentualMulta write FPercentualMulta ;
    property MultaValorFixo: Boolean read FMultaValorFixo write FMultaValorFixo ;
    property SeuNumero: String read FSeuNumero write FSeuNumero ;
    property TipoDiasProtesto: TACBrTipoDiasIntrucao read FTipoDiasProtesto write FTipoDiasProtesto ;
    property Vencimento: TDateTime read FVencimento write FVencimento ;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento ;
    property NumeroDocumento: String read FNumeroDocumento write FNumeroDocumento ;
    property EspecieDoc: String read FEspecieDoc write FEspecieDoc ;
    property Aceite: TACBrAceiteTitulo read FAceite write FAceite ;
    property DataProcessamento: TDateTime read FDataProcessamento write FDataProcessamento ;
    property NossoNumero: String read FNossoNumero write FNossoNumero ;
    property UsoBanco: String read FUsoBanco write FUsoBanco ;
    property Carteira: String read FCarteira write FCarteira ;
    property EspecieMod: String read FEspecieMod write FEspecieMod ;
    property ValorDocumento: Currency read FValorDocumento write FValorDocumento ;
    property Mensagem: TStrings read FMensagem write FMensagem ;
    property Informativo: TStrings read FInformativo write FInformativo ;
    property Instrucoes: TStrings read FInstrucoes write FInstrucoes ;
    property Sacado: TACBrBoletoSacadoRet read FSacado write FSacado ;
    property SacadoAvalista: TACBrBoletoSacadoAvalistaRet read FSacadoAvalista write FSacadoAvalista ;
    property DataCredito: TDateTime read FDataCredito write FDataCredito ;
    property DataAbatimento: TDateTime read FDataAbatimento write FDataAbatimento ;
    property DataDesconto: TDateTime read FDataDesconto write FDataDesconto ;
    property DataDesconto2: TDateTime read FDataDesconto2 write FDataDesconto2 ;
    property DataDesconto3: TDateTime read FDataDesconto3 write FDataDesconto3 ;
    property DataMoraJuros: TDateTime read FDataMoraJuros write FDataMoraJuros ;
    property DataMulta: TDateTime read FDataMulta write FDataMulta ;
    property DataProtesto: TDateTime read FDataProtesto write FDataProtesto ;
    property DiasDeProtesto: Integer read FDiasDeProtesto write FDiasDeProtesto ;
    property DataBaixa: TDateTime read FDataBaixa write FDataBaixa ;
    property HoraBaixa: String read FHoraBaixa write FHoraBaixa;
    property DataLimitePagto: TDateTime read FDataLimitePagto write FDataLimitePagto ;
    property ValorDespesaCobranca: Currency read FValorDespesaCobranca write FValorDespesaCobranca ;
    property ValorAbatimento: Currency read FValorAbatimento write FValorAbatimento ;
    property ValorDesconto: Currency read FValorDesconto write FValorDesconto ;
    property ValorDesconto2: Currency read FValorDesconto2 write FValorDesconto2 ;
    property ValorDesconto3: Currency read FValorDesconto3 write FValorDesconto3 ;
    property ValorMoraJuros: Currency read FValorMoraJuros write FValorMoraJuros ;
    property ValorMulta: Currency read FValorMulta write FValorMulta ;
    property ValorIOF: Currency read FValorIOF write FValorIOF ;
    property ValorOutrasDespesas: Currency read FValorOutrasDespesas write FValorOutrasDespesas ;
    property ValorOutrosCreditos: Currency read FValorOutrosCreditos write FValorOutrosCreditos ;
    property ValorRecebido: Currency read FValorRecebido write FValorRecebido ;
    property CodigoMora: String read FCodigoMora write FCodigoMora ;
    property CarteiraEnvio: TACBrCarteiraEnvio read FCarteiraEnvio write FCarteiraEnvio ;
    property CodigoNegativacao: TACBrCodigoNegativacao read FCodigoNegativacao write FCodigoNegativacao ;
    property CodigoDesconto: TACBrCodigoDesconto read FCodigoDesconto write FCodigoDesconto ;
    property CodigoMoraJuros: TACBrCodigoJuros read FCodigoMoraJuros write FCodigoMoraJuros ;
    property CodigoMulta: TACBrCodigoMulta read FCodigoMulta write FCodigoMulta ;
    property ValorPago: Currency read FValorPago write FValorPago ;
    property CaracTitulo: TACBrCaracTitulo read FCaracTitulo write FCaracTitulo ;
    property TipoPagamento: TTipo_Pagamento read FTipoPagamento write FTipoPagamento ;
    property QtdePagamentoParcial: integer read FQtdePagamentoParcial write FQtdePagamentoParcial ;
    property QtdeParcelas: integer read FQtdeParcelas write FQtdeParcelas ;
    property ValorMinPagamento: currency read FValorMinPagamento write FValorMinPagamento ;
    property ValorMaxPagamento: currency read FValorMaxPagamento write FValorMaxPagamento ;
    property PercentualMinPagamento: currency read FPercentualMinPagamento write FPercentualMinPagamento ;
    property PercentualMaxPagamento: currency read FPercentualMaxPagamento write FPercentualMaxPagamento ;
    property Modalidade : Integer read FModalidade write FModalidade;
    property CodigoCliente : Extended read FCodigoCliente write FCodigoCliente;
    property DataRegistro: TDateTime read FDataRegistro write FDataRegistro;
    property ValorAtual: Extended read FValorAtual write FValorAtual;
    property Contrato: String read FContrato write FContrato;
    property CodigoEstadoTituloCobranca: String read FCodigoEstadoTituloCobranca write FCodigoEstadoTituloCobranca;
    property CodigoCanalTituloCobranca: String read FCodigoCanalTituloCobranca write FCodigoCanalTituloCobranca;
    property EstadoTituloCobranca: String read FEstadoTituloCobranca write FEstadoTituloCobranca;
    property CodigoOcorrenciaCartorio: String read FCodigoOcorrenciaCartorio write FCodigoOcorrenciaCartorio;
    property DataMovimento: TDateTime read FDataMovimento write FDataMovimento;
    property EMV: String read Femv write Femv;
    property UrlPix: String read FurlPix write FurlPix;
    property TxId: String read FtxId write FtxId;
  end;

  { TACBrBoletoDadosRet }
  TACBrBoletoDadosRet = class
  private
    FControleNegocial: TACBrBoletoControleNegocial;
    FComprovante: TACBrBoletoComprovante;
    FExcecao: String;
    FIDBoleto: TACBrBoletoIDBoleto;
    FTituloRet: TACBrBoletoTituloRet;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(DeACBrBoletoDadosRet: TACBrBoletoDadosRet); reintroduce; virtual;

    property ControleNegocial: TACBrBoletoControleNegocial read FControleNegocial write FControleNegocial;
    property Comprovante: TACBrBoletoComprovante           read FComprovante      write FComprovante;
    property Excecao: String                               read FExcecao          write FExcecao;
    property IDBoleto: TACBrBoletoIDBoleto                 read FIDBoleto         write FIDBoleto;
    property TituloRet: TACBrBoletoTituloRet               read FTituloRet        write FTituloRet;

  end;

  { TACBrBoletoRejeicao }

  TACBrBoletoRejeicao = class
  private
    FCampo      : String;
    FMensagem   : String;
    FValor      : String;
    FCodigo     : String;
    FVersao     : String;
    FOcorrencia : String;
  public
    procedure Assign(DeACBrBoletoRejeicao: TACBrBoletoRejeicao); reintroduce; virtual;

    property Campo      : String  read FCampo      write FCampo;
    property Mensagem   : String  read FMensagem   write FMensagem;
    property Valor      : String  read FValor      write FValor;
    property Codigo     : String  read FCodigo     write FCodigo;
    property Versao     : String  read FVersao     write FVersao;
    property Ocorrencia : String  read FOcorrencia write FOcorrencia;
  end;

  { TACBrBoletoListaRejeicao }
  TACBrBoletoListaRejeicao = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TACBrBoletoRejeicao);
    function  GetObject (Index: Integer): TACBrBoletoRejeicao;
    procedure Insert (Index: Integer; Obj: TACBrBoletoRejeicao);
  public
    function Add (Obj: TACBrBoletoRejeicao): Integer;
    property Objects [Index: Integer]: TACBrBoletoRejeicao read GetObject write SetObject; default;
  end;

  { TACBrBoletoRetornoWS }
  TACBrBoletoRetornoWS = class
  private
    FHeader: TACBrBoletoHeader;
    FCodRetorno     : String;
    FOriRetorno     : String;
    FMsgRetorno     : String;
    FDadosRet       : TACBrBoletoDadosRet;
    FListaRejeicao  : TACBrBoletoListaRejeicao;
    FHTTPResultCode : Integer;
    FJSON           : String;
	  FJSONEnvio      : String;
    FIndicadorContinuidade: boolean;
    FProximoIndice: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(DeACBrBoletoRetEnvio: TACBrBoletoRetornoWS); reintroduce; virtual;

    function CriarRejeicaoLista : TACBrBoletoRejeicao;
    property Header                : TACBrBoletoHeader        read FHeader                write FHeader;
    property CodRetorno            : String                   read FCodRetorno            write FCodRetorno;
    property OriRetorno            : String                   read FOriRetorno            write FOriRetorno;
    property MsgRetorno            : String                   read FMsgRetorno            write FMsgRetorno;
    property HTTPResultCode        : Integer                  read FHTTPResultCode        write FHTTPResultCode;
    property JSON                  : String                   read FJSON                  write FJSON;
	  property JSONEnvio             : String                   read FJSONEnvio             write FJSONEnvio;
    property DadosRet              : TACBrBoletoDadosRet      read FDadosRet              write FDadosRet;
    property ListaRejeicao         : TACBrBoletoListaRejeicao read FListaRejeicao;
    property indicadorContinuidade : boolean                  read FIndicadorContinuidade write FIndicadorContinuidade;
    property proximoIndice         : integer                  read FProximoIndice         write FProximoIndice;
  end;

  { TListaACBrBoletoRetornoWS }
  TListaACBrBoletoRetornoWS = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TACBrBoletoRetornoWS);
    function  GetObject (Index: Integer): TACBrBoletoRetornoWS;
    procedure Insert (Index: Integer; Obj: TACBrBoletoRetornoWS);
  public
    function Add (Obj: TACBrBoletoRetornoWS): Integer;
    property Objects [Index: Integer]: TACBrBoletoRetornoWS read GetObject write SetObject; default;
  end;

implementation

{ TACBrBoletoRejeicao }

procedure TACBrBoletoRejeicao.Assign(DeACBrBoletoRejeicao: TACBrBoletoRejeicao);
begin
  Campo      := DeACBrBoletoRejeicao.Campo;
  Mensagem   := DeACBrBoletoRejeicao.Mensagem;
  Valor      := DeACBrBoletoRejeicao.Valor;
  Codigo     := DeACBrBoletoRejeicao.Codigo;
  Versao     := DeACBrBoletoRejeicao.Versao;
  Ocorrencia := DeACBrBoletoRejeicao.Ocorrencia;

end;

{ TACBrBoletoComprovante }

procedure TACBrBoletoComprovante.Assign(DeACBrBoletoComprovante: TACBrBoletoComprovante);
begin
  Hora:= DeACBrBoletoComprovante.Hora;
  Data:= DeACBrBoletoComprovante.Data;
end;

{ TACBrBoletoSacadoRet }

procedure TACBrBoletoSacadoRet.Assign(DeACBrBoletoSacadoRet: TACBrBoletoSacadoRet);
begin
  Pessoa:= DeACBrBoletoSacadoRet.Pessoa;
  NomeSacado:= DeACBrBoletoSacadoRet.NomeSacado;
  CNPJCPF:= DeACBrBoletoSacadoRet.CNPJCPF;
  Logradouro:= DeACBrBoletoSacadoRet.Logradouro;
  Numero:= DeACBrBoletoSacadoRet.Numero;
  Complemento:= DeACBrBoletoSacadoRet.Complemento;
  Bairro:= DeACBrBoletoSacadoRet.Bairro;
  Cidade:= DeACBrBoletoSacadoRet.Cidade;
  UF:= DeACBrBoletoSacadoRet.UF;
  CEP:= DeACBrBoletoSacadoRet.CEP;
  Email:= DeACBrBoletoSacadoRet.Email;
  Fone:= DeACBrBoletoSacadoRet.Fone;

end;

{ TSacadoAvalistaRet }

procedure TACBrBoletoSacadoAvalistaRet.Assign( DeACBrBoletoSacadoAvalistaRet: TACBrBoletoSacadoAvalistaRet);
begin
  Pessoa:= DeACBrBoletoSacadoAvalistaRet.Pessoa;
  NomeAvalista:= DeACBrBoletoSacadoAvalistaRet.NomeAvalista;
  CNPJCPF:= DeACBrBoletoSacadoAvalistaRet.CNPJCPF;

end;

{ TACBrBoletoIDBoleto }

procedure TACBrBoletoIDBoleto.Assign(DeACBrBoletoIDBoleto: TACBrBoletoIDBoleto);
begin
  CodBarras:= DeACBrBoletoIDBoleto.CodBarras;
  LinhaDig:= DeACBrBoletoIDBoleto.LinhaDig;
  NossoNum:= DeACBrBoletoIDBoleto.NossoNum;
  URL:= DeACBrBoletoIDBoleto.URL;
  IDBoleto:= DeACBrBoletoIDBoleto.IDBoleto;
  URLPDF:= DeACBrBoletoIDBoleto.URLPDF;

end;

{ TControleNegocial }

procedure TACBrBoletoControleNegocial.Assign( DeACBrBoletoControleNegocial: TACBrBoletoControleNegocial);
begin
  OriRetorno:= DeACBrBoletoControleNegocial.OriRetorno;
  CodRetorno:= DeACBrBoletoControleNegocial.CodRetorno;
  NSU:= DeACBrBoletoControleNegocial.NSU;
  Retorno:= DeACBrBoletoControleNegocial.Retorno;

end;

{ TACBrBoletoHeader }

procedure TACBrBoletoHeader.Assign(DeACBrBoletoHeader: TACBrBoletoHeader);
begin
  Versao:= DeACBrBoletoHeader.Versao;
  Autenticacao:= DeACBrBoletoHeader.Autenticacao;
  Usuario_Servico:= DeACBrBoletoHeader.Usuario_Servico;
  Usuario:= DeACBrBoletoHeader.Usuario;
  Operacao:= DeACBrBoletoHeader.Operacao;
  Indice:= DeACBrBoletoHeader.Indice;
  Sistema_Origem:= DeACBrBoletoHeader.Sistema_Origem;
  Agencia:= DeACBrBoletoHeader.Agencia;
  Id_Origem:= DeACBrBoletoHeader.Id_Origem;
  Data_Hora:= DeACBrBoletoHeader.Data_Hora;
  Id_Processo:= DeACBrBoletoHeader.Id_Processo;
  CNPJCPF_Beneficiario:= DeACBrBoletoHeader.CNPJCPF_Beneficiario;

end;

{ TACBrBoletoListaRejeicao }

procedure TACBrBoletoListaRejeicao.SetObject(Index: Integer; Item: TACBrBoletoRejeicao);
begin
  inherited SetItem (Index, Item) ;
end;

function TACBrBoletoListaRejeicao.GetObject(Index: Integer): TACBrBoletoRejeicao;
begin
  Result := inherited GetItem(Index) as TACBrBoletoRejeicao ;
end;

procedure TACBrBoletoListaRejeicao.Insert(Index: Integer; Obj: TACBrBoletoRejeicao);
begin
  inherited Insert(Index, Obj);
end;

function TACBrBoletoListaRejeicao.Add(Obj: TACBrBoletoRejeicao): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TTituloRet }

constructor TACBrBoletoTituloRet.Create;
begin
  FSacadoAvalista := TACBrBoletoSacadoAvalistaRet.Create;
  FSacado := TACBrBoletoSacadoRet.Create;
  FMensagem       := TStringList.Create;
  FInformativo    := TStringList.Create;
  FInstrucoes     := TStringList.Create;
end;

destructor TACBrBoletoTituloRet.Destroy;
begin
  FMensagem.Free;
  FInformativo.Free;
  FInstrucoes.Free;
  FSacado.Free;
  FSacadoAvalista.Free;

  inherited Destroy;
end;

procedure TACBrBoletoTituloRet.Assign(DeACBrBoletoTituloRet: TACBrBoletoTituloRet);
begin
  CodBarras:= DeACBrBoletoTituloRet.CodBarras;
  LinhaDig:= DeACBrBoletoTituloRet.LinhaDig;
  URL:= DeACBrBoletoTituloRet.URL;
  Instrucao1:= DeACBrBoletoTituloRet.Instrucao1;
  Instrucao2:= DeACBrBoletoTituloRet.Instrucao2;
  Instrucao3:= DeACBrBoletoTituloRet.Instrucao3;
  Parcela:= DeACBrBoletoTituloRet.Parcela;
  PercentualMulta:= DeACBrBoletoTituloRet.PercentualMulta;
  MultaValorFixo:= DeACBrBoletoTituloRet.MultaValorFixo;
  SeuNumero:= DeACBrBoletoTituloRet.SeuNumero;
  TipoDiasProtesto:= DeACBrBoletoTituloRet.TipoDiasProtesto;
  Vencimento:= DeACBrBoletoTituloRet.Vencimento;
  DataDocumento:= DeACBrBoletoTituloRet.DataDocumento;
  NumeroDocumento:= DeACBrBoletoTituloRet.NumeroDocumento;
  EspecieDoc:= DeACBrBoletoTituloRet.EspecieDoc;
  Aceite:= DeACBrBoletoTituloRet.Aceite;
  DataProcessamento:= DeACBrBoletoTituloRet.DataProcessamento;
  NossoNumero:= DeACBrBoletoTituloRet.NossoNumero;
  UsoBanco:= DeACBrBoletoTituloRet.UsoBanco;
  Carteira:= DeACBrBoletoTituloRet.Carteira;
  EspecieMod:= DeACBrBoletoTituloRet.EspecieMod;
  ValorDocumento:= DeACBrBoletoTituloRet.ValorDocumento;
  Mensagem.Text:= DeACBrBoletoTituloRet.Mensagem.Text;
  Informativo.text:= DeACBrBoletoTituloRet.Informativo.Text;
  Instrucoes.Text:= DeACBrBoletoTituloRet.Instrucoes.Text;
  DataCredito:= DeACBrBoletoTituloRet.DataCredito;
  DataAbatimento:= DeACBrBoletoTituloRet.DataAbatimento;
  DataDesconto:= DeACBrBoletoTituloRet.DataDesconto;
  DataDesconto2:= DeACBrBoletoTituloRet.DataDesconto2;
  DataDesconto3:= DeACBrBoletoTituloRet.DataDesconto3;
  DataMoraJuros:= DeACBrBoletoTituloRet.DataMoraJuros;
  DataMulta:= DeACBrBoletoTituloRet.DataMulta;
  DataProtesto:= DeACBrBoletoTituloRet.DataProtesto;
  DiasDeProtesto:= DeACBrBoletoTituloRet.DiasDeProtesto;
  DataBaixa:= DeACBrBoletoTituloRet.DataBaixa;
  HoraBaixa:= DeACBrBoletoTituloRet.HoraBaixa;
  DataLimitePagto:= DeACBrBoletoTituloRet.DataLimitePagto;
  ValorDespesaCobranca:= DeACBrBoletoTituloRet.ValorDespesaCobranca;
  ValorAbatimento:= DeACBrBoletoTituloRet.ValorAbatimento;
  ValorDesconto:= DeACBrBoletoTituloRet.ValorDesconto;
  ValorDesconto2:= DeACBrBoletoTituloRet.ValorDesconto2;
  ValorDesconto3:= DeACBrBoletoTituloRet.ValorDesconto3;
  ValorMoraJuros:= DeACBrBoletoTituloRet.ValorMoraJuros;
  ValorMulta := DeACBrBoletoTituloRet.ValorMulta;
  ValorIOF:= DeACBrBoletoTituloRet.ValorIOF;
  ValorOutrasDespesas:= DeACBrBoletoTituloRet.ValorOutrasDespesas;
  ValorOutrosCreditos:= DeACBrBoletoTituloRet.ValorOutrosCreditos;
  ValorRecebido:= DeACBrBoletoTituloRet.ValorRecebido;
  CodigoMora:= DeACBrBoletoTituloRet.CodigoMora;
  CarteiraEnvio:= DeACBrBoletoTituloRet.CarteiraEnvio;
  CodigoNegativacao:= DeACBrBoletoTituloRet.CodigoNegativacao;
  CodigoDesconto:= DeACBrBoletoTituloRet.CodigoDesconto;
  CodigoMoraJuros:= DeACBrBoletoTituloRet.CodigoMoraJuros;
  CodigoMulta:= DeACBrBoletoTituloRet.CodigoMulta;
  ValorPago:= DeACBrBoletoTituloRet.ValorPago;
  CaracTitulo:= DeACBrBoletoTituloRet.CaracTitulo;
  TipoPagamento:= DeACBrBoletoTituloRet.TipoPagamento;
  QtdePagamentoParcial:= DeACBrBoletoTituloRet.QtdePagamentoParcial;
  QtdeParcelas:= DeACBrBoletoTituloRet.QtdeParcelas;
  ValorMinPagamento:= DeACBrBoletoTituloRet.ValorMinPagamento;
  ValorMaxPagamento:= DeACBrBoletoTituloRet.ValorMaxPagamento;
  PercentualMinPagamento:= DeACBrBoletoTituloRet.PercentualMinPagamento;
  PercentualMaxPagamento:= DeACBrBoletoTituloRet.PercentualMaxPagamento;
  Modalidade := DeACBrBoletoTituloRet.Modalidade;
  CodigoCliente := DeACBrBoletoTituloRet.CodigoCliente;
  DataRegistro:= DeACBrBoletoTituloRet.DataRegistro;
  ValorAtual:= DeACBrBoletoTituloRet.ValorAtual;
  Contrato:= DeACBrBoletoTituloRet.Contrato;
  CodigoEstadoTituloCobranca:= DeACBrBoletoTituloRet.CodigoEstadoTituloCobranca;
  EstadoTituloCobranca:= DeACBrBoletoTituloRet.EstadoTituloCobranca;
  DataMovimento:= DeACBrBoletoTituloRet.DataMovimento;
  emv:= DeACBrBoletoTituloRet.EMV;
  urlPix:= DeACBrBoletoTituloRet.UrlPix;
  txId:= DeACBrBoletoTituloRet.TxId;
  CodigoCanalTituloCobranca := DeACBrBoletoTituloRet.CodigoCanalTituloCobranca;
  Sacado.Assign(DeACBrBoletoTituloRet.Sacado);
  SacadoAvalista.Assign(DeACBrBoletoTituloRet.SacadoAvalista);

end;

{ TListaACBrBoletoRetEnvio }

procedure TListaACBrBoletoRetornoWS.SetObject(Index: Integer; Item: TACBrBoletoRetornoWS);
begin
  inherited SetItem (Index, Item) ;
end;

function TListaACBrBoletoRetornoWS.GetObject(Index: Integer): TACBrBoletoRetornoWS;
begin
  Result := inherited GetItem(Index) as TACBrBoletoRetornoWS ;
end;

procedure TListaACBrBoletoRetornoWS.Insert(Index: Integer; Obj: TACBrBoletoRetornoWS);
begin
   inherited Insert(Index, Obj);
end;

function TListaACBrBoletoRetornoWS.Add(Obj: TACBrBoletoRetornoWS): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TACBrBoletoRetornoWS }

constructor TACBrBoletoRetornoWS.Create;
begin
  FHeader:= TACBrBoletoHeader.Create;
  FDadosRet:= TACBrBoletoDadosRet.Create;
  FListaRejeicao:= TACBrBoletoListaRejeicao.Create(true);
end;

destructor TACBrBoletoRetornoWS.Destroy;
begin
  FListaRejeicao.Free;
  FDadosRet.Free;
  FHeader.Free;

  inherited;
end;

procedure TACBrBoletoRetornoWS.Assign(DeACBrBoletoRetEnvio: TACBrBoletoRetornoWS);
var
  I: Integer;
  Rej: TACBrBoletoRejeicao;
begin
  CodRetorno:= DeACBrBoletoRetEnvio.CodRetorno;
  OriRetorno:= DeACBrBoletoRetEnvio.OriRetorno;
  MsgRetorno:= DeACBrBoletoRetEnvio.MsgRetorno;
  HTTPResultCode:= DeACBrBoletoRetEnvio.HTTPResultCode;
  JSON:= DeACBrBoletoRetEnvio.JSON;
  IndicadorContinuidade:= DeACBrBoletoRetEnvio.indicadorContinuidade;
  ProximoIndice:= DeACBrBoletoRetEnvio.proximoIndice;
  Header.Assign(DeACBrBoletoRetEnvio.Header);
  DadosRet.Assign(DeACBrBoletoRetEnvio.DadosRet);

  for I := 0 to DeACBrBoletoRetEnvio.ListaRejeicao.Count -1 do
  begin
    Rej := CriarRejeicaoLista;
    Rej.Campo      := DeACBrBoletoRetEnvio.ListaRejeicao[I].Ocorrencia;
    Rej.Codigo     := DeACBrBoletoRetEnvio.ListaRejeicao[I].Codigo;
    Rej.Versao     := DeACBrBoletoRetEnvio.ListaRejeicao[I].Versao;
    Rej.Mensagem   := DeACBrBoletoRetEnvio.ListaRejeicao[I].Mensagem;
    Rej.Ocorrencia := DeACBrBoletoRetEnvio.ListaRejeicao[I].Ocorrencia;

  end;

end;


function TACBrBoletoRetornoWS.CriarRejeicaoLista: TACBrBoletoRejeicao;
var
  I: Integer;
begin
   I      := FListaRejeicao.Add(TACBrBoletoRejeicao.Create);
   Result := FListaRejeicao[I];
end;

{ TDadosRet }

constructor TACBrBoletoDadosRet.Create;
begin
  FControleNegocial := TACBrBoletoControleNegocial.Create;
  FTituloRet        := TACBrBoletoTituloRet.Create;
  FComprovante      := TACBrBoletoComprovante.Create;
  FIDBoleto         := TACBrBoletoIDBoleto.Create;

end;

destructor TACBrBoletoDadosRet.Destroy;
begin
  FIDBoleto.Free;
  FComprovante.Free;
  FTituloRet.Free;
  FControleNegocial.Free;

  inherited;
end;

procedure TACBrBoletoDadosRet.Assign(DeACBrBoletoDadosRet: TACBrBoletoDadosRet);
begin
  Excecao:= DeACBrBoletoDadosRet.Excecao;
  ControleNegocial.Assign(DeACBrBoletoDadosRet.ControleNegocial);
  Comprovante.Assign(DeACBrBoletoDadosRet.Comprovante);
  IDBoleto.Assign(DeACBrBoletoDadosRet.IDBoleto);
  TituloRet.Assign(DeACBrBoletoDadosRet.TituloRet);

end;



end.

