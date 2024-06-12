{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: José M S Junior                                  }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibBoletoRespostas;

interface

uses
  SysUtils, Classes,
  ACBrLibResposta, ACBrBoleto, ACBrBoletoRetorno, ACBrBoletoConversao, contnrs;

type

  { TRetornoDadosCedente }
  TRetornoDadosCedente = class(TACBrLibRespostaBase)
  private
    FNome: String;
    FCNPJCPF: String;
    FCodigoCedente: String;
    FModalidade: String;
    FCodTransmissao: String;
    FConvenio:String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto);

  published
    property Nome : String read FNome Write FNome;
    property CNPJCPF : String read FCNPJCPF Write FCNPJCPF;
    property CodigoCedente : String read FCodigoCedente Write FCodigoCedente;
    property Modalidade : String read FModalidade Write FModalidade;
    property CodTransmissao : String read FCodTransmissao Write FCodTransmissao;
    property Convenio : String read FConvenio Write FConvenio;

  end;

  { TRetornoDadosBanco }
  TRetornoDadosBanco = class(TACBrLibRespostaBase)
  private
    FNumero : Integer;
    FIndiceACBr : Integer;
    FNumeroCorrespondente : Integer;
    FVersaoArquivo : Integer;
    FVersaoLote : Integer;
    FNumeroArquivo: Integer;
    FNomeArqRetorno: String;
    FDensidadeGravacao: String;
    FCIP: String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto);

  published
    property Numero : Integer read FNumero write FNumero;
    property IndiceACBr : Integer read FIndiceACBr write FIndiceACBr ;
    property NumeroCorrespondente : Integer read FNumeroCorrespondente write FNumeroCorrespondente;
    property VersaoArquivo : Integer read FVersaoArquivo write FVersaoArquivo;
    property VersaoLote : Integer read FVersaoLote write FVersaoLote;
    property NumeroArquivo: Integer read FNumeroArquivo write FNumeroArquivo;
    property NomeArqRetorno: String read FNomeArqRetorno write FNomeArqRetorno;
    property DensidadeGravacao: String read FDensidadeGravacao write FDensidadeGravacao;
    property CIP: String read FCIP write FCIP;

  end;

  { TRetornoDadosConta }
  TRetornoDadosConta = class(TACBrLibRespostaBase)
  private
    FConta : String;
    FDigitoConta : String;
    FAgencia : String;
    FDigitoAgencia : String;
    FDigitoVerificadorAgenciaConta : String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto);

  published
    property Conta : String read FConta write FConta;
    property DigitoConta : String read FDigitoConta write FDigitoConta;
    property Agencia : String read FAgencia write FAgencia;
    property DigitoAgencia : String read FDigitoAgencia write FDigitoAgencia;
    property DigitoVerificadorAgenciaConta : String read FDigitoVerificadorAgenciaConta write FDigitoVerificadorAgenciaConta;

  end;

   { TRetornoRejeicoesTitulo }
  TRetornoRejeicoesTitulo = class(TACBrLibRespostaBase)
  private
    FID : Integer;
    FIDRej : Integer;
    FMotivoRejeicao : String;

  public
    constructor Create( const AIDRej: Integer; const AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const ACBrBoleto: TACBrBoleto);

  published
    property MotivoRejeicao : String read FMotivoRejeicao write FMotivoRejeicao;

  end;

  { TRetornoDadosTitulo }
  TRetornoDadosTitulo = class(TACBrLibRespostaBase)
  private
    FID: Integer;
    FSacado_Nome : String;
    FSacado_CNPJCPF : String;
    FVencimento: TDateTime;
    FDataDocumento: TDateTime;
    FNumeroDocumento: String;
    FDataProcessamento: TDateTime;
    FNossoNumero: String;
    FNossoNumeroCorrespondente:String;
    FCarteira: String;
    FValorDocumento: Currency;
    FDataOcorrencia: TDateTime;
    FDataCredito: TDateTime;
    FDataBaixa: TDateTime;
    FDataMovimento: TDateTime;
    FDataMoraJuros: TDateTime;
    FValorDespesaCobranca: Currency;
    FValorAbatimento: Currency;
    FValorDesconto: Currency;
    FValorMoraJuros: Currency;
    FValorIOF: Currency;
    FValorOutrasDespesas: Currency;
    FValorOutrosCreditos: Currency;
    FValorRecebido: Currency;
    FValorPago: Currency;
    FSeuNumero: String;
    FCodTipoOcorrencia: String;
    FDescricaoTipoOcorrencia: String;
    FRejeicoes: TObjectList;
    FHoraBaixa: String;
    FEstadoTituloCobranca : String;



  public
    constructor Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar( const ACBrBoleto: TACBrBoleto);

  published
    property Sacado_Nome : String read FSacado_Nome write FSacado_Nome;
    property Sacado_CNPJCPF : String read FSacado_CNPJCPF write FSacado_CNPJCPF;
    property Vencimento: TDateTime read FVencimento write FVencimento;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property NumeroDocumento: String read FNumeroDocumento write FNumeroDocumento;
    property DataProcessamento: TDateTime read FDataProcessamento write FDataProcessamento;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property NossoNumeroCorrespondente: String read FNossoNumeroCorrespondente write FNossoNumeroCorrespondente;
    property Carteira: String read FCarteira write FCarteira;
    property DataOcorrencia: TDateTime read FDataOcorrencia write FDataOcorrencia;
    property DataCredito: TDateTime read FDataCredito write FDataCredito;
    property DataBaixa: TDateTime read FDataBaixa write FDataBaixa;
    property DataMovimento: TDateTime read FDataMovimento write FDataMovimento;
    property DataMoraJuros: TDateTime read FDataMoraJuros write FDataMoraJuros;
    property ValorDocumento: Currency read FValorDocumento write FValorDocumento;
    property ValorDespesaCobranca: Currency read FValorDespesaCobranca write FValorDespesaCobranca;
    property ValorAbatimento: Currency read FValorAbatimento write FValorAbatimento;
    property ValorDesconto: Currency read FValorDesconto write FValorDesconto;
    property ValorMoraJuros: Currency read FValorMoraJuros write FValorMoraJuros;
    property ValorIOF: Currency read FValorIOF write FValorIOF;
    property ValorOutrasDespesas: Currency read FValorOutrasDespesas write FValorOutrasDespesas;
    property ValorOutrosCreditos: Currency read FValorOutrosCreditos write FValorOutrosCreditos;
    property ValorRecebido: Currency read FValorRecebido write FValorRecebido;
    property ValorPago: Currency read FValorPago write FValorPago;
    property SeuNumero: String read FSeuNumero write FSeuNumero;
    property CodTipoOcorrencia: String read FCodTipoOcorrencia write FCodTipoOcorrencia;
    property DescricaoTipoOcorrencia: String read FDescricaoTipoOcorrencia write FDescricaoTipoOcorrencia;
    property Rejeicoes: TObjectList read FRejeicoes write FRejeicoes;
    property EstadoTituloCobranca: String read FEstadoTituloCobranca write FEstadoTituloCobranca;
    property HoraBaixa: String read FHoraBaixa write FHoraBaixa;
   



  end;

  { TRetornoBoleto }
  TRetornoBoleto = class(TACBrLibRespostaBase)
  private
    FCedente: TRetornoDadosCedente;
    FBanco: TRetornoDadosBanco;
    FConta: TRetornoDadosConta;
    FTitulo: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrBoleto: TACBrBoleto);

  published
    property Cedente: TRetornoDadosCedente read FCedente write FCedente;
    property Banco: TRetornoDadosBanco read FBanco write FBanco;
    property Conta: TRetornoDadosConta read FConta write FConta;
    property Titulo: TObjectList read FTitulo;

  end;

  { TSacadoWeb }
  TSacadoWeb = class(TACBrLibRespostaBase)
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
    constructor Create( AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const ASacado: TACBrBoletoSacadoRet);

  published
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

  { TSacadoAvalistaWeb }
  TSacadoAvalistaWeb = class(TACBrLibRespostaBase)
  private
    FTipoPessoa: TACBrPessoa;
    FNomeAvalista: String;
    FCNPJCPF: String;

  public
    constructor Create( AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const ASacadoAvalista: TACBrBoletoSacadoAvalistaRet);

  published
    property Pessoa: TACBrPessoa  read FTipoPessoa   write FTipoPessoa;
    property NomeAvalista: String read FNomeAvalista write FNomeAvalista;
    property CNPJCPF: String      read FCNPJCPF      write FCNPJCPF;

  end;

  {TRetornoTituloWeb}
  TRetornoTituloWeb =  class(TACBrLibRespostaBase)
  private
    FAID: Integer;

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
    FNossoNumeroCorrespondente: String;
    FUsoBanco: String;
    FCarteira: String;
    FEspecieMod: String;
    FValorDocumento: Currency;
    FMensagem: TStrings;
    FInformativo: TStrings;
    FInstrucoes: TStrings;
    FSacado: TSacadoWeb;
    FSacadoAvalista: TSacadoAvalistaWeb;
    FDataCredito: TDateTime;
    FDataAbatimento: TDateTime;
    FDataDesconto: TDateTime;
    FDataDesconto2: TDateTime;
    FDataMoraJuros: TDateTime;
    FDataMulta: TDateTime;
    FDataProtesto: TDateTime;
    FDiasDeProtesto: Integer;
    FDataBaixa: TDateTime;
    FHoraBaixa: String;
    FDataMovimento : TDateTime;
    FDataLimitePagto: TDateTime;
    FValorDespesaCobranca: Currency;
    FValorAbatimento: Currency;
    FValorDesconto: Currency;
    FValorDesconto2: Currency;
    FValorMoraJuros: Currency;
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
    Femv: String;
    Furl_Pix: String;
    FTx_ID: String;
    FCodigoCanalTituloCobranca: String;
    FCodigoEstadoTituloCobranca: string;
    FEstadoTituloCobranca: String;

  public
    constructor Create( AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const DadosRet: TACBrBoletoDadosRet);
    destructor Destroy; override;

  published
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
    property NossoNumeroCorrespondente: String read FNossoNumeroCorrespondente write FNossoNumeroCorrespondente;
    property UsoBanco: String read FUsoBanco write FUsoBanco ;
    property Carteira: String read FCarteira write FCarteira ;
    property EspecieMod: String read FEspecieMod write FEspecieMod ;
    property ValorDocumento: Currency read FValorDocumento write FValorDocumento ;
    property Mensagem: TStrings read FMensagem write FMensagem ;
    property Informativo: TStrings read FInformativo write FInformativo ;
    property Instrucoes: TStrings read FInstrucoes write FInstrucoes ;
    property Sacado: TSacadoWeb read FSacado;
    property SacadoAvalista: TSacadoAvalistaWeb read FSacadoAvalista;
    property DataCredito: TDateTime read FDataCredito write FDataCredito ;
    property DataAbatimento: TDateTime read FDataAbatimento write FDataAbatimento ;
    property DataDesconto: TDateTime read FDataDesconto write FDataDesconto ;
    property DataDesconto2: TDateTime read FDataDesconto2 write FDataDesconto2 ;
    property DataMoraJuros: TDateTime read FDataMoraJuros write FDataMoraJuros ;
    property DataMulta: TDateTime read FDataMulta write FDataMulta ;
    property DataProtesto: TDateTime read FDataProtesto write FDataProtesto ;
    property DiasDeProtesto: Integer read FDiasDeProtesto write FDiasDeProtesto ;
    property DataBaixa: TDateTime read FDataBaixa write FDataBaixa ;
    property HoraBaixa: String read FHoraBaixa write FHoraBaixa;
    property DataMovimento: TDateTime read FDataMovimento write FDataMovimento ;
    property DataLimitePagto: TDateTime read FDataLimitePagto write FDataLimitePagto ;
    property ValorDespesaCobranca: Currency read FValorDespesaCobranca write FValorDespesaCobranca ;
    property ValorAbatimento: Currency read FValorAbatimento write FValorAbatimento ;
    property ValorDesconto: Currency read FValorDesconto write FValorDesconto ;
    property ValorDesconto2: Currency read FValorDesconto2 write FValorDesconto2 ;
    property ValorMoraJuros: Currency read FValorMoraJuros write FValorMoraJuros ;
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
    property emv: String read Femv write Femv;
    property url_Pix: String read Furl_Pix write Furl_Pix;
    property Tx_ID: String read FTx_ID write FTx_ID;
    property CodigoCanalTituloCobranca: String read FCodigoCanalTituloCobranca write FCodigoCanalTituloCobranca;
    property EstadoTituloCobranca: String read FEstadoTituloCobranca write FEstadoTituloCobranca;
    property CodigoEstadoTituloCobranca: String read FCodigoEstadoTituloCobranca write FCodigoEstadoTituloCobranca;



  end;

  { TRetornoRejeicoesWeb }
  TRetornoRejeicoesWeb = class(TACBrLibRespostaBase)
  private
    FID : Integer;
    FIDRej : Integer;
    FCampo: String;
    FMensagem: String;
    FValor: String;
    FCodigo: String;
    FOcorrencia: String;
    FVersao: String;

  public
    constructor Create( const AID: Integer; const AIDRej: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const ARejeicao: TACBrBoletoRejeicao);

  published
    property Campo : String read FCampo write FCampo;
    property Mensagem : String read FMensagem write FMensagem;
    property Valor : String read FValor write FValor;
    property Codigo : String read FCodigo write FCodigo;
    property Ocorrencia : String read FOcorrencia write FOcorrencia;
    property Versao : String read FVersao write FVersao;

  end;

  { TRetornoRegistroWeb }
  TRetornoRegistroWeb = class(TACBrLibRespostaBase)
  private
    FID: Integer;

    FCodRetorno: String;
    FOriRetorno: String;
    FMsgRetorno: String;
    FExcecao   : String;

    FHTTPResultCode:Integer;
    FJSON: String;

    FIndicadorContinuidade: Boolean;
    FProximoIndice: Integer;

    FHeader_Versao: String;
    FHeader_Autenticacao: String;
    FHeader_Usuario_Servico: String;
    FHeader_Usuario: String;
    FHeader_Operacao: String;
    FHeader_Indice: Integer;
    FHeader_Sistema_Origem: String;
    FHeader_Agencia: Integer;
    FHeader_Id_Origem: String;
    FHeader_Data_Hora: TDateTime;
    FHeader_Id_Processo: String;
    FHeader_CNPJCPF_Beneficiario: String;

    FControleOriRetorno: String;
    FControleCodRetorno: String;
    FControleNSU: String;
    FControleRetorno: String;
    FControleHora: String;
    FControleData: TDateTime;

    FIDCodBarras: String;
    FIDLinhaDig: String;
    FIDNossoNum: String;
    FIDURL: String;

    FRejeicoes: TObjectList;
    FTituloRetorno: TRetornoTituloWeb;

  public
    constructor Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const RetEnvio: TACBrBoletoRetornoWS);

  published
    property CodRetorno: String                  read FCodRetorno                  write FCodRetorno;
    property OriRetorno: String                  read FOriRetorno                  write FOriRetorno;
    property MsgRetorno: String                  read FMsgRetorno                  write FMsgRetorno;
    property HTTPResultCode: Integer             read FHTTPResultCode              write FHTTPResultCode;
    property JSON: String                        read FJSON                        write FJSON;

    property Excecao: String                     read FExcecao                     write FExcecao;
    property IndicadorContinuidade: Boolean      read FIndicadorContinuidade       write FIndicadorContinuidade;
    property ProximoIndice: Integer              read FProximoIndice               write FProximoIndice;

    property Header_Versao: String               read FHeader_Versao               write FHeader_Versao;
    property Header_Autenticacao: String         read FHeader_Autenticacao         write FHeader_Autenticacao;
    property Header_Usuario_Servico: String      read FHeader_Usuario_Servico      write FHeader_Usuario_Servico;
    property Header_Usuario: String              read FHeader_Usuario              write FHeader_Usuario;
    property Header_Operacao: String             read FHeader_Operacao             write FHeader_Operacao;
    property Header_Indice: Integer              read FHeader_Indice               write FHeader_Indice;
    property Header_Sistema_Origem: String       read FHeader_Sistema_Origem       write FHeader_Sistema_Origem;
    property Header_Agencia: Integer             read FHeader_Agencia              write FHeader_Agencia;
    property Header_Id_Origem: String            read FHeader_Id_Origem            write FHeader_Id_Origem;
    property Header_Data_Hora: TDateTime         read FHeader_Data_Hora            write FHeader_Data_Hora;
    property Header_Id_Processo: String          read FHeader_Id_Processo          write FHeader_Id_Processo;
    property Header_CNPJCPF_Beneficiario: String read FHeader_CNPJCPF_Beneficiario write FHeader_CNPJCPF_Beneficiario;

    property ControleOriRetorno: String           read FControleOriRetorno         write FControleOriRetorno;
    property ControleCodRetorno: String           read FControleCodRetorno         write FControleCodRetorno;
    property ControleNSU: String                  read FControleNSU                write FControleNSU;
    property ControleRetorno: String              read FControleRetorno            write FControleRetorno;
    property ControleData: TDateTime              read FControleData               write FControleData;
    property ControleHora: String                 read FControleHora               write FControleHora;

    property IDCodBarras: String                  read FIDCodBarras                write FIDCodBarras;
    property IDLinhaDig: String                   read FIDLinhaDig                 write FIDLinhaDig;
    property IDNossoNum: String                   read FIDNossoNum                 write FIDNossoNum;
    property IDURL: String                        read FIDURL                      write FIDURL;

    property Rejeicoes: TObjectList               read FRejeicoes                  write FRejeicoes;
    property TituloRetorno: TRetornoTituloWeb     read FTituloRetorno              write FTituloRetorno;


  end;

implementation

uses
  TypInfo, pcnAuxiliar, pcnConversao,
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrLibBoletoConsts;

{ TRetornoRejeicoesWeb }
constructor TRetornoRejeicoesWeb.Create(const AID, AIDRej: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoRejeicao+ IntToStr(AID) + '-' + IntToStr(AIDRej);

  inherited Create(AChave, ATipo, AFormato);
  FID:= AID;
  FIDRej:= AIDRej;

end;

procedure TRetornoRejeicoesWeb.Processar(const ARejeicao: TACBrBoletoRejeicao);
begin
  Campo := ARejeicao.Campo;
  Mensagem := ARejeicao.Mensagem;
  Valor:= ARejeicao.Valor;
  Codigo:= ARejeicao.Codigo;
  Ocorrencia:= ARejeicao.Ocorrencia;
  Versao:= ARejeicao.Versao;
end;

{ TSacadoWeb }
constructor TSacadoWeb.Create(AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoSacado+ IntToStr(AID);

  inherited Create(AChave, ATipo, AFormato);
end;

procedure TSacadoWeb.Processar(const ASacado: TACBrBoletoSacadoRet);
begin
  Pessoa := ASacado.Pessoa;
  NomeSacado := ASacado.NomeSacado;
  CNPJCPF := ASacado.CNPJCPF;
  Logradouro := ASacado.Logradouro;
  Numero := ASacado.Numero;
  Complemento := ASacado.Complemento;
  Bairro := ASacado.Bairro;
  Cidade := ASacado.Cidade;
  UF := ASacado.UF;
  CEP := ASacado.CEP;
  Email := ASacado.Email;
  Fone := ASacado.Fone;
end;

{ TSacadoAvalistaWeb }
constructor TSacadoAvalistaWeb.Create( AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoSacadoAvalista+ IntToStr(AID);

  inherited Create(AChave, ATipo, AFormato);
end;

procedure TSacadoAvalistaWeb.Processar(const ASacadoAvalista: TACBrBoletoSacadoAvalistaRet);
begin
  Pessoa := ASacadoAvalista.Pessoa;
  NomeAvalista := ASacadoAvalista.NomeAvalista;
  CNPJCPF := ASacadoAvalista.CNPJCPF;
end;

{ TRetornoTituloWeb }
constructor TRetornoTituloWeb.Create(AID: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoTituloRetorno + IntToStr(AID); //TK-4477

  inherited Create(AChave, ATipo, AFormato);
  FAID:= AID;
  FSacado := TSacadoWeb.Create(AID, ATipo, AFormato);
  FSacadoAvalista := TSacadoAvalistaWeb.Create(AID, ATipo, AFormato);
end;

destructor TRetornoTituloWeb.Destroy;
begin
  inherited Destroy;

  FSacado.Free;
  FSacadoAvalista.Free;
end;

procedure TRetornoTituloWeb.Processar(const DadosRet: TACBrBoletoDadosRet);
begin
    CodBarras:= DadosRet.TituloRet.CodBarras;
    LinhaDig:= DadosRet.TituloRet.LinhaDig;
    URL:= DadosRet.TituloRet.URL;
    Instrucao1:= DadosRet.TituloRet.Instrucao1;
    Instrucao2:= DadosRet.TituloRet.Instrucao2;
    Instrucao3:= DadosRet.TituloRet.Instrucao3;
    Parcela:= DadosRet.TituloRet.Parcela;
    PercentualMulta:= DadosRet.TituloRet.PercentualMulta;
    MultaValorFixo:= DadosRet.TituloRet.MultaValorFixo;
    SeuNumero:= DadosRet.TituloRet.SeuNumero;
    TipoDiasProtesto:= DadosRet.TituloRet.TipoDiasProtesto;
    Vencimento:= DadosRet.TituloRet.Vencimento;
    DataDocumento:= DadosRet.TituloRet.DataDocumento;
    NumeroDocumento:= DadosRet.TituloRet.NumeroDocumento;
    EspecieDoc:= DadosRet.TituloRet.EspecieDoc;
    Aceite:= DadosRet.TituloRet.Aceite;
    DataProcessamento:= DadosRet.TituloRet.DataProcessamento;
    NossoNumero:= DadosRet.TituloRet.NossoNumero;
    UsoBanco:= DadosRet.TituloRet.UsoBanco;
    Carteira:= DadosRet.TituloRet.Carteira;
    EspecieMod:= DadosRet.TituloRet.EspecieMod;
    ValorDocumento:= DadosRet.TituloRet.ValorDocumento;
    Mensagem:= DadosRet.TituloRet.Mensagem;
    Informativo:= DadosRet.TituloRet.Informativo;
    Instrucoes:= DadosRet.TituloRet.Instrucoes;
    Sacado.Processar(DadosRet.TituloRet.Sacado);
    SacadoAvalista.Processar(DadosRet.TituloRet.SacadoAvalista);
    DataCredito:= DadosRet.TituloRet.DataCredito;
    DataAbatimento:= DadosRet.TituloRet.DataAbatimento;
    DataDesconto:= DadosRet.TituloRet.DataDesconto;
    DataDesconto2:= DadosRet.TituloRet.DataDesconto2;
    DataMoraJuros:= DadosRet.TituloRet.DataMoraJuros;
    DataMulta:= DadosRet.TituloRet.DataMulta;
    DataProtesto:= DadosRet.TituloRet.DataProtesto;
    DiasDeProtesto:= DadosRet.TituloRet.DiasDeProtesto;
    DataBaixa := DadosRet.TituloRet.DataBaixa;
    HoraBaixa := DadosRet.TituloRet.HoraBaixa;
    DataMovimento:= DadosRet.TituloRet.DataMovimento;
    DataLimitePagto:= DadosRet.TituloRet.DataLimitePagto;
    ValorDespesaCobranca:= DadosRet.TituloRet.ValorDespesaCobranca;
    ValorAbatimento:= DadosRet.TituloRet.ValorAbatimento;
    ValorDesconto:= DadosRet.TituloRet.ValorDesconto;
    ValorDesconto2:= DadosRet.TituloRet.ValorDesconto2;
    ValorMoraJuros:= DadosRet.TituloRet.ValorMoraJuros;
    ValorIOF:= DadosRet.TituloRet.ValorIOF;
    ValorOutrasDespesas:= DadosRet.TituloRet.ValorOutrasDespesas;
    ValorOutrosCreditos:= DadosRet.TituloRet.ValorOutrosCreditos;
    ValorRecebido:= DadosRet.TituloRet.ValorRecebido;
    CodigoMora:= DadosRet.TituloRet.CodigoMora;
    CarteiraEnvio:= DadosRet.TituloRet.CarteiraEnvio;
    CodigoNegativacao:= DadosRet.TituloRet.CodigoNegativacao;
    CodigoDesconto:= DadosRet.TituloRet.CodigoDesconto;
    CodigoMoraJuros:= DadosRet.TituloRet.CodigoMoraJuros;
    CodigoMulta:= DadosRet.TituloRet.CodigoMulta;
    ValorPago:= DadosRet.TituloRet.ValorPago;
    CaracTitulo:= DadosRet.TituloRet.CaracTitulo;
    TipoPagamento:= DadosRet.TituloRet.TipoPagamento;
    QtdePagamentoParcial:= DadosRet.TituloRet.QtdePagamentoParcial;
    QtdeParcelas:= DadosRet.TituloRet.QtdeParcelas;
    ValorMinPagamento:= DadosRet.TituloRet.ValorMinPagamento;
    ValorMaxPagamento:= DadosRet.TituloRet.ValorMaxPagamento;
    PercentualMinPagamento:= DadosRet.TituloRet.PercentualMinPagamento;
    PercentualMaxPagamento:= DadosRet.TituloRet.PercentualMaxPagamento;
    CodigoCanalTituloCobranca:=DadosRet.TituloRet.CodigoCanalTituloCobranca;
    EstadoTituloCobranca:=DadosRet.TituloRet.EstadoTituloCobranca;;
    CodigoEstadoTituloCobranca:=DadosRet.TituloRet.CodigoEstadoTituloCobranca;


    if ( DadosRet.TituloRet.EMV  <> EmptyStr) then
    begin
      emv:= DadosRet.TituloRet.EMV;
      url_Pix:= DadosRet.TituloRet.UrlPix;
      Tx_ID:= DadosRet.TituloRet.TxId;
    end;
    NossoNumeroCorrespondente:=DadosRet.TituloRet.NossoNumeroCorrespondente;

end;

{ TRetornoWebHeader }
constructor TRetornoRegistroWeb.Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoRegistro + IntToStr(AID);
  inherited Create(AChave, ATipo, AFormato);

  FID:= AID;
  FRejeicoes := TObjectList.Create(True);
  FTituloRetorno:= Nil;
end;

destructor TRetornoRegistroWeb.Destroy;
begin
  if Assigned(FTituloRetorno) then FreeAndNil(FTituloRetorno);

  FRejeicoes.Clear;
  FRejeicoes.Free;

  inherited Destroy;
end;

procedure TRetornoRegistroWeb.Processar(const RetEnvio: TACBrBoletoRetornoWS);
var
  J: Integer;
  Rejeicao: TRetornoRejeicoesWeb;
begin

  CodRetorno:= RetEnvio.CodRetorno;
  OriRetorno:= RetEnvio.OriRetorno;
  MsgRetorno:= RetEnvio.MsgRetorno;
  HTTPResultCode := RetEnvio.HTTPResultCode;
  JSON := RetEnvio.JSON;

  Excecao:= RetEnvio.DadosRet.Excecao;
  IndicadorContinuidade:= RetEnvio.indicadorContinuidade;

  ProximoIndice:= RetEnvio.proximoIndice;

  Header_Versao:= RetEnvio.Header.Versao;
  Header_Autenticacao:= RetEnvio.Header.Autenticacao;
  Header_Usuario_Servico:= RetEnvio.Header.Usuario_Servico;
  Header_Usuario:= RetEnvio.Header.Usuario;
  Header_Operacao:= TipoOperacaoToStr( RetEnvio.Header.Operacao );
  Header_Indice:= RetEnvio.Header.Indice;
  Header_Sistema_Origem:= RetEnvio.Header.Sistema_Origem;
  Header_Agencia:= RetEnvio.Header.Agencia;
  Header_Id_Origem:= RetEnvio.Header.Id_Origem;
  Header_Data_Hora:= RetEnvio.Header.Data_Hora;
  Header_Id_Processo:= RetEnvio.Header.Id_Processo;
  Header_CNPJCPF_Beneficiario:= RetEnvio.Header.CNPJCPF_Beneficiario;

  ControleOriRetorno:= RetEnvio.DadosRet.ControleNegocial.OriRetorno;
  ControleCodRetorno:= RetEnvio.DadosRet.ControleNegocial.CodRetorno;
  ControleNSU:= RetEnvio.DadosRet.ControleNegocial.NSU;
  ControleRetorno:= RetEnvio.DadosRet.ControleNegocial.CodRetorno;
  ControleHora:= RetEnvio.DadosRet.Comprovante.Hora;
  ControleData:= RetEnvio.DadosRet.Comprovante.Data;

  IDCodBarras:= RetEnvio.DadosRet.IDBoleto.CodBarras;
  IDLinhaDig:= RetEnvio.DadosRet.IDBoleto.LinhaDig;
  IDNossoNum:= RetEnvio.DadosRet.IDBoleto.NossoNum;
  IDURL:= RetEnvio.DadosRet.IDBoleto.URL;



  for J:= 0 to  RetEnvio.ListaRejeicao.Count -1 do
  begin
    Rejeicao := TRetornoRejeicoesWeb.Create(FID, J+1, Tipo, Formato);
    Rejeicao.Processar(RetEnvio.ListaRejeicao[J]);
    Rejeicoes.Add(Rejeicao);
  end;

  TituloRetorno := TRetornoTituloWeb.Create(FID, Tipo, Formato);
  TituloRetorno.Processar(RetEnvio.DadosRet);
end;

{ TRetornoRejeicoesTitulo }
constructor TRetornoRejeicoesTitulo.Create(const AIDRej: Integer; const AID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoMotivoRejeicao + IntToStr(AID + 1) + '-' + IntToStr(AIDRej);

  inherited Create( AChave, ATipo, AFormato);
  FID:= AID;
  FIDRej:= AIDRej;
end;

procedure TRetornoRejeicoesTitulo.Processar(const ACBrBoleto: TACBrBoleto);
begin
  MotivoRejeicao := ACBrBoleto.ListadeBoletos[FID].DescricaoMotivoRejeicaoComando[FIDRej];
end;

{ TRetornoBoleto }
constructor TRetornoBoleto.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRetorno, ATipo, AFormato);

  FCedente := nil;
  FBanco := nil;
  FConta := nil;
  FTitulo := TObjectList.Create(True);
end;

destructor TRetornoBoleto.Destroy;
begin
  if Assigned(FCedente) then FreeAndNil(FCedente);
  if Assigned(FBanco) then FreeAndNil(FBanco);
  if Assigned(FConta) then FreeAndNil(FConta);

  FTitulo.Clear;
  FTitulo.Free;

  inherited Destroy;
end;

procedure TRetornoBoleto.Processar(const ACBrBoleto: TACBrBoleto);
var
  I: Integer;
  Item: TRetornoDadosTitulo;
begin
  Cedente := TRetornoDadosCedente.Create(Tipo, Formato);
  Cedente.Processar(ACBrBoleto);

  Banco := TRetornoDadosBanco.Create(Tipo, Formato);
  Banco.Processar(ACBrBoleto);

  Conta := TRetornoDadosConta.Create(Tipo, Formato);
  Conta.Processar(ACBrBoleto);

  FTitulo.Clear;
  for I:= 0 to  ACBrBoleto.ListadeBoletos.Count-1 do
  begin
    Item := TRetornoDadosTitulo.Create(I, Tipo, Formato);
    Item.Processar(ACBrBoleto);
    FTitulo.Add(Item);
  end;
end;

{ TRetornoDadosTitulo }
constructor TRetornoDadosTitulo.Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoTitulo + IntToStr(AID+1), ATipo, AFormato);
  FID := AID;
  FRejeicoes := TObjectList.Create(True);

end;

destructor TRetornoDadosTitulo.Destroy;
begin
  if Assigned(FRejeicoes) then FRejeicoes.Free;

  inherited Destroy;
end;

procedure TRetornoDadosTitulo.Processar(const ACBrBoleto: TACBrBoleto);
var
  I: Integer;
  Item: TRetornoRejeicoesTitulo;
begin
  if ACBrBoleto.ListadeBoletos.Count > 0 then
  begin
    Sacado_Nome := ACBrBoleto.ListadeBoletos[FID].Sacado.NomeSacado;
    Sacado_CNPJCPF := ACBrBoleto.ListadeBoletos[FID].Sacado.CNPJCPF;
    Vencimento := ACBrBoleto.ListadeBoletos[FID].Vencimento;
    DataDocumento := ACBrBoleto.ListadeBoletos[FID].DataDocumento;
    NumeroDocumento := ACBrBoleto.ListadeBoletos[FID].NumeroDocumento;
    DataProcessamento := ACBrBoleto.ListadeBoletos[FID].DataProcessamento;
    NossoNumero := ACBrBoleto.ListadeBoletos[FID].NossoNumero;
    NossoNumeroCorrespondente:=ACBrBoleto.ListadeBoletos[FID].NossoNumeroCorrespondente;
    Carteira := ACBrBoleto.ListadeBoletos[FID].Carteira;
    ValorDocumento := ACBrBoleto.ListadeBoletos[FID].ValorDocumento;
    DataOcorrencia := ACBrBoleto.ListadeBoletos[FID].DataOcorrencia;
    DataCredito := ACBrBoleto.ListadeBoletos[FID].DataCredito;
    DataBaixa := ACBrBoleto.ListadeBoletos[FID].DataBaixa;
    HoraBaixa := ACBrBoleto.ListadeBoletos[FID].HoraBaixa;
    DataMoraJuros := ACBrBoleto.ListadeBoletos[FID].DataMoraJuros;
    ValorDespesaCobranca := ACBrBoleto.ListadeBoletos[FID].ValorDespesaCobranca;
    ValorAbatimento := ACBrBoleto.ListadeBoletos[FID].ValorAbatimento;
    ValorDesconto := ACBrBoleto.ListadeBoletos[FID].ValorDesconto;
    ValorMoraJuros := ACBrBoleto.ListadeBoletos[FID].ValorMoraJuros;
    ValorIOF := ACBrBoleto.ListadeBoletos[FID].ValorIOF;
    ValorOutrasDespesas := ACBrBoleto.ListadeBoletos[FID].ValorOutrasDespesas;
    ValorOutrosCreditos := ACBrBoleto.ListadeBoletos[FID].ValorOutrosCreditos;
    ValorRecebido := ACBrBoleto.ListadeBoletos[FID].ValorRecebido;
    ValorPago := ACBrBoleto.ListadeBoletos[FID].ValorPago;
    SeuNumero := ACBrBoleto.ListadeBoletos[FID].SeuNumero;
    CodTipoOcorrencia := GetEnumName( TypeInfo(TACBrTipoOcorrencia),
                                             Integer(ACBrBoleto.ListadeBoletos[FID].OcorrenciaOriginal.Tipo));
    DescricaoTipoOcorrencia := ACBrBoleto.ListadeBoletos[FID].OcorrenciaOriginal.Descricao;

    for I:= 0 to  ACBrBoleto.ListadeBoletos[FID].DescricaoMotivoRejeicaoComando.Count-1 do
    begin
      Item := TRetornoRejeicoesTitulo.Create( I, FID , Tipo, Formato);
      Item.Processar(ACBrBoleto);
      Rejeicoes.Add(Item);
    end;

  end;
end;

{ TRetornoDadosConta }
constructor TRetornoDadosConta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoConta, ATipo, AFormato);
end;

destructor TRetornoDadosConta.Destroy;
begin
  inherited Destroy;
end;

procedure TRetornoDadosConta.Processar(const ACBrBoleto: TACBrBoleto);
begin
  if Assigned(ACBrBoleto) then
  begin
    Conta := ACBrBoleto.Cedente.Conta;
    DigitoConta := ACBrBoleto.Cedente.ContaDigito;
    Agencia := ACBrBoleto.Cedente.Agencia;
    DigitoAgencia := ACBrBoleto.Cedente.AgenciaDigito;
    DigitoVerificadorAgenciaConta := ACBrBoleto.Cedente.DigitoVerificadorAgenciaConta;
  end;
end;

{ TRetornoDadosBanco }
constructor TRetornoDadosBanco.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoBanco, ATipo, AFormato);
end;

destructor TRetornoDadosBanco.Destroy;
begin
  inherited Destroy;
end;

procedure TRetornoDadosBanco.Processar(const ACBrBoleto: TACBrBoleto);
begin
  if Assigned(ACBrBoleto) then
  begin
    Numero := ACBrBoleto.Banco.Numero;
    IndiceACBr := Integer(ACBrBoleto.Banco.TipoCobranca);
    NumeroCorrespondente := ACBrBoleto.Banco.NumeroCorrespondente;
    VersaoArquivo := ACBrBoleto.Banco.LayoutVersaoArquivo;
    VersaoLote := ACBrBoleto.Banco.LayoutVersaoLote;
    NumeroArquivo := ACBrBoleto.NumeroArquivo;
    NomeArqRetorno := ACBrBoleto.NomeArqRetorno;
    DensidadeGravacao := ACBrBoleto.Banco.DensidadeGravacao;
    CIP := ACBrBoleto.Banco.CIP;
  end;
end;

{ TRetornoDadosCedente }
constructor TRetornoDadosCedente.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoCedente, ATipo, AFormato);
end;

destructor TRetornoDadosCedente.Destroy;
begin
  inherited Destroy;
end;

procedure TRetornoDadosCedente.Processar(const ACBrBoleto: TACBrBoleto);
begin
  if Assigned(ACBrBoleto) then
  begin
    Nome := ACBrBoleto.Cedente.Nome;
    CNPJCPF := ACBrBoleto.Cedente.CNPJCPF;
    CodigoCedente := ACBrBoleto.Cedente.CodigoCedente;
    Modalidade := ACBrBoleto.Cedente.Modalidade;
    CodTransmissao := ACBrBoleto.Cedente.CodigoTransmissao;
    Convenio := ACBrBoleto.Cedente.Convenio;
  end;
end;

end.

