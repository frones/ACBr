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

  { TLibBoletoServiceResposta }
  TLibBoletoServiceResposta = class abstract(TACBrLibResposta<TACBrBoleto>)
  private

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBrBoleto: TACBrBoleto); virtual; abstract; reintroduce;

  end;

  { TRetornoDadosCedente }
  TRetornoDadosCedente = class(TLibBoletoServiceResposta)
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
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property Nome : String read FNome Write FNome;
    property CNPJCPF : String read FCNPJCPF Write FCNPJCPF;
    property CodigoCedente : String read FCodigoCedente Write FCodigoCedente;
    property Modalidade : String read FModalidade Write FModalidade;
    property CodTransmissao : String read FCodTransmissao Write FCodTransmissao;
    property Convenio : String read FConvenio Write FConvenio;

  end;

  { TRetornoDadosBanco }
  TRetornoDadosBanco = class(TLibBoletoServiceResposta)
  private
    FNumero : Integer;
    FIndiceACBr : Integer;
    FNumeroCorrespondente : Integer;
    FVersaoArquivo : Integer;
    FVersaoLote : Integer;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property Numero : Integer read FNumero write FNumero;
    property IndiceACBr : Integer read FIndiceACBr write FIndiceACBr ;
    property NumeroCorrespondente : Integer read FNumeroCorrespondente write FNumeroCorrespondente;
    property VersaoArquivo : Integer read FVersaoArquivo write FVersaoArquivo;
    property VersaoLote : Integer read FVersaoLote write FVersaoLote;

  end;

  { TRetornoDadosConta }
  TRetornoDadosConta = class(TLibBoletoServiceResposta)
  private
    FConta : String;
    FDigitoConta : String;
    FAgencia : String;
    FDigitoAgencia : String;
    FDigitoVerificadorAgenciaConta : String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

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
  TRetornoDadosTitulo = class(TLibBoletoServiceResposta)
  private
    FID: Integer;
    FSacado_Nome : String;
    FSacado_CNPJCPF : String;
    FVencimento: TDateTime;
    FDataDocumento: TDateTime;
    FNumeroDocumento: String;
    FDataProcessamento: TDateTime;
    FNossoNumero: String;
    FCarteira: String;
    FValorDocumento: TDateTime;
    FDataOcorrencia: TDateTime;
    FDataCredito: TDateTime;
    FDataBaixa: TDateTime;
    FDataMoraJuros: TDateTime;
    FValorDespesaCobranca: Currency;
    FValorAbatimento: Currency;
    FValorDesconto: Currency;
    FValorMoraJuros: Currency;
    FValorIOF: Currency;
    FValorOutrasDespesas: Currency;
    FValorOutrosCreditos: Currency;
    FValorRecebido: Currency;
    FSeuNumero: String;
    FCodTipoOcorrencia: String;
    FDescricaoTipoOcorrencia: String;
    FRejeicoes: TObjectList;

  public
    constructor Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar( const ACBrBoleto: TACBrBoleto); override;

  published
    property Sacado_Nome : String read FSacado_Nome write FSacado_Nome;
    property Sacado_CNPJCPF : String read FSacado_CNPJCPF write FSacado_CNPJCPF;
    property Vencimento: TDateTime read FVencimento write FVencimento;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property NumeroDocumento: String read FNumeroDocumento write FNumeroDocumento;
    property DataProcessamento: TDateTime read FDataProcessamento write FDataProcessamento;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property Carteira: String read FCarteira write FCarteira;
    property ValorDocumento: TDateTime read FValorDocumento write FValorDocumento;
    property DataOcorrencia: TDateTime read FDataOcorrencia write FDataOcorrencia;
    property DataCredito: TDateTime read FDataCredito write FDataCredito;
    property DataBaixa: TDateTime read FDataBaixa write FDataBaixa;
    property DataMoraJuros: TDateTime read FDataMoraJuros write FDataMoraJuros;
    property ValorDespesaCobranca: Currency read FValorDespesaCobranca write FValorDespesaCobranca;
    property ValorAbatimento: Currency read FValorAbatimento write FValorAbatimento;
    property ValorDesconto: Currency read FValorDesconto write FValorDesconto;
    property ValorMoraJuros: Currency read FValorMoraJuros write FValorMoraJuros;
    property ValorIOF: Currency read FValorIOF write FValorIOF;
    property ValorOutrasDespesas: Currency read FValorOutrasDespesas write FValorOutrasDespesas;
    property ValorOutrosCreditos: Currency read FValorOutrosCreditos write FValorOutrosCreditos;
    property ValorRecebido: Currency read FValorRecebido write FValorRecebido;
    property SeuNumero: String read FSeuNumero write FSeuNumero;
    property CodTipoOcorrencia: String read FCodTipoOcorrencia write FCodTipoOcorrencia;
    property DescricaoTipoOcorrencia: String read FDescricaoTipoOcorrencia write FDescricaoTipoOcorrencia;
    property Rejeicoes: TObjectList read FRejeicoes write FRejeicoes;

  end;

  { TRetornoBoleto }

  TRetornoBoleto = class(TLibBoletoServiceResposta)
  private
    FCedente: TRetornoDadosCedente;
    FBanco: TRetornoDadosBanco;
    FConta: TRetornoDadosConta;
    FTitulo: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property Cedente: TRetornoDadosCedente read FCedente write FCedente;
    property Banco: TRetornoDadosBanco read FBanco write FBanco;
    property Conta: TRetornoDadosConta read FConta write FConta;
    property Titulo: TObjectList read FTitulo;

  end;

  {TRetornoTituloWeb}
  TRetornoTituloWeb =  class(TLibBoletoServiceResposta)
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
    FUsoBanco: String;
    FCarteira: String;
    FEspecieMod: String;
    FValorDocumento: Currency;
    FMensagem: TStrings;
    FInformativo: TStrings;
    FInstrucoes: TStrings;
    FSacado: TSacadoRet;
    FSacadoAvalista: TSacadoAvalistaRet;
    FDataCredito: TDateTime;
    FDataAbatimento: TDateTime;
    FDataDesconto: TDateTime;
    FDataDesconto2: TDateTime;
    FDataMoraJuros: TDateTime;
    FDataMulta: TDateTime;
    FDataProtesto: TDateTime;
    FDiasDeProtesto: Integer;
    FDataBaixa: TDateTime;
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

  public
    constructor Create( AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;
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
    property UsoBanco: String read FUsoBanco write FUsoBanco ;
    property Carteira: String read FCarteira write FCarteira ;
    property EspecieMod: String read FEspecieMod write FEspecieMod ;
    property ValorDocumento: Currency read FValorDocumento write FValorDocumento ;
    property Mensagem: TStrings read FMensagem write FMensagem ;
    property Informativo: TStrings read FInformativo write FInformativo ;
    property Instrucoes: TStrings read FInstrucoes write FInstrucoes ;
    property Sacado: TSacadoRet read FSacado write FSacado ;
    property SacadoAvalista: TSacadoAvalistaRet read FSacadoAvalista write FSacadoAvalista ;
    property DataCredito: TDateTime read FDataCredito write FDataCredito ;
    property DataAbatimento: TDateTime read FDataAbatimento write FDataAbatimento ;
    property DataDesconto: TDateTime read FDataDesconto write FDataDesconto ;
    property DataDesconto2: TDateTime read FDataDesconto2 write FDataDesconto2 ;
    property DataMoraJuros: TDateTime read FDataMoraJuros write FDataMoraJuros ;
    property DataMulta: TDateTime read FDataMulta write FDataMulta ;
    property DataProtesto: TDateTime read FDataProtesto write FDataProtesto ;
    property DiasDeProtesto: Integer read FDiasDeProtesto write FDiasDeProtesto ;
    property DataBaixa: TDateTime read FDataBaixa write FDataBaixa ;
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

  end;

  { TRetornoRejeicoesWeb }
  TRetornoRejeicoesWeb = class(TACBrLibRespostaBase)
  private
    FID : Integer;
    FIDRej : Integer;
    FCampo: String;
    FMensagem: String;
    FValor: String;

  public
    constructor Create( const AID: Integer; const AIDRej: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const ACBrBoleto: TACBrBoleto);

  published
    property Campo : String read FCampo write FCampo;
    property Mensagem : String read FMensagem write FMensagem;
    property Valor : String read FValor write FValor;

  end;

  { TRetornoRegistroWeb }
  TRetornoRegistroWeb = class(TLibBoletoServiceResposta)
  private
    FID: Integer;

    FCodRetorno: String;
    FOriRetorno: String;
    FMsgRetorno: String;
    FExcecao: String;

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

    FRejeicao: TRetornoRejeicoesWeb;
    FTituloRetorno: TRetornoTituloWeb;

  public
    constructor Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property CodRetorno: String                  read FCodRetorno                  write FCodRetorno;
    property OriRetorno: String                  read FOriRetorno                  write FOriRetorno;
    property MsgRetorno: String                  read FMsgRetorno                  write FMsgRetorno;
    property Excecao: String                     read FExcecao                     write FExcecao;

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

    property Rejeicao: TRetornoRejeicoesWeb       read FRejeicao                   write FRejeicao;
    property TituloRetorno: TRetornoTituloWeb     read FTituloRetorno              write FTituloRetorno;


  end;

implementation

uses
  TypInfo, pcnAuxiliar, pcnConversao,
  ACBrUtil, ACBrLibBoletoConsts;

{ TRetornoRejeicoesWeb }

constructor TRetornoRejeicoesWeb.Create(const AID, AIDRej: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoRejeicao+ IntToStr(AID + 1) + '-' + IntToStr(AIDRej + 1);

  inherited Create( AChave, ATipo, AFormato);
  FID:= AID;
  FIDRej:= AIDRej;

end;

procedure TRetornoRejeicoesWeb.Processar(const ACBrBoleto: TACBrBoleto);
begin
  Campo := ACBrBoleto.ListaRetornoWeb[FID].ListaRejeicao[FIDRej].Campo;
  Mensagem := ACBrBoleto.ListaRetornoWeb[FID].ListaRejeicao[FIDRej].Mensagem;
  Valor:= ACBrBoleto.ListaRetornoWeb[FID].ListaRejeicao[FIDRej].Valor;
end;

{ TRetornoTituloWeb }

constructor TRetornoTituloWeb.Create(AID: Integer;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoTituloRetorno, ATipo, AFormato);
  FAID:= AID;
end;

procedure TRetornoTituloWeb.Processar(const ACBrBoleto: TACBrBoleto);
begin
  if ACBrBoleto.ListaRetornoWeb.Count > 0 then
  begin

    CodBarras:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CodBarras;
    LinhaDig:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.LinhaDig;
    URL:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.URL;
    Instrucao1:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Instrucao1;
    Instrucao2:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Instrucao2;
    Instrucao3:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Instrucao3;
    Parcela:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Parcela;
    PercentualMulta:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.PercentualMulta;
    MultaValorFixo:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.MultaValorFixo;
    SeuNumero:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.SeuNumero;
    TipoDiasProtesto:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.TipoDiasProtesto;
    Vencimento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Vencimento;
    DataDocumento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataDocumento;
    NumeroDocumento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.NumeroDocumento;
    EspecieDoc:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.EspecieDoc;
    Aceite:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Aceite;
    DataProcessamento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataProcessamento;
    NossoNumero:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.NossoNumero;
    UsoBanco:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.UsoBanco;
    Carteira:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Carteira;
    EspecieMod:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.EspecieMod;
    ValorDocumento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorDocumento;
    Mensagem:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Mensagem;
    Informativo:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Informativo;
    Instrucoes:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Instrucoes;
    Sacado:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.Sacado;
    SacadoAvalista:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.SacadoAvalista;
    DataCredito:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataCredito;
    DataAbatimento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataAbatimento;
    DataDesconto:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataDesconto;
    DataDesconto2:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataDesconto2;
    DataMoraJuros:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataMoraJuros;
    DataMulta:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataMulta;
    DataProtesto:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataProtesto;
    DiasDeProtesto:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DiasDeProtesto;
    DataBaixa:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataBaixa;
    DataLimitePagto:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.DataLimitePagto;
    ValorDespesaCobranca:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorDespesaCobranca;
    ValorAbatimento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorAbatimento;
    ValorDesconto:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorDesconto;
    ValorDesconto2:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorDesconto2;
    ValorMoraJuros:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorMoraJuros;
    ValorIOF:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorIOF;
    ValorOutrasDespesas:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorOutrasDespesas;
    ValorOutrosCreditos:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorOutrosCreditos;
    ValorRecebido:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorRecebido;
    CodigoMora:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CodigoMora;
    CarteiraEnvio:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CarteiraEnvio;
    CodigoNegativacao:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CodigoNegativacao;
    CodigoDesconto:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CodigoDesconto;
    CodigoMoraJuros:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CodigoMoraJuros;
    CodigoMulta:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CodigoMulta;
    ValorPago:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorPago;
    CaracTitulo:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.CaracTitulo;
    TipoPagamento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.TipoPagamento;
    QtdePagamentoParcial:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.QtdePagamentoParcial;
    QtdeParcelas:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.QtdeParcelas;
    ValorMinPagamento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorMinPagamento;
    ValorMaxPagamento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.ValorMaxPagamento;
    PercentualMinPagamento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.PercentualMinPagamento;
    PercentualMaxPagamento:= ACBrBoleto.ListaRetornoWeb[FAID].DadosRet.TituloRet.PercentualMaxPagamento;

  end;

end;

destructor TRetornoTituloWeb.Destroy;
begin
  inherited Destroy;
end;

{ TRetornoWebHeader }

constructor TRetornoRegistroWeb.Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoRegistro + IntToStr(AID + 1) ;
  inherited Create(AChave, ATipo, AFormato);

  FID:= AID;
  FRejeicao:= Nil;
  FTituloRetorno:= Nil;

end;

destructor TRetornoRegistroWeb.Destroy;
begin
  if Assigned(FTituloRetorno) then FreeAndNil(FTituloRetorno);
  if Assigned(FRejeicao) then FreeAndNil(FRejeicao);

  inherited Destroy;
end;

procedure TRetornoRegistroWeb.Processar(const ACBrBoleto: TACBrBoleto);
var
  I: Integer;
begin
  if ACBrBoleto.ListaRetornoWeb.Count > 0 then
  begin

    CodRetorno:= ACBrBoleto.ListaRetornoWeb[FID].CodRetorno;
    OriRetorno:= ACBrBoleto.ListaRetornoWeb[FID].OriRetorno;
    MsgRetorno:= ACBrBoleto.ListaRetornoWeb[FID].MsgRetorno;
    Excecao:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.Excecao;

    Header_Versao:= ACBrBoleto.ListaRetornoWeb[FID].Header.Versao;
    Header_Autenticacao:= ACBrBoleto.ListaRetornoWeb[FID].Header.Autenticacao;
    Header_Usuario_Servico:= ACBrBoleto.ListaRetornoWeb[FID].Header.Usuario_Servico;
    Header_Usuario:= ACBrBoleto.ListaRetornoWeb[FID].Header.Usuario;
    Header_Operacao:= TipoOperacaoToStr( ACBrBoleto.ListaRetornoWeb[FID].Header.Operacao );
    Header_Indice:= ACBrBoleto.ListaRetornoWeb[FID].Header.Indice;
    Header_Sistema_Origem:= ACBrBoleto.ListaRetornoWeb[FID].Header.Sistema_Origem;
    Header_Agencia:= ACBrBoleto.ListaRetornoWeb[FID].Header.Agencia;
    Header_Id_Origem:= ACBrBoleto.ListaRetornoWeb[FID].Header.Id_Origem;
    Header_Data_Hora:= ACBrBoleto.ListaRetornoWeb[FID].Header.Data_Hora;
    Header_Id_Processo:= ACBrBoleto.ListaRetornoWeb[FID].Header.Id_Processo;
    Header_CNPJCPF_Beneficiario:= ACBrBoleto.ListaRetornoWeb[FID].Header.CNPJCPF_Beneficiario;

    ControleOriRetorno:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.ControleNegocial.OriRetorno;
    ControleCodRetorno:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.ControleNegocial.CodRetorno;
    ControleNSU:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.ControleNegocial.NSU;
    ControleRetorno:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.ControleNegocial.CodRetorno;
    ControleHora:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.Comprovante.Hora;
    ControleData:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.Comprovante.Data;

    IDCodBarras:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.IDBoleto.CodBarras;
    IDLinhaDig:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.IDBoleto.LinhaDig;
    IDNossoNum:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.IDBoleto.NossoNum;
    IDURL:= ACBrBoleto.ListaRetornoWeb[FID].DadosRet.IDBoleto.URL;

    for I:= 0 to  ACBrBoleto.ListaRetornoWeb[FID].ListaRejeicao.Count do
    begin
      Rejeicao := TRetornoRejeicoesWeb.Create( FID, I, Tipo, Formato);
      Rejeicao.Processar(ACBrBoleto);
    end;

    TituloRetorno := TRetornoTituloWeb.Create(FID, Tipo, Formato);
    TituloRetorno.Processar(ACBrBoleto);
  end;

end;

{ TRetornoRejeicoesTitulo }

constructor TRetornoRejeicoesTitulo.Create(const AIDRej: Integer; const AID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoMotivoRejeicao + IntToStr(AID + 1) + '-' + IntToStr(AIDRej + 1);

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
  inherited Create(CSessaoTitulo + IntToStr(AID + 1), ATipo, AFormato);
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
    Carteira := ACBrBoleto.ListadeBoletos[FID].Carteira;
    ValorDocumento := ACBrBoleto.ListadeBoletos[FID].ValorDocumento;
    DataOcorrencia := ACBrBoleto.ListadeBoletos[FID].DataOcorrencia;
    DataCredito := ACBrBoleto.ListadeBoletos[FID].DataCredito;
    DataBaixa := ACBrBoleto.ListadeBoletos[FID].DataBaixa;
    DataMoraJuros := ACBrBoleto.ListadeBoletos[FID].DataMoraJuros;
    ValorDespesaCobranca := ACBrBoleto.ListadeBoletos[FID].ValorDespesaCobranca;
    ValorAbatimento := ACBrBoleto.ListadeBoletos[FID].ValorAbatimento;
    ValorDesconto := ACBrBoleto.ListadeBoletos[FID].ValorDesconto;
    ValorMoraJuros := ACBrBoleto.ListadeBoletos[FID].ValorMoraJuros;
    ValorIOF := ACBrBoleto.ListadeBoletos[FID].ValorIOF;
    ValorOutrasDespesas := ACBrBoleto.ListadeBoletos[FID].ValorOutrasDespesas;
    ValorOutrosCreditos := ACBrBoleto.ListadeBoletos[FID].ValorOutrosCreditos;
    ValorRecebido := ACBrBoleto.ListadeBoletos[FID].ValorRecebido;
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

  end;

end;

{ TLibBoletoServiceResposta }

constructor TLibBoletoServiceResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
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

