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

  THeader = class
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

  TControleNegocial = class
  private
    FOriRetorno: String;
    FCodRetorno: String;
    FNSU: String;
    FRetorno: String;
  public
    property OriRetorno: String  read FOriRetorno write FOriRetorno;
    property CodRetorno: String  read FCodRetorno write FCodRetorno;
    property NSU: String         read FNSU        write FNSU;
    property Retorno: String     read FRetorno    write FRetorno;
  end;

  TComprovante = class
  private
    FHora: String;
    FData: TDateTime;
  public
    property Data: TDateTime read FData write FData;
    property Hora: String    read FHora write FHora;
  end;

  TIDBoleto = class
  private
    FCodBarras: String;
    FLinhaDig: String;
    FNossoNum: String;
    FURL: String;
  public
    property CodBarras: String read FCodBarras write FCodBarras;
    property LinhaDig: String  read FLinhaDig  write FLinhaDig;
    property NossoNum: String  read FNossoNum  write FNossoNum;
    property URL: String       read FURL       write FURL;
  end;

  TSacadoAvalistaRet = class
  private
    FTipoPessoa: TACBrPessoa;
    FNomeAvalista: String;
    FCNPJCPF: String;

  public
    property Pessoa: TACBrPessoa  read FTipoPessoa   write FTipoPessoa;
    property NomeAvalista: String read FNomeAvalista write FNomeAvalista;
    property CNPJCPF: String      read FCNPJCPF      write FCNPJCPF;

  end;


  TSacadoRet = class
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

   { TTituloRet }

   TTituloRet = class
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
    constructor Create;
    destructor Destroy; override;

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

  { TDadosRet }
  TDadosRet = class
  private
    FControleNegocial: TControleNegocial;
    FComprovante: TComprovante;
    FExcecao: String;
    FIDBoleto: TIDBoleto;
    FTituloRet: TTituloRet;

  public
    constructor Create;
    destructor Destroy; override;

    property ControleNegocial: TControleNegocial read FControleNegocial write FControleNegocial;
    property Comprovante: TComprovante           read FComprovante      write FComprovante;
    property Excecao: String                     read FExcecao          write FExcecao;
    property IDBoleto: TIDBoleto                 read FIDBoleto         write FIDBoleto;
    property TituloRet: TTituloRet               read FTituloRet        write FTituloRet;

  end;

  TRejeicao = class
  private
    FCampo: String;
    FMensagem: String;
    FValor: String;
  public
    property Campo: String  read FCampo write FCampo;
    property Mensagem: String  read FMensagem write FMensagem;
    property Valor: String read FValor write FValor;
  end;

  { TListaRejeicao }
  TListaRejeicao = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRejeicao);
    function  GetObject (Index: Integer): TRejeicao;
    procedure Insert (Index: Integer; Obj: TRejeicao);
  public
    function Add (Obj: TRejeicao): Integer;
    property Objects [Index: Integer]: TRejeicao read GetObject write SetObject; default;
  end;


  { TRetEnvio }
  TRetEnvio = class
  private
    FHeader: THeader;
    FCodRetorno: String;
    FOriRetorno: String;
    FMsgRetorno: String;
    FDadosRet: TDadosRet;
    FListaRejeicao: TListaRejeicao;
  public
    constructor Create;
    destructor Destroy; override;

    function CriarRejeicaoLista: TRejeicao;
    property Header: THeader     read FHeader     write FHeader;
    property CodRetorno: String  read FCodRetorno write FCodRetorno;
    property OriRetorno: String  read FOriRetorno write FOriRetorno;
    property MsgRetorno: String  read FMsgRetorno write FMsgRetorno;
    property DadosRet: TDadosRet read FDadosRet   write FDadosRet;
    property ListaRejeicao: TListaRejeicao read FListaRejeicao;
  end;

  { TListaRetEnvio }
  TListaRetEnvio = class(TObjectList)
  protected
    procedure SetObject (Index: Integer; Item: TRetEnvio);
    function  GetObject (Index: Integer): TRetEnvio;
    procedure Insert (Index: Integer; Obj: TRetEnvio);
  public
    function Add (Obj: TRetEnvio): Integer;
    property Objects [Index: Integer]: TRetEnvio read GetObject write SetObject; default;
  end;

implementation

{ TListaRejeicao }

procedure TListaRejeicao.SetObject(Index: Integer; Item: TRejeicao);
begin
  inherited SetItem (Index, Item) ;
end;

function TListaRejeicao.GetObject(Index: Integer): TRejeicao;
begin
  Result := inherited GetItem(Index) as TRejeicao ;
end;

procedure TListaRejeicao.Insert(Index: Integer; Obj: TRejeicao);
begin
  inherited Insert(Index, Obj);
end;

function TListaRejeicao.Add(Obj: TRejeicao): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TTituloRet }

constructor TTituloRet.Create;
begin
  FSacadoAvalista := TSacadoAvalistaRet.Create;
  FSacado := TSacadoRet.Create;
  FMensagem       := TStringList.Create;
  FInformativo    := TStringList.Create;
  FInstrucoes     := TStringList.Create;
end;

destructor TTituloRet.Destroy;
begin
  FSacadoAvalista.Free;
  FSacado.Free;
  FMensagem.Free;
  FInformativo.Free;
  FInstrucoes.Free;
  inherited Destroy;
end;

{ TListaRetEnvio }

procedure TListaRetEnvio.SetObject(Index: Integer; Item: TRetEnvio);
begin
  inherited SetItem (Index, Item) ;
end;

function TListaRetEnvio.GetObject(Index: Integer): TRetEnvio;
begin
  Result := inherited GetItem(Index) as TRetEnvio ;
end;

procedure TListaRetEnvio.Insert(Index: Integer; Obj: TRetEnvio);
begin
   inherited Insert(Index, Obj);
end;

function TListaRetEnvio.Add(Obj: TRetEnvio): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TRetEnvio }

constructor TRetEnvio.Create;
begin
  FHeader    := THeader.Create;
  FDadosRet  := TDadosRet.Create;
  FListaRejeicao := TListaRejeicao.Create(true);
end;

destructor TRetEnvio.Destroy;
begin
  FHeader.Free;
  FDadosRet.Free;
  FListaRejeicao.Free;
  inherited;
end;

function TRetEnvio.CriarRejeicaoLista: TRejeicao;
var
  I: Integer;
begin
   I      := FListaRejeicao.Add(TRejeicao.Create);
   Result := FListaRejeicao[I];
end;

{ TDadosRet }

constructor TDadosRet.Create;
begin
  FControleNegocial := TControleNegocial.Create;
  FTituloRet        := TTituloRet.Create;
  FComprovante      := TComprovante.Create;
  FIDBoleto         := TIDBoleto.Create;

end;

destructor TDadosRet.Destroy;
begin
  FControleNegocial.Free;
  FTituloRet.Free;
  FComprovante.Free;
  FIDBoleto.Free;

  inherited;
end;

end.

