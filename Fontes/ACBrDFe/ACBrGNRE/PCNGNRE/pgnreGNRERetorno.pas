{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti                              }
{                              Claudemir Vitor Pereira                         }
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

unit pgnreGNRERetorno;

interface

uses
  SysUtils, Classes,
  pcnConversao, pgnreRetConsResLoteGNRE;

type
  TGNRERetorno = class(TObject)
  private
    FIdentificador: Integer;
    FSequencialGuia: Integer;
    FSituacaoGuia: string;
    FUFFavorecida: string;
    FCodReceita: Integer;
    FTipoDocEmitente: Integer;
    FDocEmitente: string;
    FRazaoSocialEmitente: string;
    FEnderecoEmitente: string;
    FMunicipioEmitente: string;
    FMunicipioEmitenteNome: string;
    FUFEmitente: string;
    FCEPEmitente: string;
    FTelefoneEmitente: string;
    FTipoDocDestinatario: Integer;
    FDocDestinatario: string;
    FMunicipioDestinatario: string;
    FMunicipioDestinatarioNome: string;
    FProduto: string;
    FNumDocOrigem: string;
    FConvenio: string;
    FInfoComplementares: string;
    FDataVencimento: string;
    FDataLimitePagamento: string;
    FPeriodoReferencia: string;
    FMesAnoReferencia: string;
    FParcela: Integer;
    FValorPrincipal: Currency;
    FValorFCP: Currency;
    FAtualizacaoMonetaria: Currency;
    FAtualizacaoMonetariaFCP: Currency;
    FJuros: Currency;
    FJurosFCP: Currency;
    FMulta: Currency;
    FMultaFCP: Currency;
    FRepresentacaoNumerica: string;
    FCodigoBarras: string;
    FQtdeVias: Integer;
    FNumeroControle: string;
    FIdentificadorGuia: string;
    FGuiaGeradaContingencia: Integer;
    FReservado: string;
    FInfoCabec: TInfoCabec;
    FtipoGnre: string;
    FValorICMS: Currency;
    FAtualMonetFECP: Currency;
    FTotalFECP: Currency;
    FJurosFECP: Currency;
    FMultaFECP: Currency;
    FAtualMonetICMS: Currency;
    FJurosICMS: Currency;
    FMultaICMS: Currency;
    FValorFECP: Currency;
    FValorPrincICMS: Currency;
    FvalorGNRE: Currency;
    FqrcodePayload: String;
    FDadosPagamento: TDadosPagamento;
  public
    constructor Create;
    destructor Destroy; override;

    property Identificador: Integer read FIdentificador write FIdentificador;
    property SequencialGuia: Integer read FSequencialGuia write FSequencialGuia;
    property SituacaoGuia: string read FSituacaoGuia write FSituacaoGuia;
    property UFFavorecida: string read FUFFavorecida write FUFFavorecida;
    property CodReceita: Integer read FCodReceita write FCodReceita;
    property TipoDocEmitente: Integer read FTipoDocEmitente write FTipoDocEmitente;
    property DocEmitente: string read FDocEmitente write FDocEmitente;
    property RazaoSocialEmitente: string read FRazaoSocialEmitente write FRazaoSocialEmitente;
    property EnderecoEmitente: string read FEnderecoEmitente write FEnderecoEmitente;
    property MunicipioEmitente: string read FMunicipioEmitente write FMunicipioEmitente;
    property MunicipioEmitenteNome: string read FMunicipioEmitenteNome write FMunicipioEmitenteNome;
    property UFEmitente: string read FUFEmitente write FUFEmitente;
    property CEPEmitente: string read FCEPEmitente write FCEPEmitente;
    property TelefoneEmitente: string read FTelefoneEmitente write FTelefoneEmitente;
    property TipoDocDestinatario: Integer read FTipoDocDestinatario write FTipoDocDestinatario;
    property DocDestinatario: string read FDocDestinatario write FDocDestinatario;
    property MunicipioDestinatario: string read FMunicipioDestinatario write FMunicipioDestinatario;
    property MunicipioDestinatarioNome: string read FMunicipioDestinatarioNome write FMunicipioDestinatarioNome;
    property Produto: string read FProduto write FProduto;
    property NumDocOrigem: string read FNumDocOrigem write FNumDocOrigem;
    property Convenio: string read FConvenio write FConvenio;
    property InfoComplementares: string read FInfoComplementares write FInfoComplementares;
    property DataVencimento: string read FDataVencimento write FDataVencimento;
    property DataLimitePagamento: string read FDataLimitePagamento write FDataLimitePagamento;
    property PeriodoReferencia: string read FPeriodoReferencia write FPeriodoReferencia;
    property MesAnoReferencia: string read FMesAnoReferencia write FMesAnoReferencia;
    property Parcela: Integer read FParcela write FParcela;
    property ValorPrincipal: Currency read FValorPrincipal write FValorPrincipal;
    property ValorFCP: Currency read FValorFCP write FValorFCP;
    property AtualizacaoMonetaria: Currency read FAtualizacaoMonetaria write FAtualizacaoMonetaria;
    property AtualizacaoMonetariaFCP: Currency read FAtualizacaoMonetariaFCP write FAtualizacaoMonetariaFCP;
    property Juros: Currency read FJuros write FJuros;
    property JurosFCP: Currency read FJurosFCP write FJurosFCP;
    property Multa: Currency read FMulta write FMulta;
    property MultaFCP: Currency read FMultaFCP write FMultaFCP;
    property RepresentacaoNumerica: string read FRepresentacaoNumerica write FRepresentacaoNumerica;
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    property QtdeVias: Integer read FQtdeVias write FQtdeVias;
    property NumeroControle: string read FNumeroControle write FNumeroControle;
    property IdentificadorGuia: string read FIdentificadorGuia write FIdentificadorGuia;
    property GuiaGeradaContingencia: Integer read FGuiaGeradaContingencia write FGuiaGeradaContingencia;
    property Reservado: string read FReservado write FReservado;
    property InfoCabec: TInfoCabec read FInfoCabec write FInfoCabec;
    // Versao 2.00
    property tipoGnre: string read FtipoGnre write FtipoGnre;
    property ValorICMS: Currency read FValorICMS write FValorICMS;
    property ValorFECP: Currency read FValorFECP write FValorFECP;
    property TotalFECP: Currency read FTotalFECP write FTotalFECP;
    property MultaICMS: Currency read FMultaICMS write FMultaICMS;
    property MultaFECP: Currency read FMultaFECP write FMultaFECP;
    property JurosICMS: Currency read FJurosICMS write FJurosICMS;
    property JurosFECP: Currency read FJurosFECP write FJurosFECP;
    property AtualMonetICMS: Currency read FAtualMonetICMS write FAtualMonetICMS;
    property AtualMonetFECP: Currency read FAtualMonetFECP write FAtualMonetFECP;
    property ValorPrincICMS: Currency read FValorPrincICMS write FValorPrincICMS;
    property valorGNRE: Currency read FvalorGNRE write FvalorGNRE;
    // Versao 2.10
    property qrcodePayload: String read FqrcodePayload write FqrcodePayload;
    property dadosPagamento: TDadosPagamento read FDadosPagamento;
  end;

implementation

{ TGNRERetorno }

constructor TGNRERetorno.Create;
begin
  inherited Create;

  FIdentificador := 0;
  FSequencialGuia := 0;
  FSituacaoGuia := '';
  FUFFavorecida := '';
  FCodReceita := 0;
  FTipoDocEmitente := 0;
  FDocEmitente := '';
  FRazaoSocialEmitente := '';
  FEnderecoEmitente := '';
  FMunicipioEmitente := '';
  FUFEmitente := '';
  FCEPEmitente := '';
  FTelefoneEmitente := '';
  FTipoDocDestinatario := 0;
  FDocDestinatario := '';
  FMunicipioDestinatario := '';
  FProduto := '';
  FNumDocOrigem := '';
  FConvenio := '';
  FInfoComplementares := '';
  FDataVencimento := '';
  FDataLimitePagamento := '';
  FPeriodoReferencia := '';
  FMesAnoReferencia := '';
  FParcela := 0;
  FValorPrincipal := 0;
  FValorFCP := 0;
  FAtualizacaoMonetaria := 0;
  FAtualizacaoMonetariaFCP := 0;
  FJuros := 0;
  FJurosFCP := 0;
  FMulta := 0;
  FMultaFCP := 0;
  FRepresentacaoNumerica := '';
  FCodigoBarras := '';
  FQtdeVias := 0;
  FNumeroControle := '';
  FIdentificadorGuia := '';
  FGuiaGeradaContingencia := 0;
  FReservado := '';
  FvalorGNRE := 0;
  FInfoCabec := TInfoCabec.Create;
  FqrcodePayload := '';
  FDadosPagamento := TDadosPagamento.Create;
end;

destructor TGNRERetorno.Destroy;
begin
  FInfoCabec.Free;
  FDadosPagamento.Free;

  inherited Destroy;
end;

end.
