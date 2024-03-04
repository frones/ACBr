{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Giovane Preis                          }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{******************************************************************************}

{$I ACBr.inc}

unit pcnDIR;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnLeitor, pcnDI;

type
  TDIR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FDI: TDI;
  public
    constructor Create(AOwner: TDI);
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property DI: TDI         read FDI     write FDI;
  end;

implementation

uses
  pcnConversaoNFe,
  ACBrUtil.Base;

{ TNFeR }

constructor TDIR.Create(AOwner: TDI);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FDI     := AOwner;
end;

destructor TDIR.Destroy;
begin
  FLeitor.Free;
  inherited Destroy;
end;

function TDIR.LerXml: Boolean;
var
  i, ii: Integer;
  oAdicao: TadicaoCollectionItem;
begin
  Leitor.FloatIsIntString := True;

  i := 0;
  while (Leitor.rExtrai(1, 'adicao', '', i+1) <> '') do
  begin
    Leitor.GroupStore;
    oAdicao := DI.adicao.New;

    ii := 0;
    oAdicao.acrescimo.Clear;
    while (Leitor.rExtrai(2, 'acrescimo', '', ii+1) <> '') do
    begin
      oAdicao.acrescimo.New;
      oAdicao.acrescimo[ii].codigoAcrescimo      := Leitor.rCampo(tcInt, 'codigoAcrescimo');
      oAdicao.acrescimo[ii].denominacao          := Leitor.rCampo(tcStr, 'denominacao');
      oAdicao.acrescimo[ii].moedaNegociadaCodigo := Leitor.rCampo(tcInt, 'moedaNegociadaCodigo');
      oAdicao.acrescimo[ii].moedaNegociadaNome   := Leitor.rCampo(tcStr, 'moedaNegociadaNome');
      oAdicao.acrescimo[ii].valorMoedaNegociada  := Leitor.rCampo(tcDe2, 'valorMoedaNegociada');
      oAdicao.acrescimo[ii].valorReais           := Leitor.rCampo(tcDe2, 'valorReais');
      Inc(ii);
    end;
    Leitor.GroupRestore(False);

    oAdicao.cideValorAliquotaEspecifica                  := Leitor.rCampo(tcDe2, 'cideValorAliquotaEspecifica');
    oAdicao.cideValorDevido                              := Leitor.rCampo(tcDe2, 'cideValorDevido');
    oAdicao.cideValorRecolher                            := Leitor.rCampo(tcDe2, 'cideValorRecolher');
    oAdicao.codigoRelacaoCompradorVendedor               := Leitor.rCampo(tcInt, 'codigoRelacaoCompradorVendedor');
    oAdicao.codigoVinculoCompradorVendedor               := Leitor.rCampo(tcInt, 'codigoVinculoCompradorVendedor');
    oAdicao.cofinsAliquotaAdValorem                      := Leitor.rCampo(tcDe2, 'cofinsAliquotaAdValorem');
    oAdicao.cofinsAliquotaEspecificaQuantidadeUnidade    := Leitor.rCampo(tcDe2, 'cofinsAliquotaEspecificaQuantidadeUnidade');
    oAdicao.cofinsAliquotaEspecificaValor                := Leitor.rCampo(tcDe2, 'cofinsAliquotaEspecificaValor');
    oAdicao.cofinsAliquotaReduzida                       := Leitor.rCampo(tcDe2, 'cofinsAliquotaReduzida');
    oAdicao.cofinsAliquotaValorDevido                    := Leitor.rCampo(tcDe2, 'cofinsAliquotaValorDevido');
    oAdicao.cofinsAliquotaValorRecolher                  := Leitor.rCampo(tcDe2, 'cofinsAliquotaValorRecolher');
    oAdicao.condicaoVendaIncoterm                        := Leitor.rCampo(tcStr, 'condicaoVendaIncoterm');
    oAdicao.condicaoVendaLocal                           := Leitor.rCampo(tcStr, 'condicaoVendaLocal');
    oAdicao.condicaoVendaMetodoValoracaoCodigo           := Leitor.rCampo(tcInt, 'condicaoVendaMetodoValoracaoCodigo');
    oAdicao.condicaoVendaMetodoValoracaoNome             := Leitor.rCampo(tcStr, 'condicaoVendaMetodoValoracaoNome');
    oAdicao.condicaoVendaMoedaCodigo                     := Leitor.rCampo(tcInt, 'condicaoVendaMoedaCodigo');
    oAdicao.condicaoVendaMoedaNome                       := Leitor.rCampo(tcStr, 'condicaoVendaMoedaNome');
    oAdicao.condicaoVendaValorMoeda                      := Leitor.rCampo(tcDe2, 'condicaoVendaValorMoeda');
    oAdicao.condicaoVendaValorReais                      := Leitor.rCampo(tcDe2, 'condicaoVendaValorReais');
    oAdicao.dadosCambiaisCoberturaCambialCodigo          := Leitor.rCampo(tcInt, 'dadosCambiaisCoberturaCambialCodigo');
    oAdicao.dadosCambiaisCoberturaCambialNome            := Leitor.rCampo(tcStr, 'dadosCambiaisCoberturaCambialNome');
    oAdicao.dadosCambiaisInstituicaoFinanciadoraCodigo   := Leitor.rCampo(tcInt, 'dadosCambiaisInstituicaoFinanciadoraCodigo');
    oAdicao.dadosCambiaisInstituicaoFinanciadoraNome     := Leitor.rCampo(tcStr, 'dadosCambiaisInstituicaoFinanciadoraNome');
    oAdicao.dadosCambiaisMotivoSemCoberturaCodigo        := Leitor.rCampo(tcInt, 'dadosCambiaisMotivoSemCoberturaCodigo');
    oAdicao.dadosCambiaisMotivoSemCoberturaNome          := Leitor.rCampo(tcStr, 'dadosCambiaisMotivoSemCoberturaNome');
    oAdicao.dadosCambiaisValorRealCambio                 := Leitor.rCampo(tcDe2, 'dadosCambiaisValorRealCambio');
    oAdicao.dadosCargaPaisProcedenciaCodigo              := Leitor.rCampo(tcInt, 'dadosCargaPaisProcedenciaCodigo');
    oAdicao.dadosCargaUrfEntradaCodigo                   := Leitor.rCampo(tcInt, 'dadosCargaUrfEntradaCodigo');
    oAdicao.dadosCargaViaTransporteCodigo                := Leitor.rCampo(tcInt, 'dadosCargaViaTransporteCodigo');
    oAdicao.dadosMercadoriaAplicacao                     := Leitor.rCampo(tcStr, 'dadosMercadoriaAplicacao');
    oAdicao.dadosMercadoriaCodigoNaladiNCCA              := Leitor.rCampo(tcInt, 'dadosMercadoriaCodigoNaladiNCCA');
    oAdicao.dadosMercadoriaCodigoNaladiSH                := Leitor.rCampo(tcInt, 'dadosMercadoriaCodigoNaladiSH');
    oAdicao.dadosMercadoriaCodigoNcm                     := Leitor.rCampo(tcStr, 'dadosMercadoriaCodigoNcm');
    oAdicao.dadosMercadoriaCondicao                      := Leitor.rCampo(tcStr, 'dadosMercadoriaCondicao');
    oAdicao.dadosMercadoriaMedidaEstatisticaQuantidade   := Leitor.rCampo(tcDe2, 'dadosMercadoriaMedidaEstatisticaQuantidade');
    oAdicao.dadosMercadoriaMedidaEstatisticaUnidade      := Leitor.rCampo(tcStr, 'dadosMercadoriaMedidaEstatisticaUnidade');
    oAdicao.dadosMercadoriaNomeNcm                       := Leitor.rCampo(tcStr, 'dadosMercadoriaNomeNcm');
    oAdicao.dadosMercadoriaPesoLiquido                   := Leitor.rCampo(tcDe5, 'dadosMercadoriaPesoLiquido');
    oAdicao.dcrCoeficienteReducao                        := Leitor.rCampo(tcInt, 'dcrCoeficienteReducao');
    oAdicao.dcrIdentificacao                             := Leitor.rCampo(tcInt, 'dcrIdentificacao');
    oAdicao.dcrValorDevido                               := Leitor.rCampo(tcDe2, 'dcrValorDevido');
    oAdicao.dcrValorDolar                                := Leitor.rCampo(tcDe2, 'dcrValorDolar');
    oAdicao.dcrValorReal                                 := Leitor.rCampo(tcDe2, 'dcrValorReal');
    oAdicao.dcrValorRecolher                             := Leitor.rCampo(tcDe2, 'dcrValorRecolher');

    ii := 0;
    oAdicao.deducao.Clear;
    while (Leitor.rExtrai(2, 'deducao', '', ii+1) <> '') do
    begin
      oAdicao.deducao.New;
      oAdicao.deducao[ii].codigoDeducao        := Leitor.rCampo(tcInt, 'codigoDeducao');
      oAdicao.deducao[ii].denominacao          := Leitor.rCampo(tcStr, 'denominacao');
      oAdicao.deducao[ii].moedaNegociadaCodigo := Leitor.rCampo(tcInt, 'moedaNegociadaCodigo');
      oAdicao.deducao[ii].moedaNegociadaNome   := Leitor.rCampo(tcStr, 'moedaNegociadaNome');
      oAdicao.deducao[ii].valorMoedaNegociada  := Leitor.rCampo(tcDe2, 'valorMoedaNegociada');
      oAdicao.deducao[ii].valorReais           := Leitor.rCampo(tcDe2, 'valorReais');
      Inc(ii);
    end;
    Leitor.GroupRestore(False);

    if (Leitor.rExtrai(2, 'destaqueNcm') <> '') then
      oAdicao.destaqueNcm.numeroDestaque := Leitor.rCampo(tcInt, 'numeroDestaque');

    Leitor.GroupRestore(False);

    ii := 0;
    oAdicao.documentoVinculado.Clear;
    while (Leitor.rExtrai(2, 'documentoVinculado', '', ii+1) <> '') do
    begin
      oAdicao.documentoVinculado.New;
      oAdicao.documentoVinculado[ii].codigoTipo := Leitor.rCampo(tcInt, 'codigoTipo');
      oAdicao.documentoVinculado[ii].nomeTipo   := Leitor.rCampo(tcStr, 'nomeTipo');
      oAdicao.documentoVinculado[ii].numero     := Leitor.rCampo(tcStr, 'numero');
      Inc(ii);
    end;
    Leitor.GroupRestore(False);

    oAdicao.fabricanteCidade                             := Leitor.rCampo(tcStr, 'fabricanteCidade');
    oAdicao.fabricanteComplemento                        := Leitor.rCampo(tcStr, 'fabricanteComplemento');
    oAdicao.fabricanteEstado                             := Leitor.rCampo(tcStr, 'fabricanteEstado');
    oAdicao.fabricanteLogradouro                         := Leitor.rCampo(tcStr, 'fabricanteLogradouro');
    oAdicao.fabricanteNome                               := Leitor.rCampo(tcStr, 'fabricanteNome');
    oAdicao.fabricanteNumero                             := Leitor.rCampo(tcInt, 'fabricanteNumero');
    oAdicao.fornecedorCidade                             := Leitor.rCampo(tcStr, 'fornecedorCidade');
    oAdicao.fornecedorComplemento                        := Leitor.rCampo(tcStr, 'fornecedorComplemento');
    oAdicao.fornecedorEstado                             := Leitor.rCampo(tcStr, 'fornecedorEstado');
    oAdicao.fornecedorLogradouro                         := Leitor.rCampo(tcStr, 'fornecedorLogradouro');
    oAdicao.fornecedorNome                               := Leitor.rCampo(tcStr, 'fornecedorNome');
    oAdicao.fornecedorNumero                             := Leitor.rCampo(tcInt, 'fornecedorNumero');
    oAdicao.freteMoedaNegociadaCodigo                    := Leitor.rCampo(tcInt, 'freteMoedaNegociadaCodigo');
    oAdicao.freteValorMoedaNegociada                     := Leitor.rCampo(tcDe2, 'freteValorMoedaNegociada');
    oAdicao.freteValorReais                              := Leitor.rCampo(tcDe2, 'freteValorReais');
    oAdicao.iiAcordoTarifarioAladiCodigo                 := Leitor.rCampo(tcInt, 'iiAcordoTarifarioAladiCodigo');
    oAdicao.iiAcordoTarifarioAladiNome                   := Leitor.rCampo(tcStr, 'iiAcordoTarifarioAladiNome');
    oAdicao.iiAcordoTarifarioAtoLegalAno                 := Leitor.rCampo(tcInt, 'iiAcordoTarifarioAtoLegalAno');
    oAdicao.iiAcordoTarifarioAtoLegalCodigo              := Leitor.rCampo(tcStr, 'iiAcordoTarifarioAtoLegalCodigo');
    oAdicao.iiAcordoTarifarioAtoLegalEX                  := Leitor.rCampo(tcInt, 'iiAcordoTarifarioAtoLegalEX');
    oAdicao.iiAcordoTarifarioAtoLegalNumero              := Leitor.rCampo(tcInt, 'iiAcordoTarifarioAtoLegalNumero');
    oAdicao.iiAcordoTarifarioAtoLegalOrgaoEmissor        := Leitor.rCampo(tcStr, 'iiAcordoTarifarioAtoLegalOrgaoEmissor');
    oAdicao.iiAcordoTarifarioTipoCodigo                  := Leitor.rCampo(tcInt, 'iiAcordoTarifarioTipoCodigo');
    oAdicao.iiAcordoTarifarioTipoNome                    := Leitor.rCampo(tcStr, 'iiAcordoTarifarioTipoNome');
    oAdicao.iiAliquotaAcordo                             := Leitor.rCampo(tcInt, 'iiAliquotaAcordo');
    oAdicao.iiAliquotaAdValorem                          := Leitor.rCampo(tcDe2, 'iiAliquotaAdValorem');
    oAdicao.iiAliquotaPercentualReducao                  := Leitor.rCampo(tcDe2, 'iiAliquotaPercentualReducao');
    oAdicao.iiAliquotaReduzida                           := Leitor.rCampo(tcDe2, 'iiAliquotaReduzida');
    oAdicao.iiAliquotaValorCalculado                     := Leitor.rCampo(tcDe2, 'iiAliquotaValorCalculado');
    oAdicao.iiAliquotaValorDevido                        := Leitor.rCampo(tcDe2, 'iiAliquotaValorDevido');
    oAdicao.iiAliquotaValorRecolher                      := Leitor.rCampo(tcDe2, 'iiAliquotaValorRecolher');
    oAdicao.iiAliquotaValorReduzido                      := Leitor.rCampo(tcDe2, 'iiAliquotaValorReduzido');
    oAdicao.iiBaseCalculo                                := Leitor.rCampo(tcDe2, 'iiBaseCalculo');
    oAdicao.iiFundamentoLegalCodigo                      := Leitor.rCampo(tcInt, 'iiFundamentoLegalCodigo');
    oAdicao.iiMotivoAdmissaoTemporariaCodigo             := Leitor.rCampo(tcInt, 'iiMotivoAdmissaoTemporariaCodigo');
    oAdicao.iiRegimeTributacaoCodigo                     := Leitor.rCampo(tcInt, 'iiRegimeTributacaoCodigo');
    oAdicao.iiRegimeTributacaoNome                       := Leitor.rCampo(tcStr, 'iiRegimeTributacaoNome');
    oAdicao.ipiAliquotaAdValorem                         := Leitor.rCampo(tcDe2, 'ipiAliquotaAdValorem');
    oAdicao.ipiAliquotaEspecificaCapacidadeRecipciente   := Leitor.rCampo(tcDe2, 'ipiAliquotaEspecificaCapacidadeRecipciente');
    oAdicao.ipiAliquotaEspecificaQuantidadeUnidadeMedida := Leitor.rCampo(tcDe2, 'ipiAliquotaEspecificaQuantidadeUnidadeMedida');
    oAdicao.ipiAliquotaEspecificaTipoRecipienteCodigo    := Leitor.rCampo(tcDe2, 'ipiAliquotaEspecificaTipoRecipienteCodigo');
    oAdicao.ipiAliquotaEspecificaValorUnidadeMedida      := Leitor.rCampo(tcDe2, 'ipiAliquotaEspecificaValorUnidadeMedida');
    oAdicao.ipiAliquotaNotaComplementarTIPI              := Leitor.rCampo(tcDe2, 'ipiAliquotaNotaComplementarTIPI');
    oAdicao.ipiAliquotaReduzida                          := Leitor.rCampo(tcDe2, 'ipiAliquotaReduzida');
    oAdicao.ipiAliquotaValorDevido                       := Leitor.rCampo(tcDe2, 'ipiAliquotaValorDevido');
    oAdicao.ipiAliquotaValorRecolher                     := Leitor.rCampo(tcDe2, 'ipiAliquotaValorRecolher');
    oAdicao.ipiRegimeTributacaoCodigo                    := Leitor.rCampo(tcInt, 'ipiRegimeTributacaoCodigo');
    oAdicao.ipiRegimeTributacaoNome                      := Leitor.rCampo(tcStr, 'ipiRegimeTributacaoNome');

    ii := 0;
    oAdicao.mercadoria.Clear;
    while (Leitor.rExtrai(2, 'mercadoria', '', ii+1) <> '') do
    begin
      oAdicao.mercadoria.New;
      oAdicao.mercadoria[ii].descricaoMercadoria  := Leitor.rCampo(tcStr, 'descricaoMercadoria');
      oAdicao.mercadoria[ii].numeroSequencialItem := Leitor.rCampo(tcInt, 'numeroSequencialItem');
      oAdicao.mercadoria[ii].quantidade           := Leitor.rCampo(tcDe5, 'quantidade');
      oAdicao.mercadoria[ii].unidadeMedida        := Leitor.rCampo(tcStr, 'unidadeMedida');
      oAdicao.mercadoria[ii].valorUnitario        := Leitor.rCampo(tcDe7, 'valorUnitario');
      Inc(ii);
    end;
    Leitor.GroupRestore(False);

    ii := 0;
    oAdicao.nomenclaturaValorAduaneiro.Clear;
    while (Leitor.rExtrai(2, 'nomenclaturaValorAduaneiro', '', ii+1) <> '') do
    begin
      oAdicao.nomenclaturaValorAduaneiro.New;
      oAdicao.nomenclaturaValorAduaneiro[ii].atributo      := Leitor.rCampo(tcStr, 'atributo');
      oAdicao.nomenclaturaValorAduaneiro[ii].especificacao := Leitor.rCampo(tcInt, 'especificacao');
      oAdicao.nomenclaturaValorAduaneiro[ii].nivelNome     := Leitor.rCampo(tcStr, 'nivelNome');
      Inc(ii);
    end;
    Leitor.GroupRestore(False);

    oAdicao.numeroAdicao                                := Leitor.rCampo(tcInt, 'numeroAdicao');
    oAdicao.numeroDI                                    := Leitor.rCampo(tcStr, 'numeroDI');
    oAdicao.numeroLI                                    := Leitor.rCampo(tcStr, 'numeroLI');
    oAdicao.paisAquisicaoMercadoriaCodigo               := Leitor.rCampo(tcInt, 'paisAquisicaoMercadoriaCodigo');
    oAdicao.paisAquisicaoMercadoriaNome                 := Leitor.rCampo(tcStr, 'paisAquisicaoMercadoriaNome');
    oAdicao.paisOrigemMercadoriaCodigo                  := Leitor.rCampo(tcInt, 'paisOrigemMercadoriaCodigo');
    oAdicao.paisOrigemMercadoriaNome                    := Leitor.rCampo(tcStr, 'paisOrigemMercadoriaNome');
    oAdicao.pisCofinsBaseCalculoAliquotaICMS            := Leitor.rCampo(tcDe2, 'pisCofinsBaseCalculoAliquotaICMS');
    oAdicao.pisCofinsBaseCalculoFundamentoLegalCodigo   := Leitor.rCampo(tcInt, 'pisCofinsBaseCalculoFundamentoLegalCodigo');
    oAdicao.pisCofinsBaseCalculoPercentualReducao       := Leitor.rCampo(tcDe2, 'pisCofinsBaseCalculoPercentualReducao');
    oAdicao.pisCofinsBaseCalculoValor                   := Leitor.rCampo(tcDe2, 'pisCofinsBaseCalculoValor');
    oAdicao.pisCofinsFundamentoLegalReducaoCodigo       := Leitor.rCampo(tcInt, 'pisCofinsFundamentoLegalReducaoCodigo');
    oAdicao.pisCofinsRegimeTributacaoCodigo             := Leitor.rCampo(tcInt, 'pisCofinsRegimeTributacaoCodigo');
    oAdicao.pisCofinsRegimeTributacaoNome               := Leitor.rCampo(tcStr, 'pisCofinsRegimeTributacaoNome');
    oAdicao.pisPasepAliquotaAdValorem                   := Leitor.rCampo(tcDe2, 'pisPasepAliquotaAdValorem');
    oAdicao.pisPasepAliquotaEspecificaQuantidadeUnidade := Leitor.rCampo(tcDe2, 'pisPasepAliquotaEspecificaQuantidadeUnidade');
    oAdicao.pisPasepAliquotaEspecificaValor             := Leitor.rCampo(tcDe2, 'pisPasepAliquotaEspecificaValor');
    oAdicao.pisPasepAliquotaReduzida                    := Leitor.rCampo(tcDe2, 'pisPasepAliquotaReduzida');
    oAdicao.pisPasepAliquotaValorDevido                 := Leitor.rCampo(tcDe2, 'pisPasepAliquotaValorDevido');
    oAdicao.pisPasepAliquotaValorRecolher               := Leitor.rCampo(tcDe2, 'pisPasepAliquotaValorRecolher');
    oAdicao.relacaoCompradorVendedor                    := Leitor.rCampo(tcStr, 'relacaoCompradorVendedor');
    oAdicao.seguroMoedaNegociadaCodigo                  := Leitor.rCampo(tcDe2, 'seguroMoedaNegociadaCodigo');
    oAdicao.seguroValorMoedaNegociada                   := Leitor.rCampo(tcDe2, 'seguroValorMoedaNegociada');
    oAdicao.seguroValorReais                            := Leitor.rCampo(tcDe2, 'seguroValorReais');
    oAdicao.sequencialRetificacao                       := Leitor.rCampo(tcInt, 'sequencialRetificacao');
    oAdicao.valorMultaARecolher                         := Leitor.rCampo(tcDe2, 'valorMultaARecolher');
    oAdicao.valorMultaARecolherAjustado                 := Leitor.rCampo(tcDe2, 'valorMultaARecolherAjustado');
    oAdicao.valorReaisFreteInternacional                := Leitor.rCampo(tcDe2, 'valorReaisFreteInternacional');
    oAdicao.valorReaisSeguroInternacional               := Leitor.rCampo(tcDe2, 'valorReaisSeguroInternacional');
    oAdicao.valorTotalCondicaoVenda                     := Leitor.rCampo(tcDe2, 'valorTotalCondicaoVenda');
    oAdicao.vinculoCompradorVendedor                    := Leitor.rCampo(tcStr, 'vinculoCompradorVendedor');
    Inc(i);
    Leitor.GroupRestore;
  end;

  if (Leitor.rExtrai(1, 'armazem') <> '') then
    DI.armazem.nomeArmazem := Leitor.rCampo(tcStr, 'nomeArmazem');

  Leitor.GroupRestore;

  DI.armazenamentoRecintoAduaneiroCodigo := Leitor.rCampo(tcInt, 'armazenamentoRecintoAduaneiroCodigo');
  DI.armazenamentoRecintoAduaneiroNome   := Leitor.rCampo(tcStr, 'armazenamentoRecintoAduaneiroNome');
  DI.armazenamentoSetor                  := Leitor.rCampo(tcInt, 'armazenamentoSetor');
  DI.canalSelecaoParametrizada           := Leitor.rCampo(tcInt, 'canalSelecaoParametrizada');
  DI.caracterizacaoOperacaoCodigoTipo    := Leitor.rCampo(tcInt, 'caracterizacaoOperacaoCodigoTipo');
  DI.caracterizacaoOperacaoDescricaoTipo := Leitor.rCampo(tcStr, 'caracterizacaoOperacaoDescricaoTipo');
  DI.caracterizacaoOperacaoNome          := Leitor.rCampo(tcStr, 'caracterizacaoOperacaoNome');
  DI.caracterizacaoOperacaoNumero        := Leitor.rCampo(tcStr, 'caracterizacaoOperacaoNumero');
  DI.cargaDataChegada                    := Leitor.rCampo(tcDatCFe, 'cargaDataChegada');
  DI.cargaNumeroAgente                   := Leitor.rCampo(tcStr, 'cargaNumeroAgente');
  DI.cargaPaisProcedenciaCodigo          := Leitor.rCampo(tcInt, 'cargaPaisProcedenciaCodigo');
  DI.cargaPaisProcedenciaNome            := Leitor.rCampo(tcStr, 'cargaPaisProcedenciaNome');
  DI.cargaPesoBruto                      := Leitor.rCampo(tcDe5, 'cargaPesoBruto');
  DI.cargaPesoLiquido                    := Leitor.rCampo(tcDe5, 'cargaPesoLiquido');
  DI.cargaUrfEntradaCodigo               := Leitor.rCampo(tcInt, 'cargaUrfEntradaCodigo');
  DI.cargaUrfEntradaNome                 := Leitor.rCampo(tcStr, 'cargaUrfEntradaNome');
  DI.conhecimentoCargaEmbarqueData       := Leitor.rCampo(tcDatCFe, 'conhecimentoCargaEmbarqueData');
  DI.conhecimentoCargaEmbarqueLocal      := Leitor.rCampo(tcStr, 'conhecimentoCargaEmbarqueLocal');
  DI.conhecimentoCargaId                 := Leitor.rCampo(tcStr, 'conhecimentoCargaId');
  DI.conhecimentoCargaTipoCodigo         := Leitor.rCampo(tcInt, 'conhecimentoCargaTipoCodigo');
  DI.conhecimentoCargaIdMaster           := Leitor.rCampo(tcStr, 'conhecimentoCargaIdMaster');
  DI.conhecimentoCargaTipoNome           := Leitor.rCampo(tcStr, 'conhecimentoCargaTipoNome');
  DI.conhecimentoCargaUtilizacao         := Leitor.rCampo(tcInt, 'conhecimentoCargaUtilizacao');
  DI.conhecimentoCargaUtilizacaoNome     := Leitor.rCampo(tcStr, 'conhecimentoCargaUtilizacaoNome');
  DI.dataDesembaraco                     := Leitor.rCampo(tcDatCFe, 'dataDesembaraco');
  DI.dataRegistro                        := Leitor.rCampo(tcDatCFe, 'dataRegistro');

  if (Leitor.rExtrai(1, 'declaracaoEe') <> '') then
  begin
    DI.declaracaoEe.faixaFinal                  := Leitor.rCampo(tcInt, 'faixaFinal');
    DI.declaracaoEe.faixaInicial                := Leitor.rCampo(tcInt, 'faixaInicial');
    DI.declaracaoEe.numeroDeclaracaoEstrangeira := Leitor.rCampo(tcStr, 'numeroDeclaracaoEstrangeira');
  end;
  Leitor.GroupRestore;

  DI.documentoChegadaCargaCodigoTipo := Leitor.rCampo(tcInt, 'documentoChegadaCargaCodigoTipo');
  DI.documentoChegadaCargaNome       := Leitor.rCampo(tcStr, 'documentoChegadaCargaNome');
  DI.documentoChegadaCargaNumero     := Leitor.rCampo(tcStr, 'documentoChegadaCargaNumero');

  i := 0;
  DI.documentoInstrucaoDespacho.Clear;
  while (Leitor.rExtrai(1, 'documentoInstrucaoDespacho', '', i+1) <> '') do
  begin
    DI.documentoInstrucaoDespacho.New;
    DI.documentoInstrucaoDespacho[i].codigoTipoDocumentoDespacho := Leitor.rCampo(tcInt, 'codigoTipoDocumentoDespacho');
    DI.documentoInstrucaoDespacho[i].nomeDocumentoDespacho       := Leitor.rCampo(tcStr, 'nomeDocumentoDespacho');
    DI.documentoInstrucaoDespacho[i].numeroDocumentoDespacho     := Leitor.rCampo(tcStr, 'numeroDocumentoDespacho');
    Inc(i);
  end;
  Leitor.GroupRestore;

  i := 0;
  DI.dossie.Clear;
  while (Leitor.rExtrai(1, 'dossie', '', i+1) <> '') do
  begin
    DI.dossie.New;
    DI.dossie[i].dossieDataVinculacao := Leitor.rCampo(tcStr,    'dossieDataVinculacao');
    DI.dossie[i].dossieHoraVinculacao := Leitor.rCampo(tcHorCFe, 'dossieHoraVinculacao');
    DI.dossie[i].dossieNumero         := Leitor.rCampo(tcStr,    'dossieNumero');
    DI.dossie[i].indicadorDossieVazio := Leitor.rCampo(tcStr,    'indicadorDossieVazio');
    Inc(i);
  end;
  Leitor.GroupRestore;

  if (Leitor.rExtrai(1, 'embalagem') <> '') then
  begin
    DI.embalagem.codigoTipoEmbalagem  := Leitor.rCampo(tcInt, 'codigoTipoEmbalagem');
    DI.embalagem.nomeEmbalagem        := Leitor.rCampo(tcStr, 'nomeEmbalagem');
    DI.embalagem.moedaNegociadaCodigo := Leitor.rCampo(tcInt, 'moedaNegociadaCodigo');
    DI.embalagem.quantidadeVolume     := Leitor.rCampo(tcInt, 'quantidadeVolume');
  end;
  Leitor.GroupRestore;

  DI.freteCollect              := Leitor.rCampo(tcDe2, 'freteCollect');
  DI.freteEmTerritorioNacional := Leitor.rCampo(tcDe2, 'freteEmTerritorioNacional');
  DI.freteMoedaNegociadaCodigo := Leitor.rCampo(tcInt, 'freteMoedaNegociadaCodigo');
  DI.freteMoedaNegociadaNome   := Leitor.rCampo(tcStr, 'freteMoedaNegociadaNome');
  DI.fretePrepaid              := Leitor.rCampo(tcDe2, 'fretePrepaid');
  DI.freteTotalDolares         := Leitor.rCampo(tcDe2, 'freteTotalDolares');
  DI.freteTotalMoeda           := Leitor.rCampo(tcDe2, 'freteTotalMoeda');
  DI.freteTotalReais           := Leitor.rCampo(tcDe2, 'freteTotalReais');

  i := 0;
  DI.dossie.Clear;
  while (Leitor.rExtrai(1, 'icms', '', i+1) <> '') do
  begin
    DI.icms.New;
    DI.icms[i].agenciaIcms                := Leitor.rCampo(tcInt, 'agenciaIcms');
    DI.icms[i].bancoIcms                  := Leitor.rCampo(tcInt, 'bancoIcms');
    DI.icms[i].codigoTipoRecolhimentoIcms := Leitor.rCampo(tcInt, 'codigoTipoRecolhimentoIcms');
    DI.icms[i].cpfResponsavelRegistro     := Leitor.rCampo(tcStr, 'cpfResponsavelRegistro');
    DI.icms[i].dataPagamentoIcms          := Leitor.rCampo(tcDatCFe, 'dataPagamentoIcms');
    DI.icms[i].dataRegistro               := Leitor.rCampo(tcDatCFe, 'dataRegistro');
    DI.icms[i].horaRegistro               := Leitor.rCampo(tcHorCFe, 'horaRegistro');
    DI.icms[i].nomeTipoRecolhimentoIcms   := Leitor.rCampo(tcStr, 'nomeTipoRecolhimentoIcms');
    DI.icms[i].numeroSequencialIcms       := Leitor.rCampo(tcInt, 'numeroSequencialIcms');
    DI.icms[i].ufIcms                     := Leitor.rCampo(tcStr, 'ufIcms');
    DI.icms[i].valorTotalIcms             := Leitor.rCampo(tcDe2, 'valorTotalIcms');
    Inc(i);
  end;
  Leitor.GroupRestore;

  DI.importadorCodigoTipo             := Leitor.rCampo(tcInt, 'importadorCodigoTipo');
  DI.importadorCpfRepresentanteLegal  := Leitor.rCampo(tcStr, 'importadorCpfRepresentanteLegal');
  DI.importadorEnderecoBairro         := Leitor.rCampo(tcStr, 'importadorEnderecoBairro');
  DI.importadorEnderecoCep            := Leitor.rCampo(tcStr, 'importadorEnderecoCep');
  DI.importadorEnderecoComplemento    := Leitor.rCampo(tcStr, 'importadorEnderecoComplemento');
  DI.importadorEnderecoLogradouro     := Leitor.rCampo(tcStr, 'importadorEnderecoLogradouro');
  DI.importadorEnderecoMunicipio      := Leitor.rCampo(tcStr, 'importadorEnderecoMunicipio');
  DI.importadorEnderecoNumero         := Leitor.rCampo(tcStr, 'importadorEnderecoNumero');
  DI.importadorEnderecoUf             := Leitor.rCampo(tcStr, 'importadorEnderecoUf');
  DI.importadorNome                   := Leitor.rCampo(tcStr, 'importadorNome');
  DI.importadorNomeRepresentanteLegal := Leitor.rCampo(tcStr, 'importadorNomeRepresentanteLegal');
  DI.importadorNumero                 := Leitor.rCampo(tcStr, 'importadorNumero');
  DI.importadorNumeroTelefone         := Leitor.rCampo(tcStr, 'importadorNumeroTelefone');
  DI.informacaoComplementar           := Leitor.rCampo(tcStr, 'informacaoComplementar');
  DI.localDescargaTotalDolares        := Leitor.rCampo(tcDe2, 'localDescargaTotalDolares');
  DI.localDescargaTotalReais          := Leitor.rCampo(tcDe2, 'localDescargaTotalReais');
  DI.localEmbarqueTotalDolares        := Leitor.rCampo(tcDe2, 'localEmbarqueTotalDolares');
  DI.localEmbarqueTotalReais          := Leitor.rCampo(tcDe2, 'localEmbarqueTotalReais');
  DI.modalidadeDespachoCodigo         := Leitor.rCampo(tcInt, 'modalidadeDespachoCodigo');
  DI.modalidadeDespachoNome           := Leitor.rCampo(tcStr, 'modalidadeDespachoNome');
  DI.numeroDI                         := Leitor.rCampo(tcStr, 'numeroDI');
  DI.operacaoFundap                   := Leitor.rCampo(tcStr, 'operacaoFundap');

  i := 0;
  DI.pagamento.Clear;
  while (Leitor.rExtrai(1, 'pagamento', '', i+1) <> '') do
  begin
    DI.pagamento.New;
    DI.pagamento[i].agenciaPagamento    := Leitor.rCampo(tcInt, 'agenciaPagamento');
    DI.pagamento[i].bancoPagamento      := Leitor.rCampo(tcInt, 'bancoPagamento');
    DI.pagamento[i].codigoReceita       := Leitor.rCampo(tcInt, 'codigoReceita');
    DI.pagamento[i].codigoTipoPagamento := Leitor.rCampo(tcInt, 'codigoTipoPagamento');
    DI.pagamento[i].contaPagamento      := Leitor.rCampo(tcStr, 'contaPagamento');
    DI.pagamento[i].dataPagamento       := Leitor.rCampo(tcDatCFe, 'dataPagamento');
    DI.pagamento[i].nomeTipoPagamento   := Leitor.rCampo(tcStr, 'nomeTipoPagamento');
    DI.pagamento[i].numeroRetificacao   := Leitor.rCampo(tcInt, 'numeroRetificacao');
    DI.pagamento[i].valorJurosEncargos  := Leitor.rCampo(tcDe2, 'valorJurosEncargos');
    DI.pagamento[i].valorMulta          := Leitor.rCampo(tcDe2, 'valorMulta');
    DI.pagamento[i].valorReceita        := Leitor.rCampo(tcDe2, 'valorReceita');
    Inc(i);
  end;
  Leitor.GroupRestore;

  DI.seguroMoedaNegociadaCodigo           := Leitor.rCampo(tcInt, 'seguroMoedaNegociadaCodigo');
  DI.seguroMoedaNegociadaNome             := Leitor.rCampo(tcInt, 'seguroMoedaNegociadaNome');
  DI.seguroTotalDolares                   := Leitor.rCampo(tcDe2, 'seguroTotalDolares');
  DI.seguroTotalMoedaNegociada            := Leitor.rCampo(tcDe2, 'seguroTotalMoedaNegociada');
  DI.seguroTotalReais                     := Leitor.rCampo(tcDe2, 'seguroTotalReais');
  DI.sequencialRetificacao                := Leitor.rCampo(tcInt, 'sequencialRetificacao');
  DI.situacaoEntregaCarga                 := Leitor.rCampo(tcStr, 'situacaoEntregaCarga');
  DI.tipoDeclaracaoCodigo                 := Leitor.rCampo(tcInt, 'tipoDeclaracaoCodigo');
  DI.tipoDeclaracaoNome                   := Leitor.rCampo(tcStr, 'tipoDeclaracaoNome');
  DI.totalAdicoes                         := Leitor.rCampo(tcInt, 'totalAdicoes');
  DI.urfDespachoCodigo                    := Leitor.rCampo(tcInt, 'urfDespachoCodigo');
  DI.urfDespachoNome                      := Leitor.rCampo(tcStr, 'urfDespachoNome');
  DI.valorTotalMultaARecolher             := Leitor.rCampo(tcDe2, 'valorTotalMultaARecolher');
  DI.valorTotalMultaARecolherAjustado     := Leitor.rCampo(tcDe2, 'valorTotalMultaARecolherAjustado');
  DI.viaTransporteCodigo                  := Leitor.rCampo(tcInt, 'viaTransporteCodigo');
  DI.viaTransporteMultimodal              := Leitor.rCampo(tcStr, 'viaTransporteMultimodal');
  DI.viaTransporteNome                    := Leitor.rCampo(tcStr, 'viaTransporteNome');
  DI.viaTransporteNomeTransportador       := Leitor.rCampo(tcStr, 'viaTransporteNomeTransportador');
  DI.viaTransporteNomeVeiculo             := Leitor.rCampo(tcStr, 'viaTransporteNomeVeiculo');
  DI.viaTransporteNumeroVeiculo           := Leitor.rCampo(tcStr, 'viaTransporteNumeroVeiculo');
  DI.viaTransportePaisTransportadorCodigo := Leitor.rCampo(tcInt, 'viaTransportePaisTransportadorCodigo');
  DI.viaTransportePaisTransportadorNome   := Leitor.rCampo(tcStr, 'viaTransportePaisTransportadorNome');

  Result := True;
end;

end.
