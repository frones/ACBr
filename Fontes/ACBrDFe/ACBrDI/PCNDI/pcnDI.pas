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

unit pcnDI;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, pcnConversaoNFe{, pcnSignature, pcnProcNFe, pcnGerador};

type
  TadicaoCollection = class;
  TadicaoCollectionItem = class;
  TacrescimoCollection = class;
  TacrescimoCollectionItem = class;
  TdestaqueNcm = class;
  TdocumentoVinculadoCollection = class;
  TdocumentoVinculadoCollectionItem = class;
  TmercadoriaCollection = class;
  TmercadoriaCollectionItem = class;
  TnomenclaturaValorAduaneiroCollection = class;
  TnomenclaturaValorAduaneiroCollectionItem = class;
  TdeducaoCollection = class;
  TdeducaoCollectionItem = class;
  Tarmazem = class;
  TdeclaracaoEe = class;
  TicmsCollection = class;
  TicmsCollectionItem = class;
  TdocumentoInstrucaoDespachoCollection = class;
  TdocumentoInstrucaoDespachoCollectionItem = class;
  TdossieCollection = class;
  TdossieCollectionItem = class;
  Tembalagem = class;
  TpagamentoCollection = class;
  TpagamentoCollectionItem = class;

  { TDI }

  TDI = class(TObject)
  private
    Fadicao: TadicaoCollection;
    Farmazem: Tarmazem;
    FarmazenamentoRecintoAduaneiroCodigo: Integer;
    FarmazenamentoRecintoAduaneiroNome: String;
    FarmazenamentoSetor: Integer;
    FcanalSelecaoParametrizada: Integer;
    FcaracterizacaoOperacaoCodigoTipo: Integer;
    FcaracterizacaoOperacaoDescricaoTipo: String;
    FcaracterizacaoOperacaoNome: string;
    FcaracterizacaoOperacaoNumero: string;
    FcargaDataChegada: TDateTime;
    FcargaNumeroAgente: String;
    FcargaPaisProcedenciaCodigo: Integer;
    FcargaPaisProcedenciaNome: String;
    FcargaPesoBruto: Currency;
    FcargaPesoLiquido: Currency;
    FcargaUrfEntradaCodigo: Integer;
    FcargaUrfEntradaNome: String;
    FconhecimentoCargaEmbarqueData: TDateTime;
    FconhecimentoCargaEmbarqueLocal: String;
    FconhecimentoCargaId: String;
    FconhecimentoCargaTipoCodigo: Integer;
    FconhecimentoCargaIdMaster: String;
    FconhecimentoCargaTipoNome: String;
    FconhecimentoCargaUtilizacao: Integer;
    FconhecimentoCargaUtilizacaoNome: String;
    FdataDesembaraco: TDateTime;
    FdataRegistro: TDateTime;
    FdeclaracaoEe: TdeclaracaoEe;
    FdocumentoChegadaCargaCodigoTipo: Integer;
    FdocumentoChegadaCargaNome: String;
    FdocumentoChegadaCargaNumero: String;
    FdocumentoInstrucaoDespacho: TdocumentoInstrucaoDespachoCollection;
    Fdossie: TdossieCollection;
    Fembalagem: Tembalagem;
    FfreteCollect: Currency;
    FfreteEmTerritorioNacional: Currency;
    FfreteMoedaNegociadaCodigo: Integer;
    FfreteMoedaNegociadaNome: String;
    FfretePrepaid: Currency;
    FfreteTotalDolares: Currency;
    FfreteTotalMoeda: Currency;
    FfreteTotalReais: Currency;
    Ficms: TicmsCollection;
    FimportadorCodigoTipo: Integer;
    FimportadorCpfRepresentanteLegal: string;
    FimportadorEnderecoBairro: String;
    FimportadorEnderecoCep: string;
    FimportadorEnderecoComplemento: String;
    FimportadorEnderecoLogradouro: String;
    FimportadorEnderecoMunicipio: String;
    FimportadorEnderecoNumero: String;
    FimportadorEnderecoUf: String;
    FimportadorNome: String;
    FimportadorNomeRepresentanteLegal: String;
    FimportadorNumero: String;
    FimportadorNumeroTelefone: String;
    FinformacaoComplementar: String;
    FlocalDescargaTotalDolares: Currency;
    FlocalDescargaTotalReais: Currency;
    FlocalEmbarqueTotalDolares: Currency;
    FlocalEmbarqueTotalReais: Currency;
    FmodalidadeDespachoCodigo: Integer;
    FmodalidadeDespachoNome: String;
    FnumeroDI: String;
    FoperacaoFundap: String;
    Fpagamento: TpagamentoCollection;
    FseguroMoedaNegociadaCodigo: Integer;
    FseguroMoedaNegociadaNome: String;
    FseguroTotalDolares: Currency;
    FseguroTotalMoedaNegociada: Currency;
    FseguroTotalReais: Currency;
    FsequencialRetificacao: Integer;
    FsituacaoEntregaCarga: String;
    FtipoDeclaracaoCodigo: Integer;
    FtipoDeclaracaoNome: String;
    FtotalAdicoes: Integer;
    FurfDespachoCodigo: Integer;
    FurfDespachoNome: String;
    FvalorTotalMultaARecolher: Currency;
    FvalorTotalMultaARecolherAjustado: Currency;
    FviaTransporteCodigo: Integer;
    FviaTransporteMultimodal: String;
    FviaTransporteNome: String;
    FviaTransporteNomeTransportador: String;
    FviaTransporteNomeVeiculo: String;
    FviaTransporteNumeroVeiculo: String;
    FviaTransportePaisTransportadorCodigo: Integer;
    FviaTransportePaisTransportadorNome: String;
  public
    constructor Create;
    destructor Destroy; override;
    property adicao: TadicaoCollection                                         read Fadicao;
    property armazem: Tarmazem                                                 read Farmazem;
    property armazenamentoRecintoAduaneiroCodigo: Integer                      read FarmazenamentoRecintoAduaneiroCodigo  write FarmazenamentoRecintoAduaneiroCodigo;
    property armazenamentoRecintoAduaneiroNome: String                         read FarmazenamentoRecintoAduaneiroNome    write FarmazenamentoRecintoAduaneiroNome;
    property armazenamentoSetor: Integer                                       read FarmazenamentoSetor                   write FarmazenamentoSetor;
    property canalSelecaoParametrizada: Integer                                read FcanalSelecaoParametrizada            write FcanalSelecaoParametrizada;
    property caracterizacaoOperacaoCodigoTipo: Integer                         read FcaracterizacaoOperacaoCodigoTipo     write FcaracterizacaoOperacaoCodigoTipo;
    property caracterizacaoOperacaoDescricaoTipo: String                       read FcaracterizacaoOperacaoDescricaoTipo  write FcaracterizacaoOperacaoDescricaoTipo;
    property caracterizacaoOperacaoNome: String                                read FcaracterizacaoOperacaoNome           write FcaracterizacaoOperacaoNome;
    property caracterizacaoOperacaoNumero: String                              read FcaracterizacaoOperacaoNumero         write FcaracterizacaoOperacaoNumero;
    property cargaDataChegada: TDateTime                                       read FcargaDataChegada                     write FcargaDataChegada;
    property cargaNumeroAgente: String                                         read FcargaNumeroAgente                    write FcargaNumeroAgente;
    property cargaPaisProcedenciaCodigo: Integer                               read FcargaPaisProcedenciaCodigo           write FcargaPaisProcedenciaCodigo;
    property cargaPaisProcedenciaNome: String                                  read FcargaPaisProcedenciaNome             write FcargaPaisProcedenciaNome;
    property cargaPesoBruto: Currency                                          read FcargaPesoBruto                       write FcargaPesoBruto;
    property cargaPesoLiquido: Currency                                        read FcargaPesoLiquido                     write FcargaPesoLiquido;
    property cargaUrfEntradaCodigo: Integer                                    read FcargaUrfEntradaCodigo                write FcargaUrfEntradaCodigo;
    property cargaUrfEntradaNome: String                                       read FcargaUrfEntradaNome                  write FcargaUrfEntradaNome;
    property conhecimentoCargaEmbarqueData: TDateTime                          read FconhecimentoCargaEmbarqueData        write FconhecimentoCargaEmbarqueData;
    property conhecimentoCargaEmbarqueLocal: String                            read FconhecimentoCargaEmbarqueLocal       write FconhecimentoCargaEmbarqueLocal;
    property conhecimentoCargaId: String                                       read FconhecimentoCargaId                  write FconhecimentoCargaId;
    property conhecimentoCargaTipoCodigo: Integer                              read FconhecimentoCargaTipoCodigo          write FconhecimentoCargaTipoCodigo;
    property conhecimentoCargaIdMaster: String                                 read FconhecimentoCargaIdMaster            write FconhecimentoCargaIdMaster;
    property conhecimentoCargaTipoNome: String                                 read FconhecimentoCargaTipoNome            write FconhecimentoCargaTipoNome;
    property conhecimentoCargaUtilizacao: Integer                              read FconhecimentoCargaUtilizacao          write FconhecimentoCargaUtilizacao;
    property conhecimentoCargaUtilizacaoNome: String                           read FconhecimentoCargaUtilizacaoNome      write FconhecimentoCargaUtilizacaoNome;
    property dataDesembaraco: TDateTime                                        read FdataDesembaraco                      write FdataDesembaraco;
    property dataRegistro: TDateTime                                           read FdataRegistro                         write FdataRegistro;
    property declaracaoEe: TdeclaracaoEe                                       read FdeclaracaoEe;
    property documentoChegadaCargaCodigoTipo: Integer                          read FdocumentoChegadaCargaCodigoTipo      write FdocumentoChegadaCargaCodigoTipo;
    property documentoChegadaCargaNome: String                                 read FdocumentoChegadaCargaNome            write FdocumentoChegadaCargaNome;
    property documentoChegadaCargaNumero: String                               read FdocumentoChegadaCargaNumero          write FdocumentoChegadaCargaNumero;
    property documentoInstrucaoDespacho: TdocumentoInstrucaoDespachoCollection read FdocumentoInstrucaoDespacho;
    property dossie: TdossieCollection                                         read Fdossie;
    property embalagem: Tembalagem                                             read Fembalagem;
    property freteCollect: Currency                                            read FfreteCollect                         write FfreteCollect;
    property freteEmTerritorioNacional: Currency                               read FfreteEmTerritorioNacional            write FfreteEmTerritorioNacional;
    property freteMoedaNegociadaCodigo: Integer                                read FfreteMoedaNegociadaCodigo            write FfreteMoedaNegociadaCodigo;
    property freteMoedaNegociadaNome: String                                   read FfreteMoedaNegociadaNome              write FfreteMoedaNegociadaNome;
    property fretePrepaid: Currency                                            read FfretePrepaid                         write FfretePrepaid;
    property freteTotalDolares: Currency                                       read FfreteTotalDolares                    write FfreteTotalDolares;
    property freteTotalMoeda: Currency                                         read FfreteTotalMoeda                      write FfreteTotalMoeda;
    property freteTotalReais: Currency                                         read FfreteTotalReais                      write FfreteTotalReais;
    property icms: TicmsCollection                                             read Ficms;
    property importadorCodigoTipo: Integer                                     read FimportadorCodigoTipo                 write FimportadorCodigoTipo;
    property importadorCpfRepresentanteLegal: String                           read FimportadorCpfRepresentanteLegal      write FimportadorCpfRepresentanteLegal;
    property importadorEnderecoBairro: String                                  read FimportadorEnderecoBairro             write FimportadorEnderecoBairro;
    property importadorEnderecoCep: string                                     read FimportadorEnderecoCep                write FimportadorEnderecoCep;
    property importadorEnderecoComplemento: String                             read FimportadorEnderecoComplemento        write FimportadorEnderecoComplemento;
    property importadorEnderecoLogradouro: String                              read FimportadorEnderecoLogradouro         write FimportadorEnderecoLogradouro;
    property importadorEnderecoMunicipio: String                               read FimportadorEnderecoMunicipio          write FimportadorEnderecoMunicipio;
    property importadorEnderecoNumero: String                                  read FimportadorEnderecoNumero             write FimportadorEnderecoNumero;
    property importadorEnderecoUf: String                                      read FimportadorEnderecoUf                 write FimportadorEnderecoUf;
    property importadorNome: String                                            read FimportadorNome                       write FimportadorNome;
    property importadorNomeRepresentanteLegal: String                          read FimportadorNomeRepresentanteLegal     write FimportadorNomeRepresentanteLegal;
    property importadorNumero: String                                          read FimportadorNumero                     write FimportadorNumero;
    property importadorNumeroTelefone: String                                  read FimportadorNumeroTelefone             write FimportadorNumeroTelefone;
    property informacaoComplementar: String                                    read FinformacaoComplementar               write FinformacaoComplementar;
    property localDescargaTotalDolares: Currency                               read FlocalDescargaTotalDolares            write FlocalDescargaTotalDolares;
    property localDescargaTotalReais: Currency                                 read FlocalDescargaTotalReais              write FlocalDescargaTotalReais;
    property localEmbarqueTotalDolares: Currency                               read FlocalEmbarqueTotalDolares            write FlocalEmbarqueTotalDolares;
    property localEmbarqueTotalReais: Currency                                 read FlocalEmbarqueTotalReais              write FlocalEmbarqueTotalReais;
    property modalidadeDespachoCodigo: Integer                                 read FmodalidadeDespachoCodigo             write FmodalidadeDespachoCodigo;
    property modalidadeDespachoNome: String                                    read FmodalidadeDespachoNome               write FmodalidadeDespachoNome;
    property numeroDI: String                                                  read FnumeroDI                             write FnumeroDI;
    property operacaoFundap: String                                            read FoperacaoFundap                       write FoperacaoFundap;
    property pagamento: TpagamentoCollection                                   read Fpagamento;
    property seguroMoedaNegociadaCodigo: Integer                               read FseguroMoedaNegociadaCodigo           write FseguroMoedaNegociadaCodigo;
    property seguroMoedaNegociadaNome: String                                  read FseguroMoedaNegociadaNome             write FseguroMoedaNegociadaNome;
    property seguroTotalDolares: Currency                                      read FseguroTotalDolares                   write FseguroTotalDolares;
    property seguroTotalMoedaNegociada: Currency                               read FseguroTotalMoedaNegociada            write FseguroTotalMoedaNegociada;
    property seguroTotalReais: Currency                                        read FseguroTotalReais                     write FseguroTotalReais;
    property sequencialRetificacao: Integer                                    read FsequencialRetificacao                write FsequencialRetificacao;
    property situacaoEntregaCarga: String                                      read FsituacaoEntregaCarga                 write FsituacaoEntregaCarga;
    property tipoDeclaracaoCodigo: Integer                                     read FtipoDeclaracaoCodigo                 write FtipoDeclaracaoCodigo;
    property tipoDeclaracaoNome: String                                        read FtipoDeclaracaoNome                   write FtipoDeclaracaoNome;
    property totalAdicoes: Integer                                             read FtotalAdicoes                         write FtotalAdicoes;
    property urfDespachoCodigo: Integer                                        read FurfDespachoCodigo                    write FurfDespachoCodigo;
    property urfDespachoNome: String                                           read FurfDespachoNome                      write FurfDespachoNome;
    property valorTotalMultaARecolher: Currency                                read FvalorTotalMultaARecolher             write FvalorTotalMultaARecolher;
    property valorTotalMultaARecolherAjustado: Currency                        read FvalorTotalMultaARecolherAjustado     write FvalorTotalMultaARecolherAjustado;
    property viaTransporteCodigo: Integer                                      read FviaTransporteCodigo                  write FviaTransporteCodigo;
    property viaTransporteMultimodal: String                                   read FviaTransporteMultimodal              write FviaTransporteMultimodal;
    property viaTransporteNome: String                                         read FviaTransporteNome                    write FviaTransporteNome;
    property viaTransporteNomeTransportador: String                            read FviaTransporteNomeTransportador       write FviaTransporteNomeTransportador;
    property viaTransporteNomeVeiculo: String                                  read FviaTransporteNomeVeiculo             write FviaTransporteNomeVeiculo;
    property viaTransporteNumeroVeiculo: String                                read FviaTransporteNumeroVeiculo           write FviaTransporteNumeroVeiculo;
    property viaTransportePaisTransportadorCodigo: Integer                     read FviaTransportePaisTransportadorCodigo write FviaTransportePaisTransportadorCodigo;
    property viaTransportePaisTransportadorNome: String                        read FviaTransportePaisTransportadorNome   write FviaTransportePaisTransportadorNome;
  end;

  TadicaoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TadicaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TadicaoCollectionItem);
  public
    function New: TadicaoCollectionItem;
    property Items[Index: Integer]: TadicaoCollectionItem read GetItem write SetItem; default;
  end;

  TadicaoCollectionItem = class(TObject)
  private
    Facrescimo: TacrescimoCollection;
    FcideValorAliquotaEspecifica: Currency;
    FcideValorDevido: Currency;
    FcideValorRecolher: Currency;
    FcodigoRelacaoCompradorVendedor: Integer;
    FcodigoVinculoCompradorVendedor: Integer;
    FcofinsAliquotaAdValorem: Currency;
    FcofinsAliquotaEspecificaQuantidadeUnidade: Currency;
    FcofinsAliquotaEspecificaValor: Currency;
    FcofinsAliquotaReduzida: Currency;
    FcofinsAliquotaValorDevido: Currency;
    FcofinsAliquotaValorRecolher: Currency;
    FcondicaoVendaIncoterm: String;
    FcondicaoVendaLocal: String;
    FcondicaoVendaMetodoValoracaoCodigo: Integer;
    FcondicaoVendaMetodoValoracaoNome: String;
    FcondicaoVendaMoedaCodigo: Integer;
    FcondicaoVendaMoedaNome: String;
    FcondicaoVendaValorMoeda: Currency;
    FcondicaoVendaValorReais: Currency;
    FdadosCambiaisCoberturaCambialCodigo: Integer;
    FdadosCambiaisCoberturaCambialNome: String;
    FdadosCambiaisInstituicaoFinanciadoraCodigo: Integer;
    FdadosCambiaisInstituicaoFinanciadoraNome: String;
    FdadosCambiaisMotivoSemCoberturaCodigo: Integer;
    FdadosCambiaisMotivoSemCoberturaNome: String;
    FdadosCambiaisValorRealCambio: Currency;
    FdadosCargaPaisProcedenciaCodigo: Integer;
    FdadosCargaUrfEntradaCodigo: Integer;
    FdadosCargaViaTransporteCodigo: Integer;
    FdadosMercadoriaAplicacao: String;
    FdadosMercadoriaCodigoNaladiNCCA: Integer;
    FdadosMercadoriaCodigoNaladiSH: Integer;
    FdadosMercadoriaCodigoNcm: String;
    FdadosMercadoriaCondicao: String;
    FdadosMercadoriaMedidaEstatisticaQuantidade: Currency;
    FdadosMercadoriaMedidaEstatisticaUnidade: String;
    FdadosMercadoriaNomeNcm: String;
    FdadosMercadoriaPesoLiquido: Currency;
    FdcrCoeficienteReducao: Integer;
    FdcrIdentificacao: Integer;
    FdcrValorDevido: Currency;
    FdcrValorDolar: Currency;
    FdcrValorReal: Currency;
    FdcrValorRecolher: Currency;
    Fdeducao: TdeducaoCollection;
    FdestaqueNcm: TdestaqueNcm;
    FdocumentoVinculado: TdocumentoVinculadoCollection;
    FfabricanteCidade: String;
    FfabricanteComplemento: String;
    FfabricanteEstado: String;
    FfabricanteLogradouro: String;
    FfabricanteNome: String;
    FfabricanteNumero: Integer;
    FfornecedorCidade: String;
    FfornecedorComplemento: String;
    FfornecedorEstado: String;
    FfornecedorLogradouro: String;
    FfornecedorNome: String;
    FfornecedorNumero: Integer;
    FfreteMoedaNegociadaCodigo: Integer;
    FfreteValorMoedaNegociada: Currency;
    FfreteValorReais: Currency;
    FiiAcordoTarifarioAladiCodigo: Integer;
    FiiAcordoTarifarioAladiNome: String;
    FiiAcordoTarifarioAtoLegalAno: Integer;
    FiiAcordoTarifarioAtoLegalCodigo: String;
    FiiAcordoTarifarioAtoLegalEX: Integer;
    FiiAcordoTarifarioAtoLegalNumero: Integer;
    FiiAcordoTarifarioAtoLegalOrgaoEmissor: String;
    FiiAcordoTarifarioTipoCodigo: Integer;
    FiiAcordoTarifarioTipoNome: String;
    FiiAliquotaAcordo: Integer;
    FiiAliquotaAdValorem: Currency;
    FiiAliquotaPercentualReducao: Currency;
    FiiAliquotaReduzida: Currency;
    FiiAliquotaValorCalculado: Currency;
    FiiAliquotaValorDevido: Currency;
    FiiAliquotaValorRecolher: Currency;
    FiiAliquotaValorReduzido: Currency;
    FiiBaseCalculo: Currency;
    FiiFundamentoLegalCodigo: Integer;
    FiiMotivoAdmissaoTemporariaCodigo: Integer;
    FiiRegimeTributacaoCodigo: Integer;
    FiiRegimeTributacaoNome: String;
    FipiAliquotaAdValorem: Currency;
    FipiAliquotaEspecificaCapacidadeRecipciente: Currency;
    FipiAliquotaEspecificaQuantidadeUnidadeMedida: Currency;
    FipiAliquotaEspecificaTipoRecipienteCodigo: Currency;
    FipiAliquotaEspecificaValorUnidadeMedida: Currency;
    FipiAliquotaNotaComplementarTIPI: Currency;
    FipiAliquotaReduzida: Currency;
    FipiAliquotaValorDevido: Currency;
    FipiAliquotaValorRecolher: Currency;
    FipiRegimeTributacaoCodigo: Integer;
    FipiRegimeTributacaoNome: String;
    Fmercadoria: TmercadoriaCollection;
    FnomenclaturaValorAduaneiro: TnomenclaturaValorAduaneiroCollection;
    FnumeroAdicao: Integer;
    FnumeroDI: String;
    FnumeroLI: String;
    FpaisAquisicaoMercadoriaCodigo: Integer;
    FpaisAquisicaoMercadoriaNome: String;
    FpaisOrigemMercadoriaCodigo: Integer;
    FpaisOrigemMercadoriaNome: String;
    FpisCofinsBaseCalculoAliquotaICMS: Currency;
    FpisCofinsBaseCalculoFundamentoLegalCodigo: Integer;
    FpisCofinsBaseCalculoPercentualReducao: Currency;
    FpisCofinsBaseCalculoValor: Currency;
    FpisCofinsFundamentoLegalReducaoCodigo: Integer;
    FpisCofinsRegimeTributacaoCodigo: Integer;
    FpisCofinsRegimeTributacaoNome: String;
    FpisPasepAliquotaAdValorem: Currency;
    FpisPasepAliquotaEspecificaQuantidadeUnidade: Currency;
    FpisPasepAliquotaEspecificaValor: Currency;
    FpisPasepAliquotaReduzida: Currency;
    FpisPasepAliquotaValorDevido: Currency;
    FpisPasepAliquotaValorRecolher: Currency;
    FrelacaoCompradorVendedor: String;
    FseguroMoedaNegociadaCodigo: Integer;
    FseguroValorMoedaNegociada: Currency;
    FseguroValorReais: Currency;
    FsequencialRetificacao: Integer;
    FvalorMultaARecolher: Currency;
    FvalorMultaARecolherAjustado: Currency;
    FvalorReaisFreteInternacional: Currency;
    FvalorReaisSeguroInternacional: Currency;
    FvalorTotalCondicaoVenda: Currency;
    FvinculoCompradorVendedor: String;
  public
    constructor Create;
    destructor Destroy; override;
    property acrescimo: TacrescimoCollection                        read Facrescimo;
    property cideValorAliquotaEspecifica: Currency                  read FcideValorAliquotaEspecifica                  write FcideValorAliquotaEspecifica;
    property cideValorDevido: Currency                              read FcideValorDevido                              write FcideValorDevido;
    property cideValorRecolher: Currency                            read FcideValorRecolher                            write FcideValorRecolher;
    property codigoRelacaoCompradorVendedor: Integer                read FcodigoRelacaoCompradorVendedor               write FcodigoRelacaoCompradorVendedor;
    property codigoVinculoCompradorVendedor: Integer                read FcodigoVinculoCompradorVendedor               write FcodigoVinculoCompradorVendedor;
    property cofinsAliquotaAdValorem: Currency                      read FcofinsAliquotaAdValorem                      write FcofinsAliquotaAdValorem;
    property cofinsAliquotaEspecificaQuantidadeUnidade: Currency    read FcofinsAliquotaEspecificaQuantidadeUnidade    write FcofinsAliquotaEspecificaQuantidadeUnidade;
    property cofinsAliquotaEspecificaValor: Currency                read FcofinsAliquotaEspecificaValor                write FcofinsAliquotaEspecificaValor;
    property cofinsAliquotaReduzida: Currency                       read FcofinsAliquotaReduzida                       write FcofinsAliquotaReduzida;
    property cofinsAliquotaValorDevido: Currency                    read FcofinsAliquotaValorDevido                    write FcofinsAliquotaValorDevido;
    property cofinsAliquotaValorRecolher: Currency                  read FcofinsAliquotaValorRecolher                  write FcofinsAliquotaValorRecolher;
    property condicaoVendaIncoterm: String                          read FcondicaoVendaIncoterm                        write FcondicaoVendaIncoterm;
    property condicaoVendaLocal: String                             read FcondicaoVendaLocal                           write FcondicaoVendaLocal;
    property condicaoVendaMetodoValoracaoCodigo: Integer            read FcondicaoVendaMetodoValoracaoCodigo           write FcondicaoVendaMetodoValoracaoCodigo;
    property condicaoVendaMetodoValoracaoNome: String               read FcondicaoVendaMetodoValoracaoNome             write FcondicaoVendaMetodoValoracaoNome;
    property condicaoVendaMoedaCodigo: Integer                      read FcondicaoVendaMoedaCodigo                     write FcondicaoVendaMoedaCodigo;
    property condicaoVendaMoedaNome: String                         read FcondicaoVendaMoedaNome                       write FcondicaoVendaMoedaNome;
    property condicaoVendaValorMoeda: Currency                      read FcondicaoVendaValorMoeda                      write FcondicaoVendaValorMoeda;
    property condicaoVendaValorReais: Currency                      read FcondicaoVendaValorReais                      write FcondicaoVendaValorReais;
    property dadosCambiaisCoberturaCambialCodigo: Integer           read FdadosCambiaisCoberturaCambialCodigo          write FdadosCambiaisCoberturaCambialCodigo;
    property dadosCambiaisCoberturaCambialNome: String              read FdadosCambiaisCoberturaCambialNome            write FdadosCambiaisCoberturaCambialNome;
    property dadosCambiaisInstituicaoFinanciadoraCodigo: Integer    read FdadosCambiaisInstituicaoFinanciadoraCodigo   write FdadosCambiaisInstituicaoFinanciadoraCodigo;
    property dadosCambiaisInstituicaoFinanciadoraNome: String       read FdadosCambiaisInstituicaoFinanciadoraNome     write FdadosCambiaisInstituicaoFinanciadoraNome;
    property dadosCambiaisMotivoSemCoberturaCodigo: Integer         read FdadosCambiaisMotivoSemCoberturaCodigo        write FdadosCambiaisMotivoSemCoberturaCodigo;
    property dadosCambiaisMotivoSemCoberturaNome: String            read FdadosCambiaisMotivoSemCoberturaNome          write FdadosCambiaisMotivoSemCoberturaNome;
    property dadosCambiaisValorRealCambio: Currency                 read FdadosCambiaisValorRealCambio                 write FdadosCambiaisValorRealCambio;
    property dadosCargaPaisProcedenciaCodigo: Integer               read FdadosCargaPaisProcedenciaCodigo              write FdadosCargaPaisProcedenciaCodigo;
    property dadosCargaUrfEntradaCodigo: Integer                    read FdadosCargaUrfEntradaCodigo                   write FdadosCargaUrfEntradaCodigo;
    property dadosCargaViaTransporteCodigo: Integer                 read FdadosCargaViaTransporteCodigo                write FdadosCargaViaTransporteCodigo;
    property dadosMercadoriaAplicacao: String                       read FdadosMercadoriaAplicacao                     write FdadosMercadoriaAplicacao;
    property dadosMercadoriaCodigoNaladiNCCA: Integer               read FdadosMercadoriaCodigoNaladiNCCA              write FdadosMercadoriaCodigoNaladiNCCA;
    property dadosMercadoriaCodigoNaladiSH: Integer                 read FdadosMercadoriaCodigoNaladiSH                write FdadosMercadoriaCodigoNaladiSH;
    property dadosMercadoriaCodigoNcm: String                       read FdadosMercadoriaCodigoNcm                     write FdadosMercadoriaCodigoNcm;
    property dadosMercadoriaCondicao: String                        read FdadosMercadoriaCondicao                      write FdadosMercadoriaCondicao;
    property dadosMercadoriaMedidaEstatisticaQuantidade: Currency   read FdadosMercadoriaMedidaEstatisticaQuantidade   write FdadosMercadoriaMedidaEstatisticaQuantidade;
    property dadosMercadoriaMedidaEstatisticaUnidade: String        read FdadosMercadoriaMedidaEstatisticaUnidade      write FdadosMercadoriaMedidaEstatisticaUnidade;
    property dadosMercadoriaNomeNcm: String                         read FdadosMercadoriaNomeNcm                       write FdadosMercadoriaNomeNcm;
    property dadosMercadoriaPesoLiquido: Currency                   read FdadosMercadoriaPesoLiquido                   write FdadosMercadoriaPesoLiquido;
    property dcrCoeficienteReducao: Integer                         read FdcrCoeficienteReducao                        write FdcrCoeficienteReducao;
    property dcrIdentificacao: Integer                              read FdcrIdentificacao                             write FdcrIdentificacao;
    property dcrValorDevido: Currency                               read FdcrValorDevido                               write FdcrValorDevido;
    property dcrValorDolar: Currency                                read FdcrValorDolar                                write FdcrValorDolar;
    property dcrValorReal: Currency                                 read FdcrValorReal                                 write FdcrValorReal;
    property dcrValorRecolher: Currency                             read FdcrValorRecolher                             write FdcrValorRecolher;
    property deducao: TdeducaoCollection                            read Fdeducao;
    property destaqueNcm: TdestaqueNcm                              read FdestaqueNcm;
    property documentoVinculado: TdocumentoVinculadoCollection      read FdocumentoVinculado;
    property fabricanteCidade: String                               read FfabricanteCidade                             write FfabricanteCidade;
    property fabricanteComplemento: String                          read FfabricanteComplemento                        write FfabricanteComplemento;
    property fabricanteEstado: String                               read FfabricanteEstado                             write FfabricanteEstado;
    property fabricanteLogradouro: String                           read FfabricanteLogradouro                         write FfabricanteLogradouro;
    property fabricanteNome: String                                 read FfabricanteNome                               write FfabricanteNome;
    property fabricanteNumero: Integer                              read FfabricanteNumero                             write FfabricanteNumero;
    property fornecedorCidade: String                               read FfornecedorCidade                             write FfornecedorCidade;
    property fornecedorComplemento: String                          read FfornecedorComplemento                        write FfornecedorComplemento;
    property fornecedorEstado: String                               read FfornecedorEstado                             write FfornecedorEstado;
    property fornecedorLogradouro: String                           read FfornecedorLogradouro                         write FfornecedorLogradouro;
    property fornecedorNome: String                                 read FfornecedorNome                               write FfornecedorNome;
    property fornecedorNumero: Integer                              read FfornecedorNumero                             write FfornecedorNumero;
    property freteMoedaNegociadaCodigo: Integer                     read FfreteMoedaNegociadaCodigo                    write FfreteMoedaNegociadaCodigo;
    property freteValorMoedaNegociada: Currency                     read FfreteValorMoedaNegociada                     write FfreteValorMoedaNegociada;
    property freteValorReais: Currency                              read FfreteValorReais                              write FfreteValorReais;
    property iiAcordoTarifarioAladiCodigo: Integer                  read FiiAcordoTarifarioAladiCodigo                 write FiiAcordoTarifarioAladiCodigo;
    property iiAcordoTarifarioAladiNome: String                     read FiiAcordoTarifarioAladiNome                   write FiiAcordoTarifarioAladiNome;
    property iiAcordoTarifarioAtoLegalAno: Integer                  read FiiAcordoTarifarioAtoLegalAno                 write FiiAcordoTarifarioAtoLegalAno;
    property iiAcordoTarifarioAtoLegalCodigo: String                read FiiAcordoTarifarioAtoLegalCodigo              write FiiAcordoTarifarioAtoLegalCodigo;
    property iiAcordoTarifarioAtoLegalEX: Integer                   read FiiAcordoTarifarioAtoLegalEX                  write FiiAcordoTarifarioAtoLegalEX;
    property iiAcordoTarifarioAtoLegalNumero: Integer               read FiiAcordoTarifarioAtoLegalNumero              write FiiAcordoTarifarioAtoLegalNumero;
    property iiAcordoTarifarioAtoLegalOrgaoEmissor: String          read FiiAcordoTarifarioAtoLegalOrgaoEmissor        write FiiAcordoTarifarioAtoLegalOrgaoEmissor;
    property iiAcordoTarifarioTipoCodigo: Integer                   read FiiAcordoTarifarioTipoCodigo                  write FiiAcordoTarifarioTipoCodigo;
    property iiAcordoTarifarioTipoNome: String                      read FiiAcordoTarifarioTipoNome                    write FiiAcordoTarifarioTipoNome;
    property iiAliquotaAcordo: Integer                              read FiiAliquotaAcordo                             write FiiAliquotaAcordo;
    property iiAliquotaAdValorem: Currency                          read FiiAliquotaAdValorem                          write FiiAliquotaAdValorem;
    property iiAliquotaPercentualReducao: Currency                  read FiiAliquotaPercentualReducao                  write FiiAliquotaPercentualReducao;
    property iiAliquotaReduzida: Currency                           read FiiAliquotaReduzida                           write FiiAliquotaReduzida;
    property iiAliquotaValorCalculado: Currency                     read FiiAliquotaValorCalculado                     write FiiAliquotaValorCalculado;
    property iiAliquotaValorDevido: Currency                        read FiiAliquotaValorDevido                        write FiiAliquotaValorDevido;
    property iiAliquotaValorRecolher: Currency                      read FiiAliquotaValorRecolher                      write FiiAliquotaValorRecolher;
    property iiAliquotaValorReduzido: Currency                      read FiiAliquotaValorReduzido                      write FiiAliquotaValorReduzido;
    property iiBaseCalculo: Currency                                read FiiBaseCalculo                                write FiiBaseCalculo;
    property iiFundamentoLegalCodigo: Integer                       read FiiFundamentoLegalCodigo                      write FiiFundamentoLegalCodigo;
    property iiMotivoAdmissaoTemporariaCodigo: Integer              read FiiMotivoAdmissaoTemporariaCodigo             write FiiMotivoAdmissaoTemporariaCodigo;
    property iiRegimeTributacaoCodigo: Integer                      read FiiRegimeTributacaoCodigo                     write FiiRegimeTributacaoCodigo;
    property iiRegimeTributacaoNome: String                         read FiiRegimeTributacaoNome                       write FiiRegimeTributacaoNome;
    property ipiAliquotaAdValorem: Currency                         read FipiAliquotaAdValorem                         write FipiAliquotaAdValorem;
    property ipiAliquotaEspecificaCapacidadeRecipciente: Currency   read FipiAliquotaEspecificaCapacidadeRecipciente   write FipiAliquotaEspecificaCapacidadeRecipciente;
    property ipiAliquotaEspecificaQuantidadeUnidadeMedida: Currency read FipiAliquotaEspecificaQuantidadeUnidadeMedida write FipiAliquotaEspecificaQuantidadeUnidadeMedida;
    property ipiAliquotaEspecificaTipoRecipienteCodigo: Currency    read FipiAliquotaEspecificaTipoRecipienteCodigo    write FipiAliquotaEspecificaTipoRecipienteCodigo;
    property ipiAliquotaEspecificaValorUnidadeMedida: Currency      read FipiAliquotaEspecificaValorUnidadeMedida      write FipiAliquotaEspecificaValorUnidadeMedida;
    property ipiAliquotaNotaComplementarTIPI: Currency              read FipiAliquotaNotaComplementarTIPI              write FipiAliquotaNotaComplementarTIPI;
    property ipiAliquotaReduzida: Currency                          read FipiAliquotaReduzida                          write FipiAliquotaReduzida;
    property ipiAliquotaValorDevido: Currency                       read FipiAliquotaValorDevido                       write FipiAliquotaValorDevido;
    property ipiAliquotaValorRecolher: Currency                     read FipiAliquotaValorRecolher                     write FipiAliquotaValorRecolher;
    property ipiRegimeTributacaoCodigo: Integer                     read FipiRegimeTributacaoCodigo                    write FipiRegimeTributacaoCodigo;
    property ipiRegimeTributacaoNome: String                        read FipiRegimeTributacaoNome                      write FipiRegimeTributacaoNome;
    property mercadoria: TmercadoriaCollection                      read Fmercadoria;
    property nomenclaturaValorAduaneiro: TnomenclaturaValorAduaneiroCollection read FnomenclaturaValorAduaneiro;
    property numeroAdicao: Integer                                  read FnumeroAdicao                                 write FnumeroAdicao;
    property numeroDI: String                                       read FnumeroDI                                     write FnumeroDI;
    property numeroLI: String                                       read FnumeroLI                                     write FnumeroLI;
    property paisAquisicaoMercadoriaCodigo: Integer                 read FpaisAquisicaoMercadoriaCodigo                write FpaisAquisicaoMercadoriaCodigo;
    property paisAquisicaoMercadoriaNome: String                    read FpaisAquisicaoMercadoriaNome                  write FpaisAquisicaoMercadoriaNome;
    property paisOrigemMercadoriaCodigo: Integer                    read FpaisOrigemMercadoriaCodigo                   write FpaisOrigemMercadoriaCodigo;
    property paisOrigemMercadoriaNome: String                       read FpaisOrigemMercadoriaNome                     write FpaisOrigemMercadoriaNome;
    property pisCofinsBaseCalculoAliquotaICMS: Currency             read FpisCofinsBaseCalculoAliquotaICMS             write FpisCofinsBaseCalculoAliquotaICMS;
    property pisCofinsBaseCalculoFundamentoLegalCodigo: Integer     read FpisCofinsBaseCalculoFundamentoLegalCodigo    write FpisCofinsBaseCalculoFundamentoLegalCodigo;
    property pisCofinsBaseCalculoPercentualReducao: Currency        read FpisCofinsBaseCalculoPercentualReducao        write FpisCofinsBaseCalculoPercentualReducao;
    property pisCofinsBaseCalculoValor: Currency                    read FpisCofinsBaseCalculoValor                    write FpisCofinsBaseCalculoValor;
    property pisCofinsFundamentoLegalReducaoCodigo: Integer         read FpisCofinsFundamentoLegalReducaoCodigo        write FpisCofinsFundamentoLegalReducaoCodigo;
    property pisCofinsRegimeTributacaoCodigo: Integer               read FpisCofinsRegimeTributacaoCodigo              write FpisCofinsRegimeTributacaoCodigo;
    property pisCofinsRegimeTributacaoNome: String                  read FpisCofinsRegimeTributacaoNome                write FpisCofinsRegimeTributacaoNome;
    property pisPasepAliquotaAdValorem: Currency                    read FpisPasepAliquotaAdValorem                    write FpisPasepAliquotaAdValorem;
    property pisPasepAliquotaEspecificaQuantidadeUnidade: Currency  read FpisPasepAliquotaEspecificaQuantidadeUnidade  write FpisPasepAliquotaEspecificaQuantidadeUnidade;
    property pisPasepAliquotaEspecificaValor: Currency              read FpisPasepAliquotaEspecificaValor              write FpisPasepAliquotaEspecificaValor;
    property pisPasepAliquotaReduzida: Currency                     read FpisPasepAliquotaReduzida                     write FpisPasepAliquotaReduzida;
    property pisPasepAliquotaValorDevido: Currency                  read FpisPasepAliquotaValorDevido                  write FpisPasepAliquotaValorDevido;
    property pisPasepAliquotaValorRecolher: Currency                read FpisPasepAliquotaValorRecolher                write FpisPasepAliquotaValorRecolher;
    property relacaoCompradorVendedor: String                       read FrelacaoCompradorVendedor                     write FrelacaoCompradorVendedor;
    property seguroMoedaNegociadaCodigo: Integer                    read FseguroMoedaNegociadaCodigo                   write FseguroMoedaNegociadaCodigo;
    property seguroValorMoedaNegociada: Currency                    read FseguroValorMoedaNegociada                    write FseguroValorMoedaNegociada;
    property seguroValorReais: Currency                             read FseguroValorReais                             write FseguroValorReais;
    property sequencialRetificacao: Integer                         read FsequencialRetificacao                        write FsequencialRetificacao;
    property valorMultaARecolher: Currency                          read FvalorMultaARecolher                          write FvalorMultaARecolher;
    property valorMultaARecolherAjustado: Currency                  read FvalorMultaARecolherAjustado                  write FvalorMultaARecolherAjustado;
    property valorReaisFreteInternacional: Currency                 read FvalorReaisFreteInternacional                 write FvalorReaisFreteInternacional;
    property valorReaisSeguroInternacional: Currency                read FvalorReaisSeguroInternacional                write FvalorReaisSeguroInternacional;
    property valorTotalCondicaoVenda: Currency                      read FvalorTotalCondicaoVenda                      write FvalorTotalCondicaoVenda;
    property vinculoCompradorVendedor: String                       read FvinculoCompradorVendedor                     write FvinculoCompradorVendedor;
  end;

  TacrescimoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TacrescimoCollectionItem;
    procedure SetItem(Index: Integer; Value: TacrescimoCollectionItem);
  public
    function New: TacrescimoCollectionItem;
    property Items[Index: Integer]: TacrescimoCollectionItem read GetItem write SetItem; default;
  end;

  TacrescimoCollectionItem = class(TObject)
  private
    FcodigoAcrescimo: Integer;
    Fdenominacao: String;
    FmoedaNegociadaCodigo: Integer;
    FmoedaNegociadaNome: String;
    FvalorMoedaNegociada: Currency;
    FvalorReais: Currency;
  public
    property codigoAcrescimo: Integer       read FcodigoAcrescimo      write FcodigoAcrescimo;
    property denominacao: String            read Fdenominacao          write Fdenominacao;
    property moedaNegociadaCodigo: Integer  read FmoedaNegociadaCodigo write FmoedaNegociadaCodigo;
    property moedaNegociadaNome: String     read FmoedaNegociadaNome   write FmoedaNegociadaNome;
    property valorMoedaNegociada: Currency  read FvalorMoedaNegociada  write FvalorMoedaNegociada;
    property valorReais: Currency           read FvalorReais           write FvalorReais;
  end;

  TdeducaoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TdeducaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TdeducaoCollectionItem);
  public
    function New: TdeducaoCollectionItem;
    property Items[Index: Integer]: TdeducaoCollectionItem read GetItem write SetItem; default;
  end;

  TdeducaoCollectionItem = class(TObject)
  private
    FcodigoDeducao: Integer;
    Fdenominacao: String;
    FmoedaNegociadaCodigo: Integer;
    FmoedaNegociadaNome: String;
    FvalorMoedaNegociada: Currency;
    FvalorReais: Currency;
  public
    property codigoDeducao: Integer        read FcodigoDeducao        write FcodigoDeducao;
    property denominacao: String           read Fdenominacao          write Fdenominacao;
    property moedaNegociadaCodigo: Integer read FmoedaNegociadaCodigo write FmoedaNegociadaCodigo;
    property moedaNegociadaNome: String    read FmoedaNegociadaNome   write FmoedaNegociadaNome;
    property valorMoedaNegociada: Currency read FvalorMoedaNegociada  write FvalorMoedaNegociada;
    property valorReais: Currency          read FvalorReais           write FvalorReais;
  end;

  TdestaqueNcm = class(TObject)
  private
    FnumeroDestaque: Integer;
  public
    property numeroDestaque: Integer read FnumeroDestaque write FnumeroDestaque;
  end;

  TdocumentoVinculadoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TdocumentoVinculadoCollectionItem;
    procedure SetItem(Index: Integer; Value: TdocumentoVinculadoCollectionItem);
  public
    function New: TdocumentoVinculadoCollectionItem;
    property Items[Index: Integer]: TdocumentoVinculadoCollectionItem read GetItem write SetItem; default;
  end;

  TdocumentoVinculadoCollectionItem = class(TObject)
  private
    FcodigoTipo: Integer;
    FnomeTipo: String;
    Fnumero: String;
  public
    property codigoTipo: Integer read FcodigoTipo write FcodigoTipo;
    property nomeTipo: String    read FnomeTipo   write FnomeTipo;
    property numero: String      read Fnumero     write Fnumero;
  end;

  TmercadoriaCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TmercadoriaCollectionItem;
    procedure SetItem(Index: Integer; Value: TmercadoriaCollectionItem);
  public
    function New: TmercadoriaCollectionItem;
    property Items[Index: Integer]: TmercadoriaCollectionItem read GetItem write SetItem; default;
  end;

  TmercadoriaCollectionItem = class(TObject)
  private
    FdescricaoMercadoria: String;
    FnumeroSequencialItem: Integer;
    Fquantidade: Currency;
    FunidadeMedida: String;
    FvalorUnitario: Currency;
  public
    property descricaoMercadoria: String   read FdescricaoMercadoria  write FdescricaoMercadoria;
    property numeroSequencialItem: Integer read FnumeroSequencialItem write FnumeroSequencialItem;
    property quantidade: Currency          read Fquantidade           write Fquantidade;
    property unidadeMedida: String         read FunidadeMedida        write FunidadeMedida;
    property valorUnitario: Currency       read FvalorUnitario        write FvalorUnitario;
  end;

  TnomenclaturaValorAduaneiroCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TnomenclaturaValorAduaneiroCollectionItem;
    procedure SetItem(Index: Integer; Value: TnomenclaturaValorAduaneiroCollectionItem);
  public
    function New: TnomenclaturaValorAduaneiroCollectionItem;
    property Items[Index: Integer]: TnomenclaturaValorAduaneiroCollectionItem read GetItem write SetItem; default;
  end;

  TnomenclaturaValorAduaneiroCollectionItem = class(TObject)
  private
    Fatributo: String;
    Fespecificacao: Integer;
    FnivelNome: String;
  public
    property atributo: String       read Fatributo      write Fatributo;
    property especificacao: Integer read Fespecificacao write Fespecificacao;
    property nivelNome: String      read FnivelNome     write FnivelNome;
  end;

  Tarmazem = class(TObject)
  private
    FnomeArmazem: String;
  public
    property nomeArmazem: String read FnomeArmazem write FnomeArmazem;
  end;

  TdeclaracaoEe = class(TObject)
  private
    FfaixaFinal: Integer;
    FfaixaInicial: Integer;
    FnumeroDeclaracaoEstrangeira: String;
  public
    property faixaFinal: Integer                 read FfaixaFinal                  write FfaixaFinal;
    property faixaInicial: Integer               read FfaixaInicial                write FfaixaInicial;
    property numeroDeclaracaoEstrangeira: String read FnumeroDeclaracaoEstrangeira write FnumeroDeclaracaoEstrangeira;
  end;

  TdocumentoInstrucaoDespachoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TdocumentoInstrucaoDespachoCollectionItem;
    procedure SetItem(Index: Integer; Value: TdocumentoInstrucaoDespachoCollectionItem);
  public
    function New: TdocumentoInstrucaoDespachoCollectionItem;
    property Items[Index: Integer]: TdocumentoInstrucaoDespachoCollectionItem read GetItem write SetItem; default;
  end;

  TdocumentoInstrucaoDespachoCollectionItem = class(TObject)
  private
    FcodigoTipoDocumentoDespacho: Integer;
    FnomeDocumentoDespacho: String;
    FnumeroDocumentoDespacho: String;
  public
    property codigoTipoDocumentoDespacho: Integer read FcodigoTipoDocumentoDespacho write FcodigoTipoDocumentoDespacho;
    property nomeDocumentoDespacho: String        read FnomeDocumentoDespacho       write FnomeDocumentoDespacho;
    property numeroDocumentoDespacho: String      read FnumeroDocumentoDespacho     write FnumeroDocumentoDespacho;
  end;

  TdossieCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TdossieCollectionItem;
    procedure SetItem(Index: Integer; Value: TdossieCollectionItem);
  public
    function New: TdossieCollectionItem;
    property Items[Index: Integer]: TdossieCollectionItem read GetItem write SetItem; default;
  end;

  TdossieCollectionItem = class(TObject)
  private
    FdossieDataVinculacao: String;
    FdossieHoraVinculacao: String;
    FdossieNumero: String;
    FindicadorDossieVazio: String;
  public
    property dossieDataVinculacao: String read FdossieDataVinculacao write FdossieDataVinculacao;
    property dossieHoraVinculacao: String read FdossieHoraVinculacao write FdossieHoraVinculacao;
    property dossieNumero: String         read FdossieNumero         write FdossieNumero;
    property indicadorDossieVazio: String read FindicadorDossieVazio write FindicadorDossieVazio;
  end;

  Tembalagem = class(TObject)
  private
    FcodigoTipoEmbalagem: Integer;
    FnomeEmbalagem: String;
    FmoedaNegociadaCodigo: integer;
    FquantidadeVolume: Currency;
  public
    property codigoTipoEmbalagem: Integer  read FcodigoTipoEmbalagem  write FcodigoTipoEmbalagem;
    property nomeEmbalagem: String         read FnomeEmbalagem        write FnomeEmbalagem;
    property moedaNegociadaCodigo: integer read FmoedaNegociadaCodigo write FmoedaNegociadaCodigo;
    property quantidadeVolume: Currency    read FquantidadeVolume     write FquantidadeVolume;
  end;

  TicmsCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TicmsCollectionItem;
    procedure SetItem(Index: Integer; Value: TicmsCollectionItem);
  public
    function New: TicmsCollectionItem;
    property Items[Index: Integer]: TicmsCollectionItem read GetItem write SetItem; default;
  end;

  TicmsCollectionItem = class(TObject)
  private
    FagenciaIcms: Integer;
    FbancoIcms: Integer;
    FcodigoTipoRecolhimentoIcms: Integer;
    FcpfResponsavelRegistro: String;
    FdataPagamentoIcms: TDateTime;
    FdataRegistro: TDateTime;
    FhoraRegistro: String;
    FnomeTipoRecolhimentoIcms: String;
    FnumeroSequencialIcms: Integer;
    FufIcms: String;
    FvalorTotalIcms: Currency;
  public
    property agenciaIcms: Integer                read FagenciaIcms                write FagenciaIcms;
    property bancoIcms: Integer                  read FbancoIcms                  write FbancoIcms;
    property codigoTipoRecolhimentoIcms: Integer read FcodigoTipoRecolhimentoIcms write FcodigoTipoRecolhimentoIcms;
    property cpfResponsavelRegistro: String      read FcpfResponsavelRegistro     write FcpfResponsavelRegistro;
    property dataPagamentoIcms: TDateTime        read FdataPagamentoIcms          write FdataPagamentoIcms;
    property dataRegistro: TDateTime             read FdataRegistro               write FdataRegistro;
    property horaRegistro: String                read FhoraRegistro               write FhoraRegistro;
    property nomeTipoRecolhimentoIcms: String    read FnomeTipoRecolhimentoIcms   write FnomeTipoRecolhimentoIcms;
    property numeroSequencialIcms: Integer       read FnumeroSequencialIcms       write FnumeroSequencialIcms;
    property ufIcms: String                      read FufIcms                     write FufIcms;
    property valorTotalIcms: Currency            read FvalorTotalIcms             write FvalorTotalIcms;
  end;

  TpagamentoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TpagamentoCollectionItem;
    procedure SetItem(Index: Integer; Value: TpagamentoCollectionItem);
  public
    function New: TpagamentoCollectionItem;
    property Items[Index: Integer]: TpagamentoCollectionItem read GetItem write SetItem; default;
  end;

  TpagamentoCollectionItem = class(TObject)
  private
    FagenciaPagamento: Integer;
    FbancoPagamento: Integer;
    FcodigoReceita: Integer;
    FcodigoTipoPagamento: Integer;
    FcontaPagamento: String;
    FdataPagamento: TDateTime;
    FnomeTipoPagamento: String;
    FnumeroRetificacao: Integer;
    FvalorJurosEncargos: Currency;
    FvalorMulta: Currency;
    FvalorReceita: Currency;
  public
    property agenciaPagamento: Integer    read FagenciaPagamento     write FagenciaPagamento;
    property bancoPagamento: Integer      read FbancoPagamento       write FbancoPagamento;
    property codigoReceita: Integer       read FcodigoReceita        write FcodigoReceita;
    property codigoTipoPagamento: Integer read FcodigoTipoPagamento  write FcodigoTipoPagamento;
    property contaPagamento: String       read FcontaPagamento       write FcontaPagamento;
    property dataPagamento: TDateTime     read FdataPagamento        write FdataPagamento;
    property nomeTipoPagamento: String    read FnomeTipoPagamento    write FnomeTipoPagamento;
    property numeroRetificacao: Integer   read FnumeroRetificacao    write FnumeroRetificacao;
    property valorJurosEncargos: Currency read FvalorJurosEncargos   write FvalorJurosEncargos;
    property valorMulta: Currency         read FvalorMulta           write FvalorMulta;
    property valorReceita: Currency       read FvalorReceita         write FvalorReceita;
  end;

implementation

{ TDI }

constructor TDI.Create;
begin
  inherited Create;

  Fadicao       := TadicaoCollection.Create;
  Farmazem      := Tarmazem.Create;
  FdeclaracaoEe := TdeclaracaoEe.Create;
  FdocumentoInstrucaoDespacho := TdocumentoInstrucaoDespachoCollection.Create;
  Fdossie       := TdossieCollection.Create;
  Fembalagem    := Tembalagem.Create;
  Ficms         := TicmsCollection.Create;
  Fpagamento    := TpagamentoCollection.Create;
end;

destructor TDI.Destroy;
begin
  Fadicao.Free;
  Farmazem.Free;
  FdeclaracaoEe.Free;
  FdocumentoInstrucaoDespacho.Free;
  Fdossie.Free;
  Fembalagem.Free;
  Ficms.Free;
  Fpagamento.Free;

  inherited;
end;

{ TadicaoCollection }

function TadicaoCollection.GetItem(Index: Integer): TadicaoCollectionItem;
begin
  Result := TadicaoCollectionItem(inherited GetItem(Index));
end;

procedure TadicaoCollection.SetItem(Index: Integer; Value: TadicaoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TadicaoCollection.New: TadicaoCollectionItem;
begin
  Result := TadicaoCollectionItem.Create;
  Self.Add(Result);
end;

{ Tadicao }

constructor TadicaoCollectionItem.Create;
begin
  inherited Create;

  Facrescimo          := TacrescimoCollection.Create;
  FdocumentoVinculado := TdocumentoVinculadoCollection.Create;
  Fdeducao            := TdeducaoCollection.Create;
  FdestaqueNcm        := TdestaqueNcm.Create;
  Fmercadoria         := TmercadoriaCollection.Create;
  FnomenclaturaValorAduaneiro := TnomenclaturaValorAduaneiroCollection.Create;
end;

destructor TadicaoCollectionItem.Destroy;
begin
  Facrescimo.Free;
  FdocumentoVinculado.Free;
  Fdeducao.Free;
  FdestaqueNcm.Free;
  Fmercadoria.Free;
  FnomenclaturaValorAduaneiro.Free;

  inherited;
end;

{ TacrescimoCollection }

function TacrescimoCollection.GetItem(Index: Integer): TacrescimoCollectionItem;
begin
  Result := TacrescimoCollectionItem(inherited GetItem(Index));
end;

procedure TacrescimoCollection.SetItem(Index: Integer; Value: TacrescimoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TacrescimoCollection.New: TacrescimoCollectionItem;
begin
  Result := TacrescimoCollectionItem.Create;
  Self.Add(Result);
end;

{ TdeducaoCollection }

function TdeducaoCollection.GetItem(Index: Integer): TdeducaoCollectionItem;
begin
  Result := TdeducaoCollectionItem(inherited GetItem(Index));
end;

procedure TdeducaoCollection.SetItem(Index: Integer; Value: TdeducaoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TdeducaoCollection.New: TdeducaoCollectionItem;
begin
  Result := TdeducaoCollectionItem.Create;
  Self.Add(Result);
end;

{ TdocumentoVinculadoCollection }

function TdocumentoVinculadoCollection.GetItem(Index: Integer): TdocumentoVinculadoCollectionItem;
begin
  Result := TdocumentoVinculadoCollectionItem(inherited GetItem(Index));
end;

procedure TdocumentoVinculadoCollection.SetItem(Index: Integer; Value: TdocumentoVinculadoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TdocumentoVinculadoCollection.New: TdocumentoVinculadoCollectionItem;
begin
  Result := TdocumentoVinculadoCollectionItem.Create;
  Self.Add(Result);
end;

{ TmercadoriaCollection }

function TmercadoriaCollection.GetItem(Index: Integer): TmercadoriaCollectionItem;
begin
  Result := TmercadoriaCollectionItem(inherited GetItem(Index));
end;

procedure TmercadoriaCollection.SetItem(Index: Integer; Value: TmercadoriaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TmercadoriaCollection.New: TmercadoriaCollectionItem;
begin
  Result := TmercadoriaCollectionItem.Create;
  Self.Add(Result);
end;

{ TnomenclaturaValorAduaneiroCollection }

function TnomenclaturaValorAduaneiroCollection.GetItem(Index: Integer): TnomenclaturaValorAduaneiroCollectionItem;
begin
  Result := TnomenclaturaValorAduaneiroCollectionItem(inherited GetItem(Index));
end;

procedure TnomenclaturaValorAduaneiroCollection.SetItem(Index: Integer; Value: TnomenclaturaValorAduaneiroCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TnomenclaturaValorAduaneiroCollection.New: TnomenclaturaValorAduaneiroCollectionItem;
begin
  Result := TnomenclaturaValorAduaneiroCollectionItem.Create;
  Self.Add(Result);
end;

{ TdocumentoInstrucaoDespachoCollection }

function TdocumentoInstrucaoDespachoCollection.GetItem(Index: Integer): TdocumentoInstrucaoDespachoCollectionItem;
begin
  Result := TdocumentoInstrucaoDespachoCollectionItem(inherited GetItem(Index));
end;

procedure TdocumentoInstrucaoDespachoCollection.SetItem(Index: Integer; Value: TdocumentoInstrucaoDespachoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TdocumentoInstrucaoDespachoCollection.New: TdocumentoInstrucaoDespachoCollectionItem;
begin
  Result := TdocumentoInstrucaoDespachoCollectionItem.Create;
  Self.Add(Result);
end;

{ TdossieCollection }

function TdossieCollection.GetItem(Index: Integer): TdossieCollectionItem;
begin
  Result := TdossieCollectionItem(inherited GetItem(Index));
end;

procedure TdossieCollection.SetItem(Index: Integer; Value: TdossieCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TdossieCollection.New: TdossieCollectionItem;
begin
  Result := TdossieCollectionItem.Create;
  Self.Add(Result);
end;

{ TpagamentoCollection }

function TpagamentoCollection.GetItem(Index: Integer): TpagamentoCollectionItem;
begin
  Result := TpagamentoCollectionItem(inherited GetItem(Index));
end;

procedure TpagamentoCollection.SetItem(Index: Integer; Value: TpagamentoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TpagamentoCollection.New: TpagamentoCollectionItem;
begin
  Result := TpagamentoCollectionItem.Create;
  Self.Add(Result);
end;

{ TicmsCollection }

function TicmsCollection.GetItem(Index: Integer): TicmsCollectionItem;
begin
  Result := TicmsCollectionItem(inherited GetItem(Index));
end;

function TicmsCollection.New: TicmsCollectionItem;
begin
  Result := TicmsCollectionItem.Create;
  Self.Add(Result);
end;

procedure TicmsCollection.SetItem(Index: Integer; Value: TicmsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

end.
