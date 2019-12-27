{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2019 Daniel Simoes de Almeida               }
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

unit ACBrEDINotaFiscal;

{$I ACBr.inc}

interface

uses SysUtils, Classes, ACBrBase,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
    ACBrEDIClass, pediConversao, ACBrTxtClass, Contnrs ;

type

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

   TInfoTomador = class
   private
     FidRegistro    : String ;
     FRazao         : String ;
     FCNPJCPF       : String ;
     FIE            : String ;
     FEndereco      : String ;
     FBairro        : String ;
     FCidade        : String ;
     FcMunicipio    : String ;
     FCep           : String ;
     FUF            : String ;
     FTelefone      : String ;
     FFiller        : String ;
   public
     property idRegistro: String read FidRegistro write FidRegistro ;
     property Razao     : String read FRazao      write FRazao ;
     property CNPJCPF   : String read FCNPJCPF    write FCNPJCPF ;
     property IE        : String read FIE         write FIE ;
     property Endereco  : String read FEndereco   write FEndereco ;
     property Bairro    : String read FBairro     write FBairro ;
     property Cidade    : String read FCidade     write FCidade ;
     property cMunicipio: String read FcMunicipio write FcMunicipio ;
     property Cep       : String read FCep        write FCep ;
     property UF        : String read FUF         write FUF ;
     property Telefone  : String read FTelefone   write FTelefone ;
     property Filler    : String read FFiller     write FFiller ;
   end;

   { TValoresFrete Registro referente ao Cálculo efetuado do Frete a ser Cobrado }
   TCalculoFrete = class   // Registro 507 ou 333
   private
     FIdRegistro    : String ;
     FqVolumes      : Double ;
     FqPesoBruto    : Double ;
     FqPesoCubado   : Double ;
     FqPesoDensidade: Double ;
     FvTotFrete     : Currency ;
     FvFretePeso    : Currency ;
     FvFrete        : Currency ;
     FvAdValorem    : Currency ;
     FvSecCat       : Currency ;
     FvItrGris      : Currency ;
     FvDespacho     : Currency ;
     FvPedagio      : Currency ;
     FvAdemeGris    : Currency ;
     FvDespesas     : Currency ;
     FvBCIcms       : Currency ;
     FpAliqIcms     : Currency ;
     FvIcms         : Currency ;
     FST            : tSimNaoST ;
     FvBCIcmsST     : Currency ;
     FpAliqIcmsST   : Currency ;
     FvIcmsST       : Currency ;
     FvBcISS        : Currency ;
     FpAliqISS      : Currency ;
     FvISS          : Currency ;
     FvIR           : Currency ;
     FDireitoFiscal : tediDireitoFiscal ;
     FTipoImposto   : tediImposto ;
     FUFGerador     : String ;
     FFiller        : String ;
   public
     property IdRegistro    : String            read FIdRegistro     write FidRegistro ;
     property qVolumes      : Double            read FqVolumes       write FqVolumes ;
     property qPesoBruto    : Double            read FqPesoBruto     write FqPesoBruto ;
     property qPesoCubado   : Double            read FqPesoCubado    write FqPesoCubado ;
     property qPesoDensidade: Double            read FqPesoDensidade write FqPesoDensidade ;
     property vTotFrete     : Currency          read FvTotFrete      write FvTotFrete ;
     property vTotFretePeso : Currency          read FvFretePeso     write FvFretePeso ;
     property vFrete        : Currency          read FvFrete         write FvFrete ;
     property vAdValorem    : Currency          read FvAdValorem     write FvAdValorem ;
     property vSecCat       : Currency          read FvSecCat        write FvSecCat ;
     property vItrGris      : Currency          read FvItrGris       write FvItrGris ;
     property vDespacho     : Currency          read FvDespacho      write FvDespacho ;
     property vPedagio      : Currency          read FvPedagio       write FvPedagio ;
     property vAdemeGris    : Currency          read FvAdemeGris     write FvAdemeGris ;
     property vDespesas     : Currency          read FvDespesas      write FvDespesas ;
     property vBCIcms       : Currency          read FvBCIcms        write FvBCIcms ;
     property pAliqIcms     : Currency          read FpAliqIcms      write FpAliqIcms ;
     property vIcms         : Currency          read FvIcms          write FvIcms ;
     property ST            : tSimNaoST         read FST             write FST ;
     property vBCIcmsST     : Currency          read FvBCIcmsST      write FvBCIcmsST ;
     property pAliqIcmsST   : Currency          read FpAliqIcmsST    write FpAliqIcmsST ;
     property vIcmsST       : Currency          read FvIcmsST        write FvIcmsST ;
     property vBcISS        : Currency          read FvBcISS         write FvBcISS ;
     property pAliqISS      : Currency          read FpAliqISS       write FpAliqISS ;
     property vISS          : Currency          read FvISS           write FvISS ;
     property vIR           : Currency          read FvIR            write FvIR ;
     property DireitoFiscal : tediDireitoFiscal read FDireitoFiscal  write FDireitoFiscal ;
     property TipoImposto   : tediImposto       read FTipoImposto    write FTipoImposto ;
     property UFGerador     : String            read FUFGerador      write FUFGerador ;
     property Filler        : String            read FFiller         write FFiller ;
   end;

   { TValoresNF Registro referente aos Valores Informados na NF  }
   TValoresNF = class           // Registro 506 ou 333
   private
     FIdRegistro    : String ;
     FqVolumes      : Double ;
     FqPesoBruto    : Double ;
     FqPesoLiquido  : Double ;
     FqPesoCubado   : Double ;
     FqPesoDensidade: Double ;
     FIncideICMS    : tIncideICMS ;
     FSeguroEfetuado: tediSimNao ;
     FvCobrado      : Currency ;
     FvTotalNF      : Currency ;
     FvTotalSeguro  : Currency ;
     FvTotalDescto  : Currency ;
     FvOutraDespesas: Currency ;
     FvBCIcms       : Currency ;
     FvIcms         : Currency ;
     FvBCIcmsST     : Currency ;
     FvIcmsST       : Currency ;
     FvIcmsRetido   : Currency ;
     FvImpImportacao: Currency ;
     FvIPI          : Currency ;
     FvPIS          : Currency ;
     FvCofins       : Currency ;
     FvFrete        : Currency ;
     FvFretePeso    : Currency ;
     FvAdValorem    : Currency ;
     FvTaxas        : Currency ;
     FvIcmsFrete    : Currency ;
     FvIcmsFreteST  : Currency ;
     FvIssFrete     : Currency ;
     FFiller        : String ;
   public
     property IdRegistro    : String            read FIdRegistro     write FidRegistro ;
     property qVolumes      : Double            read FqVolumes       write FqVolumes ;
     property qPesoBruto    : Double            read FqPesoBruto     write FqPesoBruto ;
     property qPesoLiquido  : Double            read FqPesoLiquido   write FqPesoLiquido ;
     property qPesoCubado   : Double            read FqPesoCubado    write FqPesoCubado ;
     property qPesoDensidade: Double            read FqPesoDensidade write FqPesoDensidade ;
     property IncideICMS    : tIncideICMS       read FIncideICMS     write FIncideICMS ;
     property SeguroEfetuado: tediSimNao        read FSeguroEfetuado write FSeguroEfetuado ;
     property vCobrado      : Currency          read FvCobrado       write FvCobrado ;
     property vTotalNF      : Currency          read FvTotalNF       write FvTotalNF ;
     property vTotalSeguro  : Currency          read FvTotalSeguro   write FvTotalSeguro ;
     property vTotalDescto  : Currency          read FvTotalDescto   write FvTotalDescto ;
     property vOutraDespesas: Currency          read FvOutraDespesas write FvOutraDespesas ;
     property vBCIcms       : Currency          read FvBCIcms        write FvBCIcms ;
     property vIcms         : Currency          read FvIcms          write FvIcms ;
     property vBCIcmsST     : Currency          read FvBCIcmsST      write FvBCIcmsST ;
     property vIcmsST       : Currency          read FvIcmsST        write FvIcmsST ;
     property vIcmsRetido   : Currency          read FvIcmsRetido    write FvIcmsRetido ;
     property vImpImportacao: Currency          read FvImpImportacao write FvImpImportacao ;
     property vIPI          : Currency          read FvIPI           write FvIPI ;
     property vPIS          : Currency          read FvPIS           write FvPIS ;
     property vCofins       : Currency          read FvCofins        write FvCofins ;
     property vFrete        : Currency          read FvFrete         write FvFrete ;
     property vFretePeso    : Currency          read FvFretePeso     write FvFretePeso ;
     property vAdValorem    : Currency          read FvAdValorem     write FvAdValorem ;
     property vTaxas        : Currency          read FvTaxas         write FvTaxas ;
     property vIcmsFrete    : Currency          read FvIcmsFrete     write FvIcmsFrete ;
     property vIcmsFreteST  : Currency          read FvIcmsFreteST   write FvIcmsFreteST ;
     property vIssFrete     : Currency          read FvIssFrete      write FvIssFrete ;
     property Filler        : String            read FFiller         write FFiller ;
   end;

   { TInfoRedespacho Responsável pelo Redespacho Registro 514 ou 314 }
   TInfoRedespacho = class( TInfoTomador )
   private
     FAreaFrete     : String ;
   public
     property AreaFrete : String read FAreaFrete  write FAreaFrete ;
   end;

   { TLocalEntraga Registro do Local de Entrega da Mercadoria Registro 504 }
   TLocalEntrega = class( TInfoRedespacho )  // Apenas Versão 5.0
   private
     FTipo      : Integer ; // 1 - CNPJ/CGC  2 - CPF
     FTpComercio: String ;  // C - Comercial I - Indústria N - Não Contribuinte
     FcPais     : Integer ;
   public
     property Tipo      : Integer read FTipo        write FTipo ;
     property cPais     : Integer read FcPais       write FcPais ;
     property TpComercio: String  read FTpComercio  write FTpComercio ;
   end;

   { TCargas Registro para Identificação das Cargas Regsitro 508 ou 333 }
   TCargas = class
   private
     FIdRegistro: String ;
     FxMarca    : String ;
     FnroVolume : String ;
     FnroLacre  : String ;
     FFiller    : String ;
   public
     property IdRegistro: String     read FIdRegistro  write FIdRegistro ;
     property xMarca    : String     read FxMarca      write FxMarca ;
     property nroVolume : String     read FnroVolume   write FnroVolume ;
     property nroLacre  : String     read FnroLacre    write FnroLacre ;
     property Filler    : String     read FFiller      write FFiller ;
   end;

   { TIdCargas Registro de Identifacação da Carga }
   TIdCargas = class( TObjectList )
   private
     function  GetItem(Index: Integer): TCargas;
     procedure SetItem(Index: Integer; Value: TCargas);
   public
     function  New: TCargas;
     property  Items[Index: Integer]: TCargas read GetItem write SetItem; default;
   end;

   { TProdutos Registro dos Produtos Constantes na NF de Entrega Regsitro 511 ou 311 }
   TProdutos = class
   private
     FIdRegistro: String ;
     FqVolumes  : Double ;
     FxEspecie  : String ;
     FcItem     : String ;
     FxDescricao: String ;
     FCFOP      : Integer ;
     FnroLote   : String ;
     FdtValidade: TDateTime ;
     FxMarca    : String ;
     FnroVolume : String ;
     FnroLacre  : String ;
     FnroPedido : String ;
     FFiller    : String ;
   public
     property IdRegistro: String     read FIdRegistro  write FIdRegistro ;
     property qVolumes  : Double     read FqVolumes    write FqVolumes ;
     property xEspecie  : String     read FxEspecie    write FxEspecie ;
     property cItem     : String     read FcItem       write FcItem ;
     property xDescricao: String     read FxDescricao  write FxDescricao ;
     property CFOP      : Integer    read FCFOP        write FCFOP ;
     property nroLote   : String     read FnroLote     write FnroLote ;
     property dtValidade: TDateTime  read FdtValidade  write FdtValidade ;
     property xMarca    : String     read FxMarca      write FxMarca ;
     property nroVolume : String     read FnroVolume   write FnroVolume ;
     property nroLacre  : String     read FnroLacre    write FnroLacre ;
     property nroPedido : String     read FnroPedido   write FnroPedido ;
     property Filler    : String     read FFiller      write FFiller ;
   end;

   { TItensNF Registro de Insformações dos Produtos da NF }
   TItensNF = class( TObjectList )
   private
     function  GetItem(Index: Integer): TProdutos;
     procedure SetItem(Index: Integer; Value: TProdutos);
   public
     function  New: TProdutos;
     property  Items[Index: Integer]: TProdutos read GetItem write SetItem; default;
   end;

   { TEntrega Registro de local da Entrega }
   TEntrega = class
   private
     FIdRegistro     : String ;
     FCNPJEmissorNF1 : String ;
     FNomeEmissorNF1 : String ;
     FSerieNF1       : String ;
     FNumeroNF1      : Integer ;
     FCNPJEmissorNF2 : String ;
     FNomeEmissorNF2 : String ;
     FSerieNF2       : String ;
     FNumeroNF2      : Integer ;
     FCNPJEmissorNF3 : String ;
     FNomeEmissorNF3 : String ;
     FSerieNF3       : String ;
     FNumeroNF3      : Integer ;
     FFilEmissorCT   : String ;
     FSerieConhecto  : String ;
     FNumeroConhecto : String ;
     FCNPJContratante: String ;
     FFiller         : String ;
   public
     property IdRegistro     : String  read FIdRegistro      write FIdRegistro ;
     property CNPJEmissorNF1 : String  read FCNPJEmissorNF1  write FCNPJEmissorNF1 ;
     property NomeEmissorNF1 : String  read FNomeEmissorNF1  write FNomeEmissorNF1 ;
     property SerieNF1       : String  read FSerieNF1        write FSerieNF1 ;
     property NumeroNF1      : Integer read FNumeroNF1       write FNumeroNF1 ;
     property CNPJEmissorNF2 : String  read FCNPJEmissorNF2  write FCNPJEmissorNF2 ;
     property NomeEmissorNF2 : String  read FNomeEmissorNF2  write FNomeEmissorNF2 ;
     property SerieNF2       : String  read FSerieNF2        write FSerieNF2 ;
     property NumeroNF2      : Integer read FNumeroNF2       write FNumeroNF2 ;
     property CNPJEmissorNF3 : String  read FCNPJEmissorNF3  write FCNPJEmissorNF3 ;
     property NomeEmissorNF3 : String  read FNomeEmissorNF3  write FNomeEmissorNF3 ;
     property SerieNF3       : String  read FSerieNF3        write FSerieNF3 ;
     property NumeroNF3      : Integer read FNumeroNF3       write FNumeroNF3 ;
     property FilEmissorCT   : String  read FFilEmissorCT    write FFilEmissorCT ;
     property SerieConhecto  : String  read FSerieConhecto   write FSerieConhecto ;
     property NumeroConhecto : String  read FNumeroConhecto  write FNumeroConhecto ;
     property CNPJContratante: String  read FCNPJContratante write FCNPJContratante ;
     property Filler         : String  read FFiller          write FFiller ;
   end;

   { TInfoEntrega Informações do Local de Entrega Regsitro 509 ou 309 }
   TInfoEntrega = class( TObjectList )
   private
     function  GetItem(Index: Integer): TEntrega;
     procedure SetItem(Index: Integer; Value: TEntrega);
   public
     function  New: TEntrega;
     property  Items[Index: Integer]: TEntrega read GetItem write SetItem; default;
   end;

  { TNotasFis Registro de Notas Fiscais do Arquivo NOTAFIS Regsitro 505 ou 313 }
   TNotasFis = class( TNFCT )
   private
     FtpMercadoria     : String ;
     FxEspecie         : String ;
     FcRota            : String ;
     FMeioTransporte   : Integer ;
     FTipoTransporte   : Integer ;
     FTipoCarga        : Integer ;
     FCondicaoFrete    : String ;      // C - CIF  F - FOB
     FdtEmbarque       : TDateTime ;
     FCargaRapida      : tediSimNao ;
     FFreteDifer       : tediSimNao ;
     FTabelaFrete      : String ;
     FModalidadeFrete  : String ;
     FTipoEntrega      : Integer ;
     FdtInicio         : TDateTime ;
     FhrInicio         : TTime ;
     FdtTermino        : TDateTime ;
     FhrTermino        : TTime ;
     FcNFe             : String ;
     FchNFe            : String ;
     FProtocoloNFe     : String ;
     FPlaca            : String ;
     FAcao             : tpAcao ;
     FValoresNF        : TValoresNF ;          // Registro 506
     FCalculoFrete     : TCalculoFrete ;       // Registro 507
     FIdCargas         : TIdCargas ;           // Registro 508
     FInfoEntrega      : TInfoEntrega ;        // Registro 509
     FItensNF          : TItensNF ;            // Registro 511
     FInfoConsignatario: TInfoTomador ;        // Registro 513
     FInfoRedespacho   : TInfoRedespacho ;     // Registro 514
     FInfoTomador      : TInfoTomador ;        // Registro 515
   public
     property tpMercadoria   : String     read FtpMercadoria    write FtpMercadoria ;
     property xEspecie       : String     read FxEspecie        write FxEspecie ;
     property cRota          : String     read FcRota           write FcRota ;
     property MeioTransporte : Integer    read FMeioTransporte  write FMeioTransporte ;
     property TipoTransporte : Integer    read FTipoTransporte  write FTipoTransporte ;
     property TipoCarga      : Integer    read FTipoCarga       write FTipoCarga ;
     property CondicaoFrete  : String     read FCondicaoFrete   write FCondicaoFrete ;
     property dtEmbarque     : TDateTime  read FdtEmbarque      write FdtEmbarque ;
     property CargaRapida    : tediSimNao read FCargaRapida     write FCargaRapida ;
     property FreteDifer     : tediSimNao read FFreteDifer      write FFreteDifer ;
     property TabelaFrete    : String     read FTabelaFrete     write FTabelaFrete ;
     property ModalidadeFrete: String     read FModalidadeFrete write FModalidadeFrete ;
     property TipoEntrega    : Integer    read FTipoEntrega     write FTipoEntrega ;
     property dtInicio       : TDateTime  read FdtInicio        write FdtInicio ;
     property hrInicio       : TTime      read FhrInicio        write FhrInicio ;
     property dtTermino      : TDateTime  read FdtTermino       write FdtTermino ;
     property hrTermino      : TTime      read FhrTermino       write FhrTermino ;
     property cNFe           : String     read FcNFe            write FcNFe ;
     property chNFe          : String     read FchNFe           write FchNFe ;
     property ProtocoloNFe   : String     read FProtocoloNFe    write FProtocoloNFe ;
     property Placa          : String     read FPlaca           write FPlaca ;
     property Acao           : tpAcao     read FAcao            write FAcao ;

     property ValoresNF        : TValoresNF        read FValoresNF         write FValoresNF ;
     property CalculoFrete     : TCalculoFrete     read FCalculoFrete      write FCalculoFrete ;
     property IdCargas         : TIdCargas         read FIdCargas          write FIdCargas ;
     property InfoEntrega      : TInfoEntrega      read FInfoEntrega       write FInfoEntrega ;
     property ItensNF          : TItensNF          read FItensNF           write FItensNF ;
     property InfoConsignatario: TInfoTomador      read FInfoConsignatario write FInfoConsignatario ;
     property InfoRedespacho   : TInfoRedespacho   read FInfoRedespacho    write FInfoRedespacho ;
     property InfoTomador      : TInfoTomador      read FInfoTomador       write FInfoTomador ;

   end;

   { TInfoNotas Informações das Notas Fiscais contidas no arquivo }
   TInfoNotas = class( TObjectList )
   private
     function  GetItem(Index: Integer): TNotasFis;
     procedure SetItem(Index: Integer; Value: TNotasFis);
   public
     function  New: TNotasFis;
     property  Items[Index: Integer]: TNotasFis read GetItem write SetItem; default;
   end;

   { TDestinatario Registro do Destinatário da Mercadoria Registro 503 ou 312 }
   TDestinatario = class( TInfoRedespacho )
   private
     FTipo        : String ; // 1 - CNPJ/CGC  2 - CPF
     FTpComercio  : String ;  // C - Comercial I - Indústria N - Não Contribuinte
     FcPais       : String ;
     FInsSuframa  : String ;
     FLocalEntrega: TLocalEntrega ;
     FInfoNotas   : TInfoNotas ;
   public
     property Tipo        : String         read FTipo          write FTipo ;
     property cPais       : String         read FcPais         write FcPais ;
     property TpComercio  : String         read FTpComercio    write FTpComercio ;
     property InsSuframa  : String         read FInsSuframa    write FInsSuframa ;
     property LocalEntrega: TLocalEntrega  read FLocalEntrega  write FLocalEntrega ;
     property InfoNotas   : TInfoNotas     read FInfoNotas     write FInfoNotas ;
   end;

   TInfoDestinatario = class( TObjectList )
   private
     function  GetItem(Index: Integer): TDestinatario;
     procedure SetItem(Index: Integer; Value: TDestinatario);
   public
     function  New: TDestinatario;
     property  Items[Index: Integer]: TDestinatario read GetItem write SetItem; default;
   end;

   { TTotalNotas  Registo de Totalização do Arquivo Registro 519 ou 319}
   TTotalNotas = class
   private
     FIdRegistro: String ;
     FvTotal    : Currency ;
     FvSeguro   : Currency ;
     FqPesoBruto: Currency ;
     FqPesoDensi: Currency ;   // Versão 3.0 a 3.1
     FqVolumes  : Currency ;
     FvCobrado  : Currency ;   // Versão 3.0 a 3.1
     FqNotas    : Integer ;    // Versão 5.0
     FFiller    : String ;
   public
     property vTotal    : Currency read FvTotal     write FvTotal ;
     property vCobrado  : Currency read FvCobrado   write FvCobrado ;
     property qPesoBruto: Currency read FqPesoBruto write FqPesoBruto ;
     property qPesoDensi: Currency read FqPesoDensi write FqPesoDensi ;
     property qVolumes  : Currency read FqVolumes   write FqVolumes ;
     property qNotas    : Integer  read FqNotas     write FqNotas ;
     property IdRegistro: String   read FIdRegistro write FIdRegistro ;
     property vSeguro   : Currency read FvSeguro    write FvSeguro ;
     property Filler    : String   read FFiller     write FFiller ;
   end;

   { TInfoEmbarcadora Registro de Informações da Embarcadora Registro 501 ou 311 }
   TEmbarcadora = class( TInfoRedespacho )
   private
     FIEST            : String ;
     FIM              : String ;
     FdtEmbarque      : TDateTime ;
     FLocalColeta     : TInfoRedespacho ;
     FInfoDestinatario: TInfoDestinatario ;
   public
     constructor Create ; reintroduce ;
     destructor  Destroy ; override ;
     property IEST            : String            read FIEST             write FIEST ;
     property IM              : String            read FIM               write FIM ;
     property dtEmbarque      : TDateTime         read FdtEmbarque       write FdtEmbarque ;
     property LocalColeta     : TInfoRedespacho   read FLocalColeta      write FLocalColeta ;
     property InfoDestinatario: TInfoDestinatario read FInfoDestinatario write FInfoDestinatario ;
   end;

   { TIdentDocto Registros de Identificação do Documento }
   TInfoEmbarcadora = class( TObjectList )
   private
     function  GetItem(Index: Integer): TEmbarcadora;
     procedure SetItem(Index: Integer; Value: TEmbarcadora);
   public
     function  New: TEmbarcadora;
     property  Items[Index: Integer]: TEmbarcadora read GetItem write SetItem; default;
   end;

   { TDocto Identificação do Documento }
   TDocto = class
   private
     FIdRegistro     : String ;  // Identificador do Registro
     FIdDocto        : String ;
     FFiller         : String ;
     FInfoEmbarcadora: TInfoEmbarcadora ;
   public
     property IdRegistro     : String           read FIdRegistro      write FIdRegistro ;
     property IdDocto        : String           read FIdDocto         write FIdDocto ;
     property Filler         : String           read FFiller          write FFiller ;
     property InfoEmbarcadora: TInfoEmbarcadora read FInfoEmbarcadora write FInfoEmbarcadora ;
   end;

   { TIdentDocto Registros de Identificação do Documento }
   TInfoDocto = class(TObjectList)
   private
     function  GetItem(Index: Integer): TDocto;
     procedure SetItem(Index: Integer; Value: TDocto);
   public
     function  New: TDocto;
     property  Items[Index: Integer]: TDocto read GetItem write SetItem; default;
   end;

   TACBrEDINotaFiscais = class(TComponent)
   private
      FTxt        : TACBrTxtClass ;
      FVersao     : tveEdi ;
      FConteudo   : TStringList ;

      FCabecalho        : TCabecalhoEDI ;
      FInfoDocto        : TInfoDocto ;
      FTotalNotas       : TTotalNotas ;

      procedure LerCabecalho ;
      function  LerCabecalhoDocto   ( nRow: Integer ): Integer ;
      function  LerInfoEmbarcadora  ( Registro: TInfoEmbarcadora;  nRow: Integer ): Integer ;
      function  LerLocalColeta      ( Registro: TInfoRedespacho;   nRow: Integer ): Integer ;
      function  LerInfoDestinatario ( Registro: TInfoDestinatario; nRow: Integer ): Integer ;
      function  LerLocalEntrega     ( Registro: TLocalEntrega;     nRow: Integer ): Integer ;
      function  LerNotasFiscais     ( Registro: TInfoNotas;        nRow: Integer ): Integer ;
      function  LerValoresNF        ( Registro: TValoresNF;        nRow: Integer ): Integer ;
      function  LerCalculoFrete     ( Registro: TCalculoFrete;     nRow: Integer ): Integer ;
      function  LerIdCargas         ( Registro: TIdCargas;         nRow: Integer ): Integer ;
      function  LerInfoEntrega      ( Registro: TInfoEntrega;      nRow: Integer ): Integer ;
      function  LerItensNF          ( Registro: TItensNF;          nRow: Integer ): Integer ;
      function  LerInfoConsignatario( Registro: TInfoTomador;      nRow: Integer ): Integer ;
      function  LerInfoRedespacho   ( Registro: TInfoRedespacho;   nRow: Integer ): Integer ;
      function  LerInfoTomador      ( Registro: TInfoTomador;      nRow: Integer ): Integer ;
      function  LerTotalNotas       ( nRow: Integer ): Integer ;
   public
     constructor Create(AOwner: TComponent); Override ;
     destructor  Destroy; Override ;

     property Cabecalho : TCabecalhoEdi  read FCabecalho    write FCabecalho ;
     property InfoDocto : TInfoDocto     read FInfoDocto    write FInfoDocto ;
     property TotalNotas: TTotalNotas    read FTotalNotas   write FTotalNotas ;
     property Conteudo  : TStringList    read FConteudo     write FConteudo ;
     property Versao    : tveEdi         read FVersao       write FVersao ;

     procedure LerArquivo(const xArquivo: String);
     procedure LimpaRegistros ;
   end;

   procedure Register;

implementation

uses pcnAuxiliar, ACBrUtil ;

{$IFNDEF FPC}
 {$R ACBrEDINotaFiscais.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrEDI', [TACBrEDINotaFiscais]);
end;

{ TNotaFiscais }

procedure TACBrEDINotaFiscais.LerCabecalho;
var
  nRow: Integer ;
begin
  nRow     := 0 ;
  Cabecalho.IdRegistro   := Copy(Conteudo.Strings[nRow],  1,  3) ;
  Cabecalho.Remetente    := Copy(Conteudo.Strings[nRow],  4, 35) ;
  Cabecalho.Destinatario := Copy(Conteudo.Strings[nRow], 39, 35) ;
  Cabecalho.Data         := StringToDate(Copy(Conteudo.Strings[nRow], 74,  6)) ;
  Cabecalho.Hora         := StringToTime(Copy(Conteudo.Strings[nRow], 80,  4)) ;
  Cabecalho.Id           := Copy(Conteudo.Strings[nRow], 84, 12) ;
  case Versao of
     ve50: Cabecalho.Filler := Copy(Conteudo.Strings[nRow], 96, 225) ;
     else
       Cabecalho.Filler  := Copy(Conteudo.Strings[nRow], 96, 145) ;
  end;

  Inc(nRow) ;
  LerCabecalhoDocto( nRow ) ;
end;

function TACBrEDINotaFiscais.LerCabecalhoDocto( nRow: Integer): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '500', '310') ;

  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with InfoDocto.New do
    begin
      IdRegistro := Copy(Conteudo.Strings[nRow],  1,  3) ;
      IdDocto    := Copy(Conteudo.Strings[nRow],  4, 14) ;
      case Versao of
        ve50: Filler := Copy(Conteudo.Strings[nRow], 18, 303) ;
        else
          Filler := Copy(Conteudo.Strings[nRow], 18, 223) ;
      end;
      Inc(nRow) ;
      nRow := LerTotalNotas( LerInfoEmbarcadora( InfoEmbarcadora, nRow ) ) ;
    end;
  end ;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerItensNF( Registro: TItensNF; nRow: Integer ): Integer ;
var
  cReg: String ;
  nPos, I: Integer ;
begin
  cReg     := iif( Versao = ve50, '511', '314' );
  while Copy(Conteudo.Strings[nRow],1 ,3) = cReg do
  begin
    if Versao = ve50 then
    begin
      with Registro.New do
      begin
        IdRegistro := Copy(Conteudo.Strings[nRow],1 ,3) ;
        qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow],   4, 8)) ;
        xEspecie   := Copy(Conteudo.Strings[nRow],  12, 15) ;
        cItem      := Copy(Conteudo.Strings[nRow],  27, 20) ;
        xDescricao := Copy(Conteudo.Strings[nRow],  47, 50) ;
        CFOP       := StrToInt(Copy(Conteudo.Strings[nRow],  97,  4)) ;
        nroLote    := Copy(Conteudo.Strings[nRow], 101, 20) ;
        dtValidade := StringToDate(Copy(Conteudo.Strings[nRow], 121, 8)) ;
        xMarca     := Copy(Conteudo.Strings[nRow], 129, 50) ;
        nroVolume  := Copy(Conteudo.Strings[nRow], 179, 50) ;
        nroLacre   := Copy(Conteudo.Strings[nRow], 229, 50) ;
        nroPedido  := Copy(Conteudo.Strings[nRow], 279, 20) ;
        Filler     := Copy(Conteudo.Strings[nRow], 299, 22) ;
      end ;
    end
    else
    begin
      nPos := 4 ;
      for I := 1 to 4 do
      begin
        with Registro.New do
        begin
          qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow], nPos, 7)) ;
          xEspecie   := Copy(Conteudo.Strings[nRow],  nPos+ 7, 15) ;
          xDescricao := Copy(Conteudo.Strings[nRow],  nPos+22, 30) ;
          Filler     := Copy(Conteudo.Strings[nRow], 212, 29) ;
        end;
        nPos := nPos + 52 ;
      end;
    end;
    inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerValoresNF( Registro: TValoresNF; nRow: Integer ): Integer ;
var
  cReg: String ;
  Ok: Boolean ;
begin
  cReg := iif( Versao = ve50, '506', '313') ;
  if cReg = Copy(Conteudo.Strings[nRow], 1, 3) then
  begin
    Registro.IdRegistro     := Copy(Conteudo.Strings[nRow], 1, 3) ;
    if Versao = ve50 then
    begin
      Registro.qVolumes       := StringToDouble(Copy(Conteudo.Strings[nRow]  ,   4,  8)) ;
      Registro.qPesoBruto     := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  12,  9)) ;
      Registro.qPesoLiquido   := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  21,  9)) ;
      Registro.qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  30, 10)) ;
      Registro.qPesoCubado    := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  40, 10)) ;
      Registro.IncideICMS     := StrToIncideICMS(Ok, Copy(Conteudo.Strings[nRow],  50,  1)) ;
      Registro.SeguroEfetuado := StrToSimNaoEdi(Ok, Copy(Conteudo.Strings[nRow] ,  51,  1)) ;
      Registro.vCobrado       := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  52, 15)) ;
      Registro.vTotalNF       := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  67, 15)) ;
      Registro.vTotalSeguro   := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  82, 15)) ;
      Registro.vTotalDescto   := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  97, 15)) ;
      Registro.vOutraDespesas := StringToDouble(Copy(Conteudo.Strings[nRow]  , 112, 15)) ;
      Registro.vBCIcms        := StringToDouble(Copy(Conteudo.Strings[nRow]  , 127, 15)) ;
      Registro.vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow]  , 142, 15)) ;
      Registro.vBCIcmsST      := StringToDouble(Copy(Conteudo.Strings[nRow]  , 157, 15)) ;
      Registro.vIcmsST        := StringToDouble(Copy(Conteudo.Strings[nRow]  , 172, 15)) ;
      Registro.vIcmsRetido    := StringToDouble(Copy(Conteudo.Strings[nRow]  , 187, 15)) ;
      Registro.vImpImportacao := StringToDouble(Copy(Conteudo.Strings[nRow]  , 202, 15)) ;
      Registro.vIPI           := StringToDouble(Copy(Conteudo.Strings[nRow]  , 217, 15)) ;
      Registro.vPIS           := StringToDouble(Copy(Conteudo.Strings[nRow]  , 232, 15)) ;
      Registro.vCofins        := StringToDouble(Copy(Conteudo.Strings[nRow]  , 247, 15)) ;
      Registro.vFrete         := StringToDouble(Copy(Conteudo.Strings[nRow]  , 262, 15)) ;
      Registro.vIcmsFrete     := StringToDouble(Copy(Conteudo.Strings[nRow]  , 277, 13)) ;
      Registro.vIcmsFreteST   := StringToDouble(Copy(Conteudo.Strings[nRow]  , 290, 13)) ;
      Registro.vIssFrete      := StringToDouble(Copy(Conteudo.Strings[nRow]  , 303, 13)) ;
      Registro.Filler         := Copy(Conteudo.Strings[nRow]                 , 316,  5) ;
      inc(nRow) ;
    end
    else
    begin
      Registro.qVolumes       := StringToDouble(Copy(Conteudo.Strings[nRow]     ,  79,  7)) ;
      Registro.vTotalNF       := StringToDouble(Copy(Conteudo.Strings[nRow]     ,  86, 15)) ;
      Registro.qPesoBruto     := StringToDouble(Copy(Conteudo.Strings[nRow]     , 101,  7)) ;
      Registro.qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow]     , 108,  5)) ;
      Registro.IncideICMS     := StrToIncideICMS(Ok, Copy(Conteudo.Strings[nRow], 113,  1)) ;
      Registro.SeguroEfetuado := StrToSimNaoEdi(Ok, Copy(Conteudo.Strings[nRow] , 114,  1)) ;
      Registro.vTotalSeguro   := StringToDouble(Copy(Conteudo.Strings[nRow]     , 115, 15)) ;
      Registro.vCobrado       := StringToDouble(Copy(Conteudo.Strings[nRow]     , 130, 15)) ;
      Registro.vFretePeso     := StringToDouble(Copy(Conteudo.Strings[nRow]     , 153, 15)) ;
      Registro.vAdValorem     := StringToDouble(Copy(Conteudo.Strings[nRow]     , 168, 15)) ;
      Registro.vTaxas         := StringToDouble(Copy(Conteudo.Strings[nRow]     , 183, 15)) ;
      Registro.vFrete         := StringToDouble(Copy(Conteudo.Strings[nRow]     , 198, 15)) ;
      Registro.vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow]     , 214, 12)) ;
      Registro.vIcmsRetido    := StringToDouble(Copy(Conteudo.Strings[nRow]     , 226, 12)) ;
    end;
  end;
  result := nRow ;
end;

procedure TACBrEDINotaFiscais.LimpaRegistros;
begin
  InfoDocto.Clear ;
end;

function TACBrEDINotaFiscais.LerCalculoFrete( Registro: TCalculoFrete; nRow: Integer ): Integer ;
var
  cReg: String ;
  Ok: Boolean ;
begin
  cReg := iif( Versao = ve50, '507', '') ;
  if cReg = Copy(Conteudo.Strings[nRow], 1, 3) then
  begin
    Registro.IdRegistro     := Copy(Conteudo.Strings[nRow]                  ,   1,  3) ;
    Registro.qVolumes       := StringToDouble(Copy(Conteudo.Strings[nRow]    ,   4,  8)) ;
    Registro.qPesoBruto     := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  12,  9)) ;
    Registro.qPesoCubado    := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  21, 10)) ;
    Registro.qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  31, 10)) ;
    Registro.vTotFrete      := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  41, 15)) ;
    Registro.vTotFretePeso  := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  56, 15)) ;
    Registro.vFrete         := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  71, 15)) ;
    Registro.vAdValorem     := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  86, 15)) ;
    Registro.vSecCat        := StringToDouble(Copy(Conteudo.Strings[nRow]    , 101, 15)) ;
    Registro.vItrGris       := StringToDouble(Copy(Conteudo.Strings[nRow]    , 116, 15)) ;
    Registro.vDespacho      := StringToDouble(Copy(Conteudo.Strings[nRow]    , 131, 15)) ;
    Registro.vPedagio       := StringToDouble(Copy(Conteudo.Strings[nRow]    , 146, 15)) ;
    Registro.vAdemeGris     := StringToDouble(Copy(Conteudo.Strings[nRow]    , 161, 15)) ;
    Registro.vDespesas      := StringToDouble(Copy(Conteudo.Strings[nRow]    , 176, 15)) ;
    Registro.vBCIcms        := StringToDouble(Copy(Conteudo.Strings[nRow]    , 191, 15)) ;
    Registro.pAliqIcms      := StringToDouble(Copy(Conteudo.Strings[nRow]    , 206,  5)) ;
    Registro.vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow]    , 211, 15)) ;
    Registro.ST             := StrToSimNaoST(Ok, Copy(Conteudo.Strings[nRow], 226, 1)) ;
    Registro.vBCIcmsST      := StringToDouble(Copy(Conteudo.Strings[nRow]    , 227, 15)) ;
    Registro.pAliqIcmsST    := StringToDouble(Copy(Conteudo.Strings[nRow]    , 242,  5)) ;
    Registro.vIcmsST        := StringToDouble(Copy(Conteudo.Strings[nRow]    , 247, 15)) ;
    Registro.vBcISS         := StringToDouble(Copy(Conteudo.Strings[nRow]    , 262, 15)) ;
    Registro.pAliqISS       := StringToDouble(Copy(Conteudo.Strings[nRow]    , 277,  5)) ;
    Registro.vISS           := StringToDouble(Copy(Conteudo.Strings[nRow]    , 282, 15)) ;
    Registro.vIR            := StringToDouble(Copy(Conteudo.Strings[nRow]    , 297, 15)) ;
    Registro.DireitoFiscal  := StrToDireitoFiscal(Ok, Copy(Conteudo.Strings[nRow], 312,  3)) ;
    Registro.TipoImposto    := StrToTipoImposto(Ok, Copy(Conteudo.Strings[nRow]  , 314,  4)) ;
    Registro.UFGerador      := Copy(Conteudo.Strings[nRow]                  , 318,  2) ;
    Registro.Filler         := Copy(Conteudo.Strings[nRow]                  , 320,  1) ;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerIdCargas( Registro: TIdCargas; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '508', '') ;
  if cReg = Copy(Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro.New do
    begin
      IdRegistro := Copy(Conteudo.Strings[nRow],   1,   3) ;
      xMarca     := Copy(Conteudo.Strings[nRow],   4,  50) ;
      nroVolume  := Copy(Conteudo.Strings[nRow],  54,  50) ;
      nroLacre   := Copy(Conteudo.Strings[nRow], 104,  50) ;
      Filler     := Copy(Conteudo.Strings[nRow], 154, 167) ;
    end;
    inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoEmbarcadora( Registro: TInfoEmbarcadora; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '501', '311') ;
  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
      if Versao = ve50 then
      begin
        Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
        CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
        IE         := Copy(Conteudo.Strings[nRow],  68, 15) ;
        IEST       := Copy(Conteudo.Strings[nRow],  83, 15) ;
        IM         := Copy(Conteudo.Strings[nRow],  98, 15) ;
        Endereco   := Copy(Conteudo.Strings[nRow], 113, 50) ;
        Bairro     := Copy(Conteudo.Strings[nRow], 163, 35) ;
        Cidade     := Copy(Conteudo.Strings[nRow], 198, 35) ;
        Cep        := Copy(Conteudo.Strings[nRow], 233,  9) ;
        cMunicipio := Copy(Conteudo.Strings[nRow], 242,  9) ;
        UF         := Copy(Conteudo.Strings[nRow], 251,  9) ;
        dtEmbarque := StringTodate(Copy(Conteudo.Strings[nRow], 260,  8)) ;
        AreaFrete  := Copy(Conteudo.Strings[nRow], 268,  4) ;
        Telefone   := Copy(Conteudo.Strings[nRow], 272, 25) ;
        Filler     := Copy(Conteudo.Strings[nRow], 297, 24) ;
      end
      else
      begin
        CNPJCPF    := Copy(Conteudo.Strings[nRow],   4, 14) ;
        IE         := Copy(Conteudo.Strings[nRow],  18, 15) ;
        Endereco   := Copy(Conteudo.Strings[nRow],  33, 40) ;
        Cidade     := Copy(Conteudo.Strings[nRow],  73, 35) ;
        Cep        := Copy(Conteudo.Strings[nRow], 108,  9) ;
        UF         := Copy(Conteudo.Strings[nRow], 117,  9) ;
        dtEmbarque := StringToDate(Copy(Conteudo.Strings[nRow], 126,  8)) ;
        Razao      := Copy(Conteudo.Strings[nRow], 134, 40) ;
        Filler     := Copy(Conteudo.Strings[nRow], 174, 67) ;
      end;
      Inc(nRow) ;
      nRow := LerInfoDestinatario( InfoDestinatario, LerLocalColeta( LocalColeta, nRow ) ) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoEntrega( Registro: TInfoEntrega; nRow: Integer): Integer;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '509', '312' ) ;
  while Copy(Conteudo.Strings[nRow],1,3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro      := Copy(Conteudo.Strings[nRow],   1,  3) ;
      if Versao = ve50 then
      begin
        CNPJEmissorNF1  := Copy(Conteudo.Strings[nRow],   4, 14) ;
        NomeEmissorNF1  := Copy(Conteudo.Strings[nRow],  18, 50) ;
        SerieNF1        := Copy(Conteudo.Strings[nRow],  68,  3) ;
        NumeroNF1       := StrToInt(Copy(Conteudo.Strings[nRow],  71,  9)) ;
        CNPJEmissorNF2  := Copy(Conteudo.Strings[nRow],  80, 14) ;
        NomeEmissorNF2  := Copy(Conteudo.Strings[nRow],  94, 50) ;
        SerieNF2        := Copy(Conteudo.Strings[nRow], 144,  3) ;
        NumeroNF2       := StrToInt(Copy(Conteudo.Strings[nRow], 147,  9)) ;
        CNPJEmissorNF3  := Copy(Conteudo.Strings[nRow], 156, 14) ;
        NomeEmissorNF3  := Copy(Conteudo.Strings[nRow], 170, 50) ;
        SerieNF3        := Copy(Conteudo.Strings[nRow], 220,  3) ;
        NumeroNF3       := StrToInt(Copy(Conteudo.Strings[nRow], 223,  9)) ;
        FilEmissorCT    := Copy(Conteudo.Strings[nRow], 232, 10) ;
        SerieConhecto   := Copy(Conteudo.Strings[nRow], 242,  5) ;
        NumeroConhecto  := Copy(Conteudo.Strings[nRow], 247, 12) ;
        CNPJContratante := Copy(Conteudo.Strings[nRow], 259, 14) ;
        Filler          := Copy(Conteudo.Strings[nRow], 273, 48) ;
      end;
    end;
    inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerNotasFiscais( Registro: TInfoNotas; nRow: Integer ): Integer ;
var
  cReg: String ;
  Ok: Boolean ;
begin
  cReg := iif( Versao = ve50, '505', '313' ) ;
  while Copy(Conteudo.Strings[nRow],1,3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro        := Copy(Conteudo.Strings[nRow],  1,  3) ;
      if Versao = ve50 then
      begin
        xSerie          := Copy(Conteudo.Strings[nRow],  4,  3) ;
        xNumero         := Copy(Conteudo.Strings[nRow],  7,  9) ;
        dtEmissao       := StringToDate(Copy(Conteudo.Strings[nRow], 16, 8)) ;
        tpMercadoria    := Copy(Conteudo.Strings[nRow], 24, 15) ;
        xEspecie        := Copy(Conteudo.Strings[nRow], 39, 15) ;
        cRota           := Copy(Conteudo.Strings[nRow], 54,  7) ;
        MeioTransporte  := StrToInt(Copy(Conteudo.Strings[nRow], 61, 1)) ;
        TipoTransporte  := StrToInt(Copy(Conteudo.Strings[nRow], 62, 1)) ;
        TipoCarga       := StrToInt(Copy(Conteudo.Strings[nRow], 63, 1)) ;
        CondicaoFrete   := Copy(Conteudo.Strings[nRow], 64,  1) ;
        dtEmbarque      := StringToDate(Copy(Conteudo.Strings[nRow], 65, 8)) ;
        Desdobro        := Copy(Conteudo.Strings[nRow], 73, 10) ;
        CargaRapida     := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow], 83, 1)) ;
        TipoNF          := StrToInt(Copy(Conteudo.Strings[nRow], 84, 1)) ;
        Bonificacao     := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow], 85, 1)) ;
        CFOP            := Copy(Conteudo.Strings[nRow], 86, 4) ;
        UFGerador       := Copy(Conteudo.Strings[nRow], 90, 2) ;
        FreteDifer      := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow], 92, 1)) ;
        TabelaFrete     := Copy(Conteudo.Strings[nRow], 93, 10) ;
        ModalidadeFrete := Copy(Conteudo.Strings[nRow], 103, 2) ;
        IdPedido        := Copy(Conteudo.Strings[nRow], 105, 20) ;
        Romaneio        := Copy(Conteudo.Strings[nRow], 125, 20) ;
        NumeroSAP1      := Copy(Conteudo.Strings[nRow], 145, 20) ;
        NumeroSAP2      := Copy(Conteudo.Strings[nRow], 165, 20) ;
        NumeroSAP3      := Copy(Conteudo.Strings[nRow], 185, 20) ;
        TipoEntrega     := StrToInt(Copy(Conteudo.Strings[nRow], 205,  1)) ;
        dtInicio        := StringToDate(Copy(Conteudo.Strings[nRow], 206,  8) ) ;
        hrInicio        := StringToTime(Copy(Conteudo.Strings[nRow], 214,  4) ) ;
        dtTermino       := StringToDate(Copy(Conteudo.Strings[nRow], 218,  8) ) ;
        hrTermino       := StringToTime(Copy(Conteudo.Strings[nRow], 226,  4) ) ;
        cNFe            := Copy(Conteudo.Strings[nRow], 230,  9) ;
        chNFe           := Copy(Conteudo.Strings[nRow], 239, 45) ;
        ProtocoloNFe    := Copy(Conteudo.Strings[nRow], 284, 15) ;
        Acao            := StrToAcaoEdi(Ok, Copy(Conteudo.Strings[nRow], 299, 1)) ;
        Filler          := Copy(Conteudo.Strings[nRow], 300, 21) ;
      end
      else
      begin
        Romaneio        := Copy(Conteudo.Strings[nRow]                   ,   4, 15) ;
        cRota           := Copy(Conteudo.Strings[nRow]                   ,  19,  7) ;
        MeioTransporte  := StrToInt(Copy(Conteudo.Strings[nRow]          ,  26,  1)) ;
        TipoTransporte  := StrToInt(Copy(Conteudo.Strings[nRow]          ,  27,  1)) ;
        TipoCarga       := StrToInt(Copy(Conteudo.Strings[nRow]          ,  28,  1)) ;
        CondicaoFrete   := Copy(Conteudo.Strings[nRow]                   ,  29,  1) ;
        xSerie          := Copy(Conteudo.Strings[nRow]                   ,  30,  3) ;
        xNumero         := Copy(Conteudo.Strings[nRow]                   ,  33,  8) ;
        dtEmissao       := StringToDate(Copy(Conteudo.Strings[nRow]      ,  41,  8)) ;
        tpMercadoria    := Copy(Conteudo.Strings[nRow]                   ,  49, 15) ;
        xEspecie        := Copy(Conteudo.Strings[nRow]                   ,  64, 15) ;
        Placa           := Copy(Conteudo.Strings[nRow]                   , 145,  7) ;
        CargaRapida     := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow], 152, 1)) ;
        Acao            := StrToAcaoEdi(Ok, Copy(Conteudo.Strings[nRow]  , 213, 1)) ;
        Bonificacao     := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow], 238, 1)) ;
        Filler          := Copy(Conteudo.Strings[nRow]                   , 239, 21) ;
        LerValoresNF( ValoresNF, nRow ) ;
      end;
      Inc(nRow) ;
      nRow := LerInfoTomador( InfoTomador, LerInfoRedespacho   ( InfoRedespacho,
                                           LerInfoConsignatario( InfoConsignatario,
                                           LerItensNF          ( ItensNF,
                                           LerInfoEntrega      ( InfoEntrega,
                                           LerIdCargas         ( IdCargas,
                                           LerCalculoFrete     ( CalculoFrete,
                          iif(Versao=ve50, LerValoresNF        ( ValoresNF, nRow ), nRow) ) ) ) ) ) ) ) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerTotalNotas( nRow: Integer ): Integer ;
begin
  TotalNotas.IdRegistro := Copy(Conteudo.Strings[nRow],  1,  3) ;
  case Versao of
    ve30a,
    ve31 : begin
             TotalNotas.vTotal     := StringToDouble(Copy(Conteudo.Strings[nRow],  4, 15)) ;
             TotalNotas.qPesoBruto := StringToDouble(Copy(Conteudo.Strings[nRow], 19, 15)) ;
             TotalNotas.qPesoDensi := StringToDouble(Copy(Conteudo.Strings[nRow], 34, 15)) ;
             TotalNotas.qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow], 49, 15)) ;
             TotalNotas.vCobrado   := StringToDouble(Copy(Conteudo.Strings[nRow], 64, 15)) ;
             TotalNotas.vSeguro    := StringToDouble(Copy(Conteudo.Strings[nRow], 79, 15)) ;
             TotalNotas.Filler     := Copy(Conteudo.Strings[nRow], 94, 147) ;
           end;
    ve50 : begin
             TotalNotas.vTotal     := StringToDouble(Copy(Conteudo.Strings[nRow],  4, 15)) ;
             TotalNotas.qPesoBruto := StringToDouble(Copy(Conteudo.Strings[nRow], 19, 15)) ;
             TotalNotas.qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow], 34, 15)) ;
             TotalNotas.qNotas     := StrToInt(Copy(Conteudo.Strings[nRow], 49, 10)) ;
             TotalNotas.Filler     := Copy(Conteudo.Strings[nRow], 59, 262) ;
           end;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoTomador( Registro: TInfoTomador; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '515', '317' ) ;
  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    Registro.idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
    if Versao = ve50 then
    begin
      Registro.Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
      Registro.CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
      Registro.IE         := Copy(Conteudo.Strings[nRow],  68, 15) ;
      Registro.Endereco   := Copy(Conteudo.Strings[nRow],  83, 50) ;
      Registro.Bairro     := Copy(Conteudo.Strings[nRow], 133, 35) ;
      Registro.Cidade     := Copy(Conteudo.Strings[nRow], 168, 35) ;
      Registro.Cep        := Copy(Conteudo.Strings[nRow], 203,  9) ;
      Registro.cMunicipio := Copy(Conteudo.Strings[nRow], 212,  9) ;
      Registro.UF         := Copy(Conteudo.Strings[nRow], 221,  9) ;
      Registro.Telefone   := Copy(Conteudo.Strings[nRow], 230, 35) ;
      Registro.Filler     := Copy(Conteudo.Strings[nRow], 265, 56) ;
    end
    else
    begin
      Registro.Razao      := Copy(Conteudo.Strings[nRow],   4, 40) ;
      Registro.CNPJCPF    := Copy(Conteudo.Strings[nRow],  44, 14) ;
      Registro.IE         := Copy(Conteudo.Strings[nRow],  58, 15) ;
      Registro.Endereco   := Copy(Conteudo.Strings[nRow],  73, 40) ;
      Registro.Bairro     := Copy(Conteudo.Strings[nRow], 113, 20) ;
      Registro.Cidade     := Copy(Conteudo.Strings[nRow], 133, 35) ;
      Registro.Cep        := Copy(Conteudo.Strings[nRow], 168,  9) ;
      Registro.cMunicipio := Copy(Conteudo.Strings[nRow], 177,  9) ;
      Registro.UF         := Copy(Conteudo.Strings[nRow], 186,  9) ;
      Registro.Telefone   := Copy(Conteudo.Strings[nRow], 195, 35) ;
      Registro.Filler     := Copy(Conteudo.Strings[nRow], 230, 11) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoConsignatario( Registro: TInfoTomador; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '513', '315' ) ;
  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    Registro.idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
    if Versao = ve50 then
    begin
      Registro.Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
      Registro.CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
      Registro.IE         := Copy(Conteudo.Strings[nRow],  68, 15) ;
      Registro.Endereco   := Copy(Conteudo.Strings[nRow],  83, 50) ;
      Registro.Bairro     := Copy(Conteudo.Strings[nRow], 133, 35) ;
      Registro.Cidade     := Copy(Conteudo.Strings[nRow], 168, 35) ;
      Registro.Cep        := Copy(Conteudo.Strings[nRow], 203,  9) ;
      Registro.cMunicipio := Copy(Conteudo.Strings[nRow], 212,  9) ;
      Registro.UF         := Copy(Conteudo.Strings[nRow], 221,  9) ;
      Registro.Telefone   := Copy(Conteudo.Strings[nRow], 230, 35) ;
      Registro.Filler     := Copy(Conteudo.Strings[nRow], 265, 56) ;
    end
    else
    begin
      Registro.Razao      := Copy(Conteudo.Strings[nRow],   4, 40) ;
      Registro.CNPJCPF    := Copy(Conteudo.Strings[nRow],  44, 14) ;
      Registro.IE         := Copy(Conteudo.Strings[nRow],  58, 15) ;
      Registro.Endereco   := Copy(Conteudo.Strings[nRow],  73, 40) ;
      Registro.Bairro     := Copy(Conteudo.Strings[nRow], 113, 20) ;
      Registro.Cidade     := Copy(Conteudo.Strings[nRow], 133, 35) ;
      Registro.Cep        := Copy(Conteudo.Strings[nRow], 168,  9) ;
      Registro.cMunicipio := Copy(Conteudo.Strings[nRow], 177,  9) ;
      Registro.UF         := Copy(Conteudo.Strings[nRow], 186,  9) ;
      Registro.Telefone   := Copy(Conteudo.Strings[nRow], 195, 35) ;
      Registro.Filler     := Copy(Conteudo.Strings[nRow], 230, 11) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoDestinatario( Registro: TInfoDestinatario; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '503', '312') ;
  while Copy(Conteudo.Strings[nRow],   1,  3) = cReg do
  begin
    with Registro.New do
    begin
      idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
      if Versao = ve50 then
      begin
        Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
        CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
        IE         := Copy(Conteudo.Strings[nRow],  68, 15) ;
        InsSuframa := Copy(Conteudo.Strings[nRow],  83, 15) ;
        Endereco   := Copy(Conteudo.Strings[nRow],  98, 50) ;
        Bairro     := Copy(Conteudo.Strings[nRow], 148, 35) ;
        Cidade     := Copy(Conteudo.Strings[nRow], 183, 35) ;
        Cep        := Copy(Conteudo.Strings[nRow], 218,  9) ;
        cMunicipio := Copy(Conteudo.Strings[nRow], 227,  9) ;
        UF         := Copy(Conteudo.Strings[nRow], 236,  9) ;
        Telefone   := Copy(Conteudo.Strings[nRow], 245, 35) ;
        cPais      := Copy(Conteudo.Strings[nRow], 280,  4) ;
        AreaFrete  := Copy(Conteudo.Strings[nRow], 284,  4) ;
        Tipo       := Copy(Conteudo.Strings[nRow], 288,  1) ;
        TpComercio := Copy(Conteudo.Strings[nRow], 289,  1) ;
        Filler     := Copy(Conteudo.Strings[nRow], 290, 31) ;
      end
      else
      begin
        Razao      := Copy(Conteudo.Strings[nRow],   4, 40) ;
        CNPJCPF    := Copy(Conteudo.Strings[nRow],  44, 14) ;
        IE         := Copy(Conteudo.Strings[nRow],  58, 15) ;
        Endereco   := Copy(Conteudo.Strings[nRow],  73, 40) ;
        Bairro     := Copy(Conteudo.Strings[nRow], 113, 20) ;
        Cidade     := Copy(Conteudo.Strings[nRow], 133, 35) ;
        Cep        := Copy(Conteudo.Strings[nRow], 168,  9) ;
        cMunicipio := Copy(Conteudo.Strings[nRow], 177,  9) ;
        UF         := Copy(Conteudo.Strings[nRow], 186,  9) ;
        AreaFrete  := Copy(Conteudo.Strings[nRow], 195,  4) ;
        Telefone   := Copy(Conteudo.Strings[nRow], 199, 35) ;
        Tipo       := Copy(Conteudo.Strings[nRow], 234,  1) ;
        Filler     := Copy(Conteudo.Strings[nRow], 235,  6) ;
      end;
      Inc(nRow) ;
      nRow := LerNotasFiscais( InfoNotas, LerLocalEntrega( LocalEntrega, nRow ) ) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoRedespacho( Registro: TInfoRedespacho; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '514', '316' ) ;
  while cReg = Copy( Conteudo.Strings[nRow], 1, 3) do
  begin
    with Registro do
    begin
      idRegistro := Copy( Conteudo.Strings[nRow],   1,  3) ;
      if Versao = ve50 then
      begin
        Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
        CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
        IE         := Copy(Conteudo.Strings[nRow],  68, 15) ;
        Endereco   := Copy(Conteudo.Strings[nRow],  83, 50) ;
        Bairro     := Copy(Conteudo.Strings[nRow], 133, 35) ;
        Cidade     := Copy(Conteudo.Strings[nRow], 168, 35) ;
        Cep        := Copy(Conteudo.Strings[nRow], 203,  9) ;
        cMunicipio := Copy(Conteudo.Strings[nRow], 212,  9) ;
        UF         := Copy(Conteudo.Strings[nRow], 221,  9) ;
        Telefone   := Copy(Conteudo.Strings[nRow], 230, 35) ;
        AreaFrete  := Copy(Conteudo.Strings[nRow], 265,  4) ;
        Filler     := Copy(Conteudo.Strings[nRow], 269, 52) ;
      end
      else
      begin
        Razao      := Copy(Conteudo.Strings[nRow],   4, 40) ;
        CNPJCPF    := Copy(Conteudo.Strings[nRow],  44, 14) ;
        IE         := Copy(Conteudo.Strings[nRow],  58, 15) ;
        Endereco   := Copy(Conteudo.Strings[nRow],  73, 40) ;
        Bairro     := Copy(Conteudo.Strings[nRow], 113, 20) ;
        Cidade     := Copy(Conteudo.Strings[nRow], 133, 35) ;
        Cep        := Copy(Conteudo.Strings[nRow], 168,  9) ;
        cMunicipio := Copy(Conteudo.Strings[nRow], 177,  9) ;
        UF         := Copy(Conteudo.Strings[nRow], 186,  9) ;
        AreaFrete  := Copy(Conteudo.Strings[nRow], 195,  4) ;
        Telefone   := Copy(Conteudo.Strings[nRow], 199, 35) ;
        Filler     := Copy(Conteudo.Strings[nRow], 234,  7) ;
      end;
      Inc(nRow) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerLocalColeta( Registro: TInfoRedespacho; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '502', '' ) ;
  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    Registro.idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
    Registro.Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
    Registro.CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
    Registro.Endereco   := Copy(Conteudo.Strings[nRow],  68, 50) ;
    Registro.Bairro     := Copy(Conteudo.Strings[nRow], 118, 35) ;
    Registro.Cidade     := Copy(Conteudo.Strings[nRow], 153, 35) ;
    Registro.Cep        := Copy(Conteudo.Strings[nRow], 188,  9) ;
    Registro.cMunicipio := Copy(Conteudo.Strings[nRow], 197,  9) ;
    Registro.UF         := Copy(Conteudo.Strings[nRow], 206,  9) ;
    Registro.Telefone   := Copy(Conteudo.Strings[nRow], 215, 35) ;
    Registro.AreaFrete  := Copy(Conteudo.Strings[nRow], 250,  4) ;
    Registro.Filler     := Copy(Conteudo.Strings[nRow], 254, 67) ;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerLocalEntrega( Registro: TLocalEntrega; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := iif( Versao = ve50, '504', '' ) ;
  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    Registro.idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
    Registro.Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
    Registro.CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
    Registro.IE         := Copy(Conteudo.Strings[nRow],  68, 15) ;
    Registro.Endereco   := Copy(Conteudo.Strings[nRow],  83, 50) ;
    Registro.Bairro     := Copy(Conteudo.Strings[nRow], 133, 35) ;
    Registro.Cidade     := Copy(Conteudo.Strings[nRow], 168, 35) ;
    Registro.Cep        := Copy(Conteudo.Strings[nRow], 203,  9) ;
    Registro.cMunicipio := Copy(Conteudo.Strings[nRow], 212,  9) ;
    Registro.UF         := Copy(Conteudo.Strings[nRow], 221,  9) ;
    Registro.Telefone   := Copy(Conteudo.Strings[nRow], 230, 35) ;
    Registro.cPais      := StrToInt(Copy(Conteudo.Strings[nRow], 265,  4)) ;
    Registro.AreaFrete  := Copy(Conteudo.Strings[nRow], 269,  4) ;
    Registro.Tipo       := StrToInt(Copy(Conteudo.Strings[nRow], 273,  1)) ;
    Registro.TpComercio := Copy(Conteudo.Strings[nRow], 274,  1) ;
    Registro.Filler     := Copy(Conteudo.Strings[nRow], 275, 46) ;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

constructor TACBrEDINotaFiscais.Create(AOwner: TComponent);
begin
  inherited Create(AOwner) ;
  FTxt               := TACBrTxtClass.Create ;
  FConteudo          := TStringList.Create ;
  FCabecalho         := TCabecalhoEdi.Create ;
  FInfoDocto         := TInfoDocto.Create ;
  FTotalNotas        := TTotalNotas.Create ;

  FVersao            := ve50 ;
end;

destructor TACBrEDINotaFiscais.Destroy;
begin
  FTxt.Free ;
  FConteudo.Free ;
  FCabecalho.Free ;
  FInfoDocto.Free ;
  FTotalNotas.Free ;
  inherited ;
end;

procedure TACBrEDINotaFiscais.LerArquivo( const xArquivo: String );
begin
  if not FileExists( xArquivo ) then
    exit ;

  Conteudo.Clear ;

  Conteudo.LoadFromFile(xArquivo) ;
  LerCabecalho ;

  Conteudo.Clear ;
end;

{ TInfoDocto }

function TInfoDocto.New: TDocto;
begin
  Result := TDocto.Create;
  Result.FInfoEmbarcadora := TInfoEmbarcadora.Create ;
  Add(Result);
end;

function TInfoDocto.GetItem(Index: Integer): TDocto;
begin
  Result := TDocto(inherited GetItem(Index)) ;
end;

procedure TInfoDocto.SetItem(Index: Integer; Value: TDocto);
begin
  Put(Index, Value) ;
end;

{ TInfoEmbarcadora }

function TInfoEmbarcadora.GetItem(Index: Integer): TEmbarcadora;
begin
  Result := TEmbarcadora(inherited GetItem(Index)) ;
end;

function TInfoEmbarcadora.New: TEmbarcadora;
begin
  Result := TEmbarcadora.Create;
  Add(Result);
end;

procedure TInfoEmbarcadora.SetItem(Index: Integer; Value: TEmbarcadora);
begin
  Put(Index, Value) ;
end;

{ TEmbarcadora }

constructor TEmbarcadora.Create;
begin
  inherited Create ;
  FLocalColeta     := TInfoRedespacho.Create ;
  FInfoDestinatario:= TInfoDestinatario.Create ;
end;

destructor TEmbarcadora.Destroy;
begin
  FLocalColeta.Free ;
  FInfoDestinatario.Free ;
  inherited Destroy;
end;

{ TInfoEntrega }

function TInfoEntrega.GetItem(Index: Integer): TEntrega;
begin
  Result := TEntrega(inherited GetItem(Index)) ;
end;

function TInfoEntrega.New: TEntrega;
begin
  Result := TEntrega.Create;
  Add(Result);
end;

procedure TInfoEntrega.SetItem(Index: Integer; Value: TEntrega);
begin
  Put(Index, Value) ;
end;

{ TItensNF }

function TItensNF.GetItem(Index: Integer): TProdutos;
begin
  Result := TProdutos(inherited GetItem(Index)) ;
end;

function TItensNF.New: TProdutos;
begin
  Result := TProdutos.Create;
  Add(Result);
end;

procedure TItensNF.SetItem(Index: Integer; Value: TProdutos);
begin
  Put(Index, Value) ;
end;

{ TInfoNotas }

function TInfoNotas.New: TNotasFis;
begin
  Result := TNotasFis.Create ;
  Result.ValoresNF        := TValoresNF.Create ;
  Result.CalculoFrete     := TCalculoFrete.Create ;
  Result.IdCargas         := TIdCargas.Create ;
  Result.InfoEntrega      := TInfoEntrega.Create ;
  Result.ItensNF          := TItensNF.Create ;
  Result.InfoConsignatario:= TInfoTomador.Create ;
  Result.InfoRedespacho   := TInfoRedespacho.Create ;
  Result.InfoTomador      := TInfoTomador.Create ;
  Add(Result) ;
end;

function TInfoNotas.GetItem(Index: Integer): TNotasFis;
begin
  Result := TNotasFis(inherited GetItem(Index)) ;
end;

procedure TInfoNotas.SetItem(Index: Integer; Value: TNotasFis);
begin
  Put(Index, Value);
end;

{ TIdCargas }

function TIdCargas.GetItem(Index: Integer): TCargas;
begin
  Result := TCargas(inherited GetItem(Index)) ;
end;

function TIdCargas.New: TCargas;
begin
  Result := TCargas.Create ;
  Add(Result) ;
end;

procedure TIdCargas.SetItem(Index: Integer; Value: TCargas);
begin
  Put(Index, Value);
end;

{ TInfoDestinatario }

function TInfoDestinatario.GetItem(Index: Integer): TDestinatario;
begin
  Result := TDestinatario(inherited GetItem(Index)) ;
end;

function TInfoDestinatario.New: TDestinatario;
begin
  Result := TDestinatario.Create ;
  Result.FLocalEntrega := TLocalEntrega.Create ;
  Result.InfoNotas     := TInfoNotas.Create ;
  Add(Result) ;
end;

procedure TInfoDestinatario.SetItem(Index: Integer; Value: TDestinatario);
begin
  Put(Index, Value);
end;

end.
