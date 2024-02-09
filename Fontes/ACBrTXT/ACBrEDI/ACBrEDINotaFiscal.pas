{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

   { TComplementoNF Registro referente a Informções Complementares da NF  }
   // Registro 333 versão 3.1 ou  anterior
   TComplementoNF = class
     FIdRegistro     : String ;
     FCFOP           : String ;
     FTipoEntrega    : Integer ;
     FdtInicio       : TDateTime ;
     FhrInicio       : TTime ;
     FdtTermino      : TDateTime ;
     FhrTermino      : TTime ;
     FLocDesembarque : String ;
     FFreteDifer     : tediSimNao ;
     FTabelaFrete    : String ;
     FEmissorNF1     : String ;
     FSerieNF1       : String ;
     FNumeroNF1      : String ;
     FEmissorNF2     : String ;
     FSerieNF2       : String ;
     FNumeroNF2      : String ;
     FEmissorNF3     : String ;
     FSerieNF3       : String ;
     FNumeroNF3      : String ;
     FEmissorNF4     : String ;
     FSerieNF4       : String ;
     FNumeroNF4      : String ;
     FEmissorNF5     : String ;
     FSerieNF5       : String ;
     FNumeroNF5      : String ;
     FvDespesas      : Currency ;
     FTipoVeiculo    : tmTransporte ;
     FEmissorCTe     : String ;
     FSerieCTe       : String ;
     FNumeroCTE      : String ;
     FFiller         : String ;
   public
     property IdRegistro     : String        read FIdRegistro      write FidRegistro ;
     property CFOP           : String        read FCFOP            write FCFOP ;
     property TipoEntrega    : Integer       read FTipoEntrega     write FTipoEntrega ;
     property dtInicio       : TDateTime     read FdtInicio        write FdtInicio ;
     property hrInicio       : TTime         read FhrInicio        write FhrInicio ;
     property dtTermino      : TDateTime     read FdtTermino       write FdtTermino ;
     property hrTermino      : TTime         read FhrTermino       write FhrTermino ;
     property LocDesembarque : String        read FLocDesembarque  write FLocDesembarque ;
     property FreteDifer     : tediSimNao    read FFreteDifer      write FFreteDifer ;
     property TabelaFrete    : String        read FTabelaFrete     write FTabelaFrete ;
     property EmissorNF1     : String        read FEmissorNF1      write FEmissorNF1 ;
     property SerieNF1       : String        read FSerieNF1        write FSerieNF1 ;
     property NumeroNF1      : String        read FNumeroNF1       write FNumeroNF1 ;
     property EmissorNF2     : String        read FEmissorNF2      write FEmissorNF2 ;
     property SerieNF2       : String        read FSerieNF2        write FSerieNF2 ;
     property NumeroNF2      : String        read FNumeroNF2       write FNumeroNF2 ;
     property EmissorNF3     : String        read FEmissorNF3      write FEmissorNF3 ;
     property SerieNF3       : String        read FSerieNF3        write FSerieNF3 ;
     property NumeroNF3      : String        read FNumeroNF3       write FNumeroNF3 ;
     property EmissorNF4     : String        read FEmissorNF4      write FEmissorNF4 ;
     property SerieNF4       : String        read FSerieNF4        write FSerieNF4 ;
     property NumeroNF4      : String        read FNumeroNF4       write FNumeroNF4 ;
     property EmissorNF5     : String        read FEmissorNF5      write FEmissorNF5 ;
     property SerieNF5       : String        read FSerieNF5        write FSerieNF5 ;
     property NumeroNF5      : String        read FNumeroNF5       write FNumeroNF5 ;
     property vDespesas      : Currency      read FvDespesas       write FvDespesas ;
     property TipoVeiculo    : tmTransporte  read FTipoVeiculo     write FTipoVeiculo ;
     property EmissorCTe     : String        read FEmissorCTe      write FEmissorCTe ;
     property SerieCTe       : String        read FSerieCTe        write FSerieCTe ;
     property NumeroCTe      : String        read FNumeroCTe       write FNumeroCTe ;
     property Filler         : String        read FFiller          write FFiller ;
   end;

   { TValoresNF Registro referente aos Valores Informados na NF  }
   TValoresNF = class           // Registro 506 versao 5.0 ou Continuação 313 versão 3.1 ou  anterior
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
     FComplementoNF    : TComplementoNF ;      // Registro 333 Versão 3.1 ou anterior
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
     property ComplementoNF    : TComplementoNF    read FComplementoNF     write FComplementoNF ;

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
     FTotalNotas     : TTotalNotas ;
   public
     property IdRegistro     : String           read FIdRegistro      write FIdRegistro ;
     property IdDocto        : String           read FIdDocto         write FIdDocto ;
     property Filler         : String           read FFiller          write FFiller ;
     property InfoEmbarcadora: TInfoEmbarcadora read FInfoEmbarcadora write FInfoEmbarcadora ;
     property TotalNotas     : TTotalNotas      read FTotalNotas      write FTotalNotas ;
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

   {$IFDEF RTL230_UP}
   [ComponentPlatformsAttribute(piacbrAllPlatforms)]
   {$ENDIF RTL230_UP}
   TACBrEDINotaFiscais = class(TACBrComponent)
   private
      FTxt        : TACBrTxtClass ;
      FVersao     : tveEdi ;

      FConteudo   : TStringList ;
      FCabecalho  : TCabecalhoEDI ;
      FInfoDocto  : TInfoDocto ;

      procedure GerarCabecalho ;
      procedure GerarCabecalhoDocto   ( Registro: TInfoDocto ) ;
      procedure GerarInfoEmbarcadora  ( Registro: TInfoEmbarcadora ) ;
      procedure GerarLocalColeta      ( Registro: TInfoRedespacho ) ;
      procedure GerarInfoDestinatario ( Registro: TInfoDestinatario );
      procedure GerarLocalEntrega     ( Registro: TLocalEntrega ) ;
      procedure GerarNotasFiscais     ( Registro: TInfoNotas ) ;
      procedure GerarValoresNF        ( Registro: TValoresNF ) ;
      procedure GerarComplementoNF    ( Registro: TComplementoNF ) ;
      procedure GerarCalculoFrete     ( Registro: TCalculoFrete ) ;
      procedure GerarIdCargas         ( Registro: TIdCargas ) ;
      procedure GerarInfoEntrega      ( Registro: TInfoEntrega ) ;
      procedure GerarItensNF          ( Registro: TItensNF ) ;
      procedure GerarInfoConsignatario( Registro: TInfoTomador ) ;
      procedure GerarInfoRedespacho   ( Registro: TInfoRedespacho ) ;
      procedure GerarInfoTomador      ( Registro: TInfoTomador ) ;
      procedure GerarTotalNotas       ( Registro: TTotalNotas ) ;

      procedure LerCabecalho ;
      function  LerCabecalhoDocto   ( nRow: Integer ): Integer ;
      function  LerInfoEmbarcadora  ( Registro: TInfoEmbarcadora;  nRow: Integer ): Integer ;
      function  LerLocalColeta      ( Registro: TInfoRedespacho;   nRow: Integer ): Integer ;
      function  LerInfoDestinatario ( Registro: TInfoDestinatario; nRow: Integer ): Integer ;
      function  LerLocalEntrega     ( Registro: TLocalEntrega;     nRow: Integer ): Integer ;
      function  LerNotasFiscais     ( Registro: TInfoNotas;        nRow: Integer ): Integer ;
      function  LerValoresNF        ( Registro: TValoresNF;        nRow: Integer ): Integer ;
      function  LerComplementoNF    ( Registro: TComplementoNF;    nRow: Integer ): Integer ;
      function  LerCalculoFrete     ( Registro: TCalculoFrete;     nRow: Integer ): Integer ;
      function  LerIdCargas         ( Registro: TIdCargas;         nRow: Integer ): Integer ;
      function  LerInfoEntrega      ( Registro: TInfoEntrega;      nRow: Integer ): Integer ;
      function  LerItensNF          ( Registro: TItensNF;          nRow: Integer ): Integer ;
      function  LerInfoConsignatario( Registro: TInfoTomador;      nRow: Integer ): Integer ;
      function  LerInfoRedespacho   ( Registro: TInfoRedespacho;   nRow: Integer ): Integer ;
      function  LerInfoTomador      ( Registro: TInfoTomador;      nRow: Integer ): Integer ;
      function  LerTotalNotas       ( Registro: TTotalNotas;       nRow: Integer ): Integer ;
   public
     constructor Create(AOwner: TComponent); Override ;
     destructor  Destroy; Override ;

     property Cabecalho : TCabecalhoEdi  read FCabecalho    write FCabecalho ;
     property InfoDocto : TInfoDocto     read FInfoDocto    write FInfoDocto ;
     property Conteudo  : TStringList    read FConteudo     write FConteudo ;
     property Versao    : tveEdi         read FVersao       write FVersao ;

     procedure GravarArquivo(const xArquivo: String);
     procedure LerArquivo(const xArquivo: String);

     procedure LimpaRegistros ;
   end;

implementation

uses
  StrUtils,
  Math,
  ACBrUtil.Strings;

{ TNotaFiscais }

procedure TACBrEDINotaFiscais.LerCabecalho;
var
  nRow: Integer ;
begin
  nRow     := 0 ;
  with Cabecalho do
  begin
    IdRegistro   := Copy(Conteudo.Strings[nRow],  1,  3) ;
    Remetente    := Copy(Conteudo.Strings[nRow],  4, 35) ;
    Destinatario := Copy(Conteudo.Strings[nRow], 39, 35) ;
    Data         := StringToDate(Copy(Conteudo.Strings[nRow], 74,  6)) ;
    Hora         := StringToTime(Copy(Conteudo.Strings[nRow], 80,  4)) ;
    Id           := Copy(Conteudo.Strings[nRow], 84, 12) ;
    case Versao of
      ve50: Filler := Copy(Conteudo.Strings[nRow], 96, 225) ;
      ve31: Filler := Copy(Conteudo.Strings[nRow], 96, 145) ;
      else
      begin
        CNPJTransp := Copy(Conteudo.Strings[nRow],  960, 14) ;
        Filler     := Copy(Conteudo.Strings[nRow], 110, 135) ;
      end;
    end;
  end;

  Inc(nRow) ;
  LerCabecalhoDocto( nRow ) ;
end;

function TACBrEDINotaFiscais.LerCabecalhoDocto( nRow: Integer): Integer ;
var
  cReg: String ;
begin
  // Registros 500 ou 310
  cReg := IfThen( Versao = ve50, '500', '310') ;
  if Copy(Conteudo.Strings[nRow],1,3) <> cReg then
    raise Exception.Create('Erro: Registro Indentioficação do Arquivo não informado...,'+#13+
                           'Este registro é Obrigatório !!' );

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
      nRow := LerTotalNotas( TotalNotas, LerInfoEmbarcadora( InfoEmbarcadora, nRow ) ) ;
    end;
    if nRow > Conteudo.Count - 1 then
      Break ;
  end ;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoEmbarcadora( Registro: TInfoEmbarcadora; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  // Registros 501 ou 311
  cReg := IfThen( Versao = ve50, '501', '311') ;
  if Copy(Conteudo.Strings[nRow],1,3) <> cReg then
    raise Exception.Create('Erro: Registro Informações da Embarcadora não informado...,'+#13+
                           'Este registro é Obrigatório !!' );

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

function TACBrEDINotaFiscais.LerLocalColeta( Registro: TInfoRedespacho; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  // Registro  502 apenas versão  5.0
  cReg := IfThen( Versao = ve50, '502', '' ) ;
  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro do
    begin
      idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
      Razao      := Copy(Conteudo.Strings[nRow],   4, 50) ;
      CNPJCPF    := Copy(Conteudo.Strings[nRow],  54, 14) ;
      Endereco   := Copy(Conteudo.Strings[nRow],  68, 50) ;
      Bairro     := Copy(Conteudo.Strings[nRow], 118, 35) ;
      Cidade     := Copy(Conteudo.Strings[nRow], 153, 35) ;
      Cep        := Copy(Conteudo.Strings[nRow], 188,  9) ;
      cMunicipio := Copy(Conteudo.Strings[nRow], 197,  9) ;
      UF         := Copy(Conteudo.Strings[nRow], 206,  9) ;
      Telefone   := Copy(Conteudo.Strings[nRow], 215, 35) ;
      AreaFrete  := Copy(Conteudo.Strings[nRow], 250,  4) ;
      Filler     := Copy(Conteudo.Strings[nRow], 254, 67) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoDestinatario( Registro: TInfoDestinatario; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  // Registros 503 ou 312
  cReg := IfThen( Versao = ve50, '503', '312') ;
  if Copy(Conteudo.Strings[nRow],1,3) <> cReg then
    raise Exception.Create('Erro: Registro Informações do Destinatário não informado...,'+#13+
                           'Este registro é Obrigatório !!' );

  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
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

function TACBrEDINotaFiscais.LerLocalEntrega( Registro: TLocalEntrega; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  // Registros 504 apenas versão 5.0
  cReg := IfThen( Versao = ve50, '504', '' ) ;

  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro do
    begin
      idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
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
      cPais      := StrToInt(Copy(Conteudo.Strings[nRow], 265,  4)) ;
      AreaFrete  := Copy(Conteudo.Strings[nRow], 269,  4) ;
      Tipo       := StrToInt(Copy(Conteudo.Strings[nRow], 273,  1)) ;
      TpComercio := Copy(Conteudo.Strings[nRow], 274,  1) ;
      Filler     := Copy(Conteudo.Strings[nRow], 275, 46) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerNotasFiscais( Registro: TInfoNotas; nRow: Integer ): Integer ;
var
  cReg: String ;
  Ok: Boolean ;
begin
  // Registros 505 ou 313
  cReg := IfThen( Versao = ve50, '505', '313' ) ;
  if Copy(Conteudo.Strings[nRow],1,3) <> cReg then
    raise Exception.Create('Erro: Registro Informações das Notas não informado...,'+#13+
                           'Este registro é Obrigatório !!' );

  while Copy(Conteudo.Strings[nRow],1,3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro        := Copy(Conteudo.Strings[nRow],  1,  3) ;
      if Versao = ve50 then
      begin
        xSerie          := Copy(Conteudo.Strings[nRow]                   ,   4,  3) ;
        xNumero         := Copy(Conteudo.Strings[nRow]                   ,   7,  9) ;
        dtEmissao       := StringToDate(Copy(Conteudo.Strings[nRow]      ,  16,  8)) ;
        tpMercadoria    := Copy(Conteudo.Strings[nRow]                   ,  24, 15) ;
        xEspecie        := Copy(Conteudo.Strings[nRow]                   ,  39, 15) ;
        cRota           := Copy(Conteudo.Strings[nRow]                   ,  54,  7) ;
        MeioTransporte  := StrToInt(Copy(Conteudo.Strings[nRow]          ,  61,  1)) ;
        TipoTransporte  := StrToInt(Copy(Conteudo.Strings[nRow]          ,  62,  1)) ;
        TipoCarga       := StrToInt(Copy(Conteudo.Strings[nRow]          ,  63,  1)) ;
        CondicaoFrete   := Copy(Conteudo.Strings[nRow]                   ,  64,  1) ;
        dtEmbarque      := StringToDate(Copy(Conteudo.Strings[nRow]      ,  65,  8)) ;
        Desdobro        := Copy(Conteudo.Strings[nRow]                   ,  73, 10) ;
        CargaRapida     := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow],  83,  1)) ;
        TipoNF          := StrToInt(Copy(Conteudo.Strings[nRow]          ,  84,  1)) ;
        Bonificacao     := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow],  85,  1)) ;
        CFOP            := Copy(Conteudo.Strings[nRow]                   ,  86,  4) ;
        UFGerador       := Copy(Conteudo.Strings[nRow]                   ,  90,  2) ;
        FreteDifer      := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow],  92,  1)) ;
        TabelaFrete     := Copy(Conteudo.Strings[nRow]                   ,  93, 10) ;
        ModalidadeFrete := Copy(Conteudo.Strings[nRow]                   , 103,  2) ;
        IdPedido        := Copy(Conteudo.Strings[nRow]                   , 105, 20) ;
        Romaneio        := Copy(Conteudo.Strings[nRow]                   , 125, 20) ;
        NumeroSAP1      := Copy(Conteudo.Strings[nRow]                   , 145, 20) ;
        NumeroSAP2      := Copy(Conteudo.Strings[nRow]                   , 165, 20) ;
        NumeroSAP3      := Copy(Conteudo.Strings[nRow]                   , 185, 20) ;
        TipoEntrega     := StrToInt(Copy(Conteudo.Strings[nRow]          , 205,  1) ) ;
        dtInicio        := StringToDate(Copy(Conteudo.Strings[nRow]      , 206,  8) ) ;
        hrInicio        := StringToTime(Copy(Conteudo.Strings[nRow]      , 214,  4) ) ;
        dtTermino       := StringToDate(Copy(Conteudo.Strings[nRow]      , 218,  8) ) ;
        hrTermino       := StringToTime(Copy(Conteudo.Strings[nRow]      , 226,  4) ) ;
        cNFe            := Copy(Conteudo.Strings[nRow]                   , 230,  9) ;
        chNFe           := Copy(Conteudo.Strings[nRow]                   , 239, 45) ;
        ProtocoloNFe    := Copy(Conteudo.Strings[nRow]                   , 284, 15) ;
        Acao            := StrToAcaoEdi(Ok, Copy(Conteudo.Strings[nRow]  , 299, 1)) ;
        Filler          := Copy(Conteudo.Strings[nRow]                   , 300, 21) ;
        Inc(nRow) ;
      end
      else
      begin
        Romaneio                 := Copy(Conteudo.Strings[nRow]                   ,   4, 15) ;
        cRota                    := Copy(Conteudo.Strings[nRow]                   ,  19,  7) ;
        MeioTransporte           := StrToInt(Copy(Conteudo.Strings[nRow]          ,  26,  1)) ;
        TipoTransporte           := StrToInt(Copy(Conteudo.Strings[nRow]          ,  27,  1)) ;
        TipoCarga                := StrToInt(Copy(Conteudo.Strings[nRow]          ,  28,  1)) ;
        CondicaoFrete            := Copy(Conteudo.Strings[nRow]                   ,  29,  1) ;
        xSerie                   := Copy(Conteudo.Strings[nRow]                   ,  30,  3) ;
        xNumero                  := Copy(Conteudo.Strings[nRow]                   ,  33,  8) ;
        dtEmissao                := StringToDate(Copy(Conteudo.Strings[nRow]      ,  41,  8)) ;
        tpMercadoria             := Copy(Conteudo.Strings[nRow]                   ,  49, 15) ;
        xEspecie                 := Copy(Conteudo.Strings[nRow]                   ,  64, 15) ;
        ValoresNF.qVolumes       := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  79,  7), 5, 2) ;
        ValoresNF.vTotalNF       := StringToDouble(Copy(Conteudo.Strings[nRow]    ,  86, 15), 13, 2) ;
        ValoresNF.qPesoBruto     := StringToDouble(Copy(Conteudo.Strings[nRow]    , 101,  7), 5, 2) ;
        ValoresNF.qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow]    , 108,  5), 3, 2) ;
        ValoresNF.IncideICMS     := StrToIncideICMS(Ok,Copy(Conteudo.Strings[nRow], 113,  1)) ;
        ValoresNF.SeguroEfetuado := StrToSimNaoEdi(Ok,Copy(Conteudo.Strings[nRow] , 114,  1)) ;
        ValoresNF.vTotalSeguro   := StringToDouble(Copy(Conteudo.Strings[nRow]    , 115, 15), 13, 2) ;
        ValoresNF.vCobrado       := StringToDouble(Copy(Conteudo.Strings[nRow]    , 130, 15), 13, 2) ;
        Placa                    := Copy(Conteudo.Strings[nRow]                   , 145,  7) ;
        CargaRapida              := StrToSimNaoEDI(Ok,Copy(Conteudo.Strings[nRow] , 152, 1)) ;
        ValoresNF.vFretePeso     := StringToDouble(Copy(Conteudo.Strings[nRow]    , 153, 15), 13, 2) ;
        ValoresNF.vAdValorem     := StringToDouble(Copy(Conteudo.Strings[nRow]    , 168, 15), 13, 2) ;
        ValoresNF.vTaxas         := StringToDouble(Copy(Conteudo.Strings[nRow]    , 183, 15), 13, 2) ;
        ValoresNF.vFrete         := StringToDouble(Copy(Conteudo.Strings[nRow]    , 198, 15), 13, 2) ;
        Acao                     := StrToAcaoEdi(Ok,Copy(Conteudo.Strings[nRow]   , 213, 1)) ;
        ValoresNF.vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow]    , 214, 12), 10, 2) ;
        ValoresNF.vIcmsRetido    := StringToDouble(Copy(Conteudo.Strings[nRow]    , 226, 12), 10, 2) ;
        Bonificacao              := StrToSimNaoEDI(Ok,Copy(Conteudo.Strings[nRow] , 238, 1)) ;
        Filler                   := Copy(Conteudo.Strings[nRow]                   , 239, 21) ;
        nRow                     := LerComplementoNF( ComplementoNF, nRow ) ;
      end;
      nRow := LerInfoTomador( InfoTomador, LerInfoRedespacho   ( InfoRedespacho,
                                           LerInfoConsignatario( InfoConsignatario,
                                           LerItensNF          ( ItensNF,
                                           LerInfoEntrega      ( InfoEntrega,
                                           LerIdCargas         ( IdCargas,
                                           LerCalculoFrete     ( CalculoFrete,
                       IfThen( Versao = ve50, LerValoresNF        ( ValoresNF, nRow ), nRow) ) ) ) ) ) ) ) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerValoresNF( Registro: TValoresNF; nRow: Integer ): Integer ;
var
  cReg: String ;
  Ok: Boolean ;
begin
  // Registro 506 versão 5.0 ou 313 continuação para versão 3.10 ou anterior
  cReg := '506' ;
  if cReg = Copy(Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro do
    begin
      IdRegistro     := Copy(Conteudo.Strings[nRow], 1, 3) ;
      qVolumes       := StringToDouble(Copy(Conteudo.Strings[nRow]  ,   4,  8), 6, 2) ;
      qPesoBruto     := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  12,  9), 6, 3) ;
      qPesoLiquido   := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  21,  9), 6, 3) ;
      qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  30, 10), 6, 4) ;
      qPesoCubado    := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  40, 10), 6, 4) ;
      IncideICMS     := StrToIncideICMS(Ok, Copy(Conteudo.Strings[nRow],  50,  1)) ;
      SeguroEfetuado := StrToSimNaoEdi(Ok, Copy(Conteudo.Strings[nRow] ,  51,  1)) ;
      vCobrado       := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  52, 15), 13, 2) ;
      vTotalNF       := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  67, 15), 13, 2) ;
      vTotalSeguro   := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  82, 15), 13, 2) ;
      vTotalDescto   := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  97, 15), 13, 2) ;
      vOutraDespesas := StringToDouble(Copy(Conteudo.Strings[nRow]  , 112, 15), 13, 2) ;
      vBCIcms        := StringToDouble(Copy(Conteudo.Strings[nRow]  , 127, 15), 13, 2) ;
      vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow]  , 142, 15), 13, 2) ;
      vBCIcmsST      := StringToDouble(Copy(Conteudo.Strings[nRow]  , 157, 15), 13, 2) ;
      vIcmsST        := StringToDouble(Copy(Conteudo.Strings[nRow]  , 172, 15), 13, 2) ;
      vIcmsRetido    := StringToDouble(Copy(Conteudo.Strings[nRow]  , 187, 15), 13, 2) ;
      vImpImportacao := StringToDouble(Copy(Conteudo.Strings[nRow]  , 202, 15), 13, 2) ;
      vIPI           := StringToDouble(Copy(Conteudo.Strings[nRow]  , 217, 15), 13, 2) ;
      vPIS           := StringToDouble(Copy(Conteudo.Strings[nRow]  , 232, 15), 13, 2) ;
      vCofins        := StringToDouble(Copy(Conteudo.Strings[nRow]  , 247, 15), 13, 2) ;
      vFrete         := StringToDouble(Copy(Conteudo.Strings[nRow]  , 262, 15), 13, 2) ;
      vIcmsFrete     := StringToDouble(Copy(Conteudo.Strings[nRow]  , 277, 13), 11, 2) ;
      vIcmsFreteST   := StringToDouble(Copy(Conteudo.Strings[nRow]  , 290, 13), 11, 2) ;
      vIssFrete      := StringToDouble(Copy(Conteudo.Strings[nRow]  , 303, 13), 11, 2) ;
      Filler         := Copy(Conteudo.Strings[nRow]                 , 316,  5) ;
      inc(nRow) ;
    end;
  end
  else
    raise Exception.Create('Erro: Registro com Valores da Nota Fiscal não Informado...,'+#13+
                           'Este registro é Obrigatório !!'  );
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerComplementoNF( Registro: TComplementoNF; nRow: Integer ): Integer ;
var
  cReg: String ;
  Ok: Boolean ;
begin
  // Registro 333 Complemento da Nota Fiscal para versão 3.10 ou anterior
  cReg := '333' ;
  if cReg = Copy(Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro do
    begin
      IdRegistro     := Copy(Conteudo.Strings[nRow]                         ,   1,  3) ;
      CFOP           := Copy(Conteudo.Strings[nRow]                         ,   4,  4) ;
      TipoEntrega    := StrToInt(Copy(Conteudo.Strings[nRow]                ,   8,  1) ) ;
      dtInicio       := StringToDate(Copy(Conteudo.Strings[nRow]            ,   9,  8) ) ;
      hrInicio       := StringToTime(Copy(Conteudo.Strings[nRow]            ,  17,  4) ) ;
      dtTermino      := StringToDate(Copy(Conteudo.Strings[nRow]            ,  21,  8) ) ;
      hrTermino      := StringToTime(Copy(Conteudo.Strings[nRow]            ,  29,  4) ) ;
      LocDesembarque := Copy(Conteudo.Strings[nRow]                         ,  33, 15) ;
      FreteDifer     := StrToSimNaoEDI(Ok, Copy(Conteudo.Strings[nRow]      ,  48,  1)) ;
      TabelaFrete    := Copy(Conteudo.Strings[nRow]                         ,  49, 10) ;
      EmissorNF1     := Copy(Conteudo.Strings[nRow]                         ,  59, 15) ;
      SerieNF1       := Copy(Conteudo.Strings[nRow]                         ,  74,  3) ;
      NumeroNF1      := Copy(Conteudo.Strings[nRow]                         ,  77,  8) ;
      EmissorNF2     := Copy(Conteudo.Strings[nRow]                         ,  85, 15) ;
      SerieNF2       := Copy(Conteudo.Strings[nRow]                         , 100,  3) ;
      NumeroNF2      := Copy(Conteudo.Strings[nRow]                         , 103,  8) ;
      EmissorNF3     := Copy(Conteudo.Strings[nRow]                         , 111, 15) ;
      SerieNF3       := Copy(Conteudo.Strings[nRow]                         , 126,  3) ;
      NumeroNF3      := Copy(Conteudo.Strings[nRow]                         , 129,  8) ;
      EmissorNF4     := Copy(Conteudo.Strings[nRow]                         , 137, 15) ;
      SerieNF4       := Copy(Conteudo.Strings[nRow]                         , 152,  3) ;
      NumeroNF4      := Copy(Conteudo.Strings[nRow]                         , 155,  8) ;
      EmissorNF5     := Copy(Conteudo.Strings[nRow]                         , 163, 15) ;
      SerieNF5       := Copy(Conteudo.Strings[nRow]                         , 178,  3) ;
      NumeroNF5      := Copy(Conteudo.Strings[nRow]                         , 181,  8) ;
      vDespesas      := StringToDouble(Copy(Conteudo.Strings[nRow]          , 189, 15), 13, 2) ;
      TipoVeiculo    := StrToMeioTransporte( ok, Copy(Conteudo.Strings[nRow], 262, 15)) ;
      EmissorCTe     := Copy(Conteudo.Strings[nRow]                         , 209, 10) ;
      SerieCTe       := Copy(Conteudo.Strings[nRow]                         , 219,  5) ;
      NumeroCTe      := Copy(Conteudo.Strings[nRow]                         , 224, 12) ;
      Filler         := Copy(Conteudo.Strings[nRow]                         , 236,  5) ;
      inc(nRow) ;
    end;
  end
  else
    raise Exception.Create('Erro: Registro com Complemento da Nota Fiscal não Informado...,'+#13+
                           'Este registro é Obrigatório !!'  );
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerCalculoFrete( Registro: TCalculoFrete; nRow: Integer ): Integer ;
var
  cReg: String ;
  Ok: Boolean ;
begin
  cReg := IfThen( Versao = ve50, '507', '') ;
  if cReg = Copy(Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro do
    begin
      IdRegistro     := Copy(Conteudo.Strings[nRow]                  ,   1,  3) ;
      qVolumes       := StringToDouble(Copy(Conteudo.Strings[nRow]   ,   4,  8),  6, 2) ;
      qPesoBruto     := StringToDouble(Copy(Conteudo.Strings[nRow]   ,  12,  9),  6, 3) ;
      qPesoCubado    := StringToDouble(Copy(Conteudo.Strings[nRow]   ,  21, 10),  6, 4) ;
      qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow]   ,  31, 10),  6, 4) ;
      vTotFrete      := StringToDouble(Copy(Conteudo.Strings[nRow]   ,  41, 15), 13, 2) ;
      vTotFretePeso  := StringToDouble(Copy(Conteudo.Strings[nRow]   ,  56, 15), 13, 2) ;
      vFrete         := StringToDouble(Copy(Conteudo.Strings[nRow]   ,  71, 15), 13, 2) ;
      vAdValorem     := StringToDouble(Copy(Conteudo.Strings[nRow]   ,  86, 15), 13, 2) ;
      vSecCat        := StringToDouble(Copy(Conteudo.Strings[nRow]   , 101, 15), 13, 2) ;
      vItrGris       := StringToDouble(Copy(Conteudo.Strings[nRow]   , 116, 15), 13, 2) ;
      vDespacho      := StringToDouble(Copy(Conteudo.Strings[nRow]   , 131, 15), 13, 2) ;
      vPedagio       := StringToDouble(Copy(Conteudo.Strings[nRow]   , 146, 15), 13, 2) ;
      vAdemeGris     := StringToDouble(Copy(Conteudo.Strings[nRow]   , 161, 15), 13, 2) ;
      vDespesas      := StringToDouble(Copy(Conteudo.Strings[nRow]   , 176, 15), 13, 2) ;
      vBCIcms        := StringToDouble(Copy(Conteudo.Strings[nRow]   , 191, 15), 13, 2) ;
      pAliqIcms      := StringToDouble(Copy(Conteudo.Strings[nRow]   , 206,  5),  3, 2) ;
      vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow]   , 211, 15), 13, 2) ;
      ST             := StrToSimNaoST(Ok, Copy(Conteudo.Strings[nRow], 226, 1)) ;
      vBCIcmsST      := StringToDouble(Copy(Conteudo.Strings[nRow]   , 227, 15), 13, 2) ;
      pAliqIcmsST    := StringToDouble(Copy(Conteudo.Strings[nRow]   , 242,  5),  3, 2) ;
      vIcmsST        := StringToDouble(Copy(Conteudo.Strings[nRow]   , 247, 15), 13, 2) ;
      vBcISS         := StringToDouble(Copy(Conteudo.Strings[nRow]   , 262, 15), 13, 2) ;
      pAliqISS       := StringToDouble(Copy(Conteudo.Strings[nRow]   , 277,  5),  3, 2) ;
      vISS           := StringToDouble(Copy(Conteudo.Strings[nRow]   , 282, 15), 13, 2) ;
      vIR            := StringToDouble(Copy(Conteudo.Strings[nRow]   , 297, 15), 13, 2) ;
      DireitoFiscal  := StrToDireitoFiscal(Ok, Copy(Conteudo.Strings[nRow], 312,  3)) ;
      TipoImposto    := StrToTipoImposto(Ok, Copy(Conteudo.Strings[nRow]  , 314,  4)) ;
      UFGerador      := Copy(Conteudo.Strings[nRow]                  , 318,  2) ;
      Filler         := Copy(Conteudo.Strings[nRow]                  , 320,  1) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerIdCargas( Registro: TIdCargas; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := IfThen( Versao = ve50, '508', '') ;
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

function TACBrEDINotaFiscais.LerInfoEntrega( Registro: TInfoEntrega; nRow: Integer): Integer;
var
  cReg: String ;
begin
  cReg := IfThen( Versao = ve50, '509', '312' ) ;
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

function TACBrEDINotaFiscais.LerItensNF( Registro: TItensNF; nRow: Integer ): Integer ;
var
  cReg: String ;
  nPos, I: Integer ;
begin
  cReg     := IfThen( Versao = ve50, '511', '314' );
  while Copy(Conteudo.Strings[nRow],1 ,3) = cReg do
  begin
    if Versao = ve50 then
    begin
      with Registro.New do
      begin
        IdRegistro := Copy(Conteudo.Strings[nRow],1 ,3) ;
        qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow],   4, 8), 6, 2) ;
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
          qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow], nPos, 7), 5, 2) ;
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

procedure TACBrEDINotaFiscais.LimpaRegistros;
begin
  InfoDocto.Clear ;
end;

function TACBrEDINotaFiscais.LerInfoConsignatario( Registro: TInfoTomador; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := IfThen( Versao = ve50, '513', '315' ) ;
  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro do
    begin
      idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
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
        Filler     := Copy(Conteudo.Strings[nRow], 265, 56) ;
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
        Telefone   := Copy(Conteudo.Strings[nRow], 195, 35) ;
        Filler     := Copy(Conteudo.Strings[nRow], 230, 11) ;
      end;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerInfoRedespacho( Registro: TInfoRedespacho; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := IfThen( Versao = ve50, '514', '316' ) ;
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

function TACBrEDINotaFiscais.LerInfoTomador( Registro: TInfoTomador; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := IfThen( Versao = ve50, '515', '317' ) ;
  if cReg = Copy( Conteudo.Strings[nRow], 1, 3) then
  begin
    with Registro do
    begin
      idRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
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
        Filler     := Copy(Conteudo.Strings[nRow], 265, 56) ;
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
        Telefone   := Copy(Conteudo.Strings[nRow], 195, 35) ;
        Filler     := Copy(Conteudo.Strings[nRow], 230, 11) ;
      end;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDINotaFiscais.LerTotalNotas( Registro: TTotalNotas; nRow: Integer ): Integer ;
var
  cReg: string ;
begin
  // Registros 519 ou 318
  cReg := IfThen( Versao = ve50, '519', '318') ;
  if Copy(Conteudo.Strings[nRow], 1, 3) <> cReg then
    raise Exception.Create('Erro: Registro Totalizador das Notas não informado...,'+#13+
                           'Este registro é Obrigatório !!' );
  with Registro do
  begin
    IdRegistro := Copy(Conteudo.Strings[nRow],  1,  3) ;
    case Versao of
      ve50: begin
              vTotal     := StringToDouble(Copy(Conteudo.Strings[nRow],  4, 15), 13, 2) ;
              qPesoBruto := StringToDouble(Copy(Conteudo.Strings[nRow], 19, 15), 13, 2) ;
              qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow], 34, 15), 13, 2) ;
              qNotas     := StrToInt(trim(Copy(Conteudo.Strings[nRow] , 49, 10))) ;
              Filler     := Copy(Conteudo.Strings[nRow]               , 59, 262) ;
            end;
      else  begin
              vTotal     := StringToDouble(Copy(Conteudo.Strings[nRow],  4, 15), 13, 2) ;
              qPesoBruto := StringToDouble(Copy(Conteudo.Strings[nRow], 19, 15), 13, 2) ;
              qPesoDensi := StringToDouble(Copy(Conteudo.Strings[nRow], 34, 15), 13, 2) ;
              qVolumes   := StringToDouble(Copy(Conteudo.Strings[nRow], 49, 15), 13, 2) ;
              vCobrado   := StringToDouble(Copy(Conteudo.Strings[nRow], 64, 15), 13, 2) ;
              vSeguro    := StringToDouble(Copy(Conteudo.Strings[nRow], 79, 15), 13, 2) ;
              Filler     := Copy(Conteudo.Strings[nRow]               , 94, 147) ;
            end;
    end;
  end;
  inc(nRow) ;
  result := nRow ;
end;

constructor TACBrEDINotaFiscais.Create(AOwner: TComponent);
begin
  inherited Create(AOwner) ;
  FTxt               := TACBrTxtClass.Create ;
  FConteudo          := TStringList.Create ;
  FCabecalho         := TCabecalhoEdi.Create ;
  FInfoDocto         := TInfoDocto.Create ;

  FVersao            := ve50 ;
end;

destructor TACBrEDINotaFiscais.Destroy;
begin
  FTxt.Free ;
  FConteudo.Free ;
  FCabecalho.Free ;
  FInfoDocto.Free ;
  inherited ;
end;

procedure TACBrEDINotaFiscais.GravarArquivo( const xArquivo: String ) ;
begin
  Conteudo.Clear ;

  GerarCabecalho ;
  Conteudo.SaveToFile( xArquivo ) ;

  Conteudo.Clear ;
end;

procedure TACBrEDINotaFiscais.LerArquivo( const xArquivo: String );
begin
  if not FileExists(xArquivo) then
    raise Exception.Create('Erro: Arquivo especificado não encontrado !!');

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
  Result.FTotalNotas      := TTotalNotas.Create ;
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
  Result.ComplementoNF    := TComplementoNF.Create ;
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

procedure TACBrEDINotaFiscais.GerarCabecalho ;
var
  tam: Integer ;
  texto: string ;
begin
  // Registro 000 Cabeçalho do Arquivo NotaFis

  case Versao of
     ve50: begin
             if Cabecalho.Id = '' then
                Cabecalho.Id := 'NOT50'+
                    Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                    FTxt.LFill(Cabecalho.Sequencia, 3) ;
             tam := 225 ;
           end
     else  begin
             if Cabecalho.Id = '' then
                Cabecalho.Id := 'NOT' +
                     FTxt.LFill(Cabecalho.Data, 'ddmmyy', false) +
                     Copy(OnlyNumber(TimeToStr(FCabecalho.Hora)),1,4)      +
                     IntToStr(Cabecalho.Sequencia) ;
             tam  := IfThen( Versao = ve31, 145, 135) ;
           end;
  end;

  texto := Cabecalho.IdRegistro + FTxt.RFill(Cabecalho.Remetente,35) +
                                  FTxt.RFill(Cabecalho.Destinatario,35) +
                                  FTxt.LFill(Cabecalho.Data, 'ddmmyy', false) +
                                  FTxt.RFill(OnlyNumber(TimeToStr(Cabecalho.Hora)),4) +
                                  FTxt.RFill(Cabecalho.Id,12) ;

  if (Versao = ve30) or (Versao = ve30a) then
    texto := texto + FTxt.RFill(Cabecalho.CNPJTransp, 14) ;

  Conteudo.Add( texto + FTxt.RFill(Cabecalho.Filler, tam) ) ;

  GerarCabecalhoDocto( InfoDocto ) ;
end;

procedure TACBrEDINotaFiscais.GerarCabecalhoDocto( Registro: TInfoDocto ) ;
var
  i: integer ;
begin
  // Registro 500 ou 310 Identificação do Arquivo Nota Fiscal
  if Registro.Count = 0 then
    raise Exception.Create('Erro: Nenhuma indentificação do arquivo encontrada...,'+#13+
                           'Estas informções são obrigatórias !!' );

  for i := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[i] do
    begin
      case Versao of
        ve50: if IdDocto = '' then
                IdDocto := 'NOTAS50'+
                   Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                   FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
        else  if IdDocto = '' then
                IdDocto := 'NOTFI' +
                   Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                   FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
      end;
      Conteudo.Add( IdRegistro +
                FTxt.RFill(IdDocto, 14) +
                FTxt.RFill(Filler, IfThen( Versao = ve50, 303, 223)) ) ;

      GerarInfoEmbarcadora( InfoEmbarcadora ) ;

      GerarTotalNotas( TotalNotas ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarInfoEmbarcadora( Registro: TInfoEmbarcadora ) ;
var
  i: integer ;
begin
  // Registro 501 ou 311 Informações da Embarcadora
  if Registro.Count = 0 then
    raise Exception.Create('Erro: Nenhuma informação da embarcadora encontrada...,'+#13+
                           'Estas informções são obrigatórias !!' );

  for i := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[i] do
    begin
      Conteudo.Add( IdRegistro +
                    IfThen( Versao = ve50, FTxt.RFill(Razao, 50), '' ) +

                  FTxt.RFill(OnlyNumber(CNPJCPF), 14) +
                  FTxt.RFill(OnlyNumber(IE)     , 15) +

                  IfThen( Versao = ve50, FTxt.RFill(OnlyNumber(IEST), 15) +
                                      FTxt.RFill(IM              , 15), '') +

                  IfThen( Versao = ve50, FTxt.RFill(Endereco, 50), FTxt.RFill(Endereco, 40) ) +

                  IfThen( Versao = ve50, FTxt.RFill(Bairro, 35), '') +

                  FTxt.RFill(Cidade             , 35) +
                  FTxt.LFill(Cep                ,  9) +

                  IfThen( Versao = ve50, FTxt.LFill(cMunicipio,  9), '') +

                  FTxt.RFill(UF                 ,  9) +
                  FTxt.LFill(dtEmbarque         , 'ddmmyyyy', false) +

                  IfThen( Versao = ve50, FTxt.RFill(AreaFrete,  4) +
                                      FTxt.RFill(Telefone , 25), '') +

                  IfThen( Versao = ve50, '', FTxt.RFill(Razao, 40)) +

                  FTxt.RFill(Filler, IfThen( Versao = ve50, 24, 67) ) ) ;

      GerarLocalColeta( LocalColeta ) ;
      GerarInfoDestinatario( InfoDestinatario ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarLocalColeta( Registro: TInfoRedespacho ) ;
begin
  // Registro 502 Local de Coleta versão 5.0
  if Registro.idRegistro <> '' then
  begin
    with Registro do
    begin
      Conteudo.Add( IdRegistro + FTxt.RFill(Razao, 50) +
                    FTxt.RFill(OnlyNumber(CNPJCPF), 14) +
                    FTxt.RFill(Endereco, 50) +
                    FTxt.RFill(Bairro, 35) +
                    FTxt.RFill(Cidade, 35) +
                    FTxt.LFill(Cep,  9) +
                    FTxt.LFill(cMunicipio,  9) +
                    FTxt.RFill(UF,  9) +
                    FTxt.RFill(Telefone, 35) +
                    FTxt.RFill(AreaFrete,  4) +
                    FTxt.RFill(Filler, 67) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarInfoDestinatario( Registro: TInfoDestinatario ) ;
var
  i: Integer ;
begin
  // Registros 503 ou 312 Informações do Destinatário
  if Registro.Count = 0 then
    raise Exception.Create('Erro: Nenhuma informação do destinatário encontrada...,'+#13+
                           'Estas informções são obrigatórias !!' );

  for I := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[i] do
    begin
      Conteudo.Add( idRegistro +
                    FTxt.RFill(Razao, IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(OnlyNumber(CNPJCPF), 14) +
                    FTxt.RFill(OnlyNumber(IE), 15) +

                    IfThen( Versao = ve50, FTxt.RFill(InsSuframa, 15), '') +

                    FTxt.RFill(Endereco, IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(Bairro, IfThen( Versao = ve50, 35, 20)) +
                    FTxt.RFill(Cidade, 35) +
                    FTxt.LFill(Cep,  9) +
                    FTxt.LFill(cMunicipio,  9) +
                    FTxt.RFill(UF,  9) +

                    IfThen( Versao = ve50, FTxt.RFill(Telefone , 35)   +
                                        FTxt.LFill(cPais    ,  4)   +
                                        FTxt.RFill(AreaFrete,  4)   ,
                                        FTxt.RFill(AreaFrete,  4)   +
                                        FTxt.RFill(Telefone , 35) ) +

                    FTxt.LFill(Tipo,  1) +

                    IfThen( Versao = ve50, FTxt.RFill(TpComercio,  1), '') +

                    FTxt.RFill(Filler, IfThen( Versao = ve50, 31, 6) )  ) ;
      GerarLocalEntrega( LocalEntrega ) ;
      GerarNotasFiscais( InfoNotas ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarLocalEntrega( Registro: TLocalEntrega ) ;
begin
  // Registro 504 Local de Entrega versão 5.0
  if Registro.idRegistro <> '' then
  begin
    with Registro do
    begin
      Conteudo.Add( IdRegistro + FTxt.RFill(Razao , 50) +
                    FTxt.RFill(OnlyNumber(CNPJCPF), 14) +
                    FTxt.RFill(IE                 , 15) +
                    FTxt.RFill(Endereco           , 50) +
                    FTxt.RFill(Bairro             , 35) +
                    FTxt.RFill(Cidade             , 35) +
                    FTxt.LFill(Cep                ,  9) +
                    FTxt.LFill(cMunicipio         ,  9) +
                    FTxt.RFill(UF                 ,  9) +
                    FTxt.RFill(Telefone           , 35) +
                    FTxt.LFill(cPais              ,  4) +
                    FTxt.RFill(AreaFrete          ,  4) +
                    FTxt.LFill(Tipo               ,  1) +
                    FTxt.RFill(TpComercio         ,  1) +
                    FTxt.RFill(Filler             , 46) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarNotasFiscais( Registro: TInfoNotas ) ;
var
  i: Integer ;
  xTexto, xInicio, xTermino: String ;
begin
  // Registros 505 ou 313 Notas Fiscais
  if Registro.Count = 0 then
    raise Exception.Create('Erro: Nenhuma informação de nota fiscal encontrada...,'+#13+
                           'Estas informções são obrigatórias !!' );

  for i := 0 to Registro.Count - 1 do
  begin
    xTexto := '' ;
    with Registro.Items[i] do
    begin
      xInicio  := Copy(TimeToStr(hrInicio),1,5) ;
      xTermino := Copy(TimeToStr(hrTermino),1,5) ;
      xTexto := xTexto + IdRegistro ;
      if Versao = ve50 then
      begin
        xTexto := xTexto +
                  FTxt.RFill(xSerie         ,  3) +
                  FTxt.LFill(xNumero        ,  9) +
                  FTxt.LFill(dtEmissao      , 'ddmmyyyy', false) +
                  FTxt.RFill(tpMercadoria   , 15) +
                  FTxt.RFill(xEspecie       , 15) +
                  FTxt.RFill(cRota          ,  7) +
                  FTxt.LFill(MeioTransporte , 1) +
                  FTxt.LFill(TipoTransporte , 1) +
                  FTxt.LFill(TipoCarga      , 1) +
                  FTxt.LFill(CondicaoFrete  ,  1) +
                  FTxt.LFill(dtEmbarque     , 'ddmmyyyy', false) +
                  FTxt.RFill(Desdobro       , 10) +
                  FTxt.RFill(SimNaoEdiToStr(CargaRapida), 1) +
                  FTxt.LFill(TipoNF         , 1) +
                  FTxt.RFill(SimNaoEdiToStr(Bonificacao), 1) +
                  FTxt.RFill(CFOP           , 4) +
                  FTxt.RFill(UFGerador      , 2) +
                  FTxt.RFill(SimNaoEdiToStr(FreteDifer), 1) +
                  FTxt.RFill(TabelaFrete    , 10) +
                  FTxt.RFill(ModalidadeFrete, 2) +
                  FTxt.RFill(IdPedido       , 20) +
                  FTxt.RFill(Romaneio       , 20) +
                  FTxt.RFill(NumeroSAP1     , 20) +
                  FTxt.RFill(NumeroSAP2     , 20) +
                  FTxt.RFill(NumeroSAP3     , 20) +
                  FTxt.LFill(TipoEntrega    ,  1) +
                  FTxt.LFill(dtInicio       , 'ddmmyyyy', false ) +
                  FTxt.RFill(OnlyNumber(xInicio), 4) +
                  FTxt.LFill(dtTermino      , 'ddmmyyyy', false) +
                  FTxt.RFill(OnlyNumber(xTermino), 4) +
                  FTxt.LFill(cNFe           ,  9) +
                  FTxt.RFill(chNFe          , 45) +
                  FTxt.RFill(ProtocoloNFe   , 15) +
                  FTxt.RFill(AcaoEdiToStr(Acao), 1) +
                  FTxt.RFill(Filler         , 21) ;
      end
      else
      begin
        xTexto := xTexto +
                  FTxt.RFill(Romaneio                                , 15) +
                  FTxt.RFill(cRota                                   ,  7) +
                  FTxt.LFill(MeioTransporte                          ,  1) +
                  FTxt.LFill(TipoTransporte                          ,  1) +
                  FTxt.LFill(TipoCarga                               ,  1) +
                  FTxt.RFill(CondicaoFrete                           ,  1) +
                  FTxt.RFill(xSerie                                  ,  3) +
                  FTxt.LFill(xNumero                                 ,  8) +
                  FTxt.LFill(dtEmissao                               , 'ddmmyyyy', false) +
                  FTxt.RFill(tpMercadoria                            , 15) +
                  FTxt.RFill(xEspecie                                , 15) +
                  FTxt.VLFill(ValoresNF.qVolumes                     ,  7, 2, '0') +
                  FTxt.VLFill(ValoresNF.vTotalNF                     , 15, 2, '0') +
                  FTxt.VLFill(ValoresNF.qPesoBruto                   ,  7, 2, '0') +
                  FTxt.VLFill(ValoresNF.qPesoDensidade               ,  5, 2, '0') +
                  FTxt.RFill(IncideICMSToStr(ValoresNF.IncideICMS)   ,  1) +
                  FTxt.RFill(SimNaoEdiToStr(ValoresNF.SeguroEfetuado),  1) +
                  FTxt.VLFill(ValoresNF.vTotalSeguro                 , 15, 2, '0') +
                  FTxt.VLFill(ValoresNF.vCobrado                     , 15, 2, '0') +
                  FTxt.RFill(Placa                                   ,  7) +
                  FTxt.RFill(SimNaoEdiToStr(CargaRapida)             ,  1) +
                  FTxt.VLFill(ValoresNF.vFretePeso                   , 15, 2, '0') +
                  FTxt.VLFill(ValoresNF.vAdValorem                   , 15, 2, '0') +
                  FTxt.VLFill(ValoresNF.vTaxas                       , 15, 2, '0') +
                  FTxt.VLFill(ValoresNF.vFrete                       , 15, 2, '0') +
                  FTxt.RFill(AcaoEdiToStr(Acao)                      ,  1) +
                  FTxt.VLFill(ValoresNF.vIcms                        , 12, 2, '0') +
                  FTxt.VLFill(ValoresNF.vIcmsRetido                  , 12, 2, '0') +
                  FTxt.RFill(SimNaoEdiToStr(Bonificacao)             ,  1) +
                  FTxt.RFill(Filler                                  ,  2) ;
      end;
      Conteudo.Add( xTexto ) ;

      if Versao = ve50 then
        GerarValoresNF( ValoresNF )
      else
       GerarComplementoNF( ComplementoNF ) ;

      GerarCalculoFrete     ( CalculoFrete ) ;
      GerarIdCargas         ( IdCargas ) ;
      GerarInfoEntrega      ( InfoEntrega ) ;
      GerarItensNF          ( ItensNF ) ;
      GerarInfoConsignatario( InfoConsignatario ) ;
      GerarInfoRedespacho   ( InfoRedespacho ) ;
      GerarInfoTomador      ( InfoTomador ) ;

    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarValoresNF( Registro: TValoresNF ) ;
begin
  // Registros 506 Valores da Nota Fiscal versão 5.0
  if Registro.IdRegistro <> '' then
  begin
    with Registro do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.VLFill(qVolumes                     ,  8, 2, '0') +
                    FTxt.VLFill(qPesoBruto                   ,  9, 3, '0') +
                    FTxt.VLFill(qPesoLiquido                 ,  9, 3, '0') +
                    FTxt.VLFill(qPesoDensidade               , 10, 4 ,'0') +
                    FTxt.VLFill(qPesoCubado                  , 10, 4, '0') +
                    FTxt.RFill(IncideICMSToStr(IncideICMS)   ,  1) +
                    FTxt.RFill(SimNaoEdiToStr(SeguroEfetuado),  1) +
                    FTxt.VLFill(vCobrado                     , 15, 2, '0') +
                    FTxt.VLFill(vTotalNF                     , 15, 2, '0') +
                    FTxt.VLFill(vTotalSeguro                 , 15, 2, '0') +
                    FTxt.VLFill(vTotalDescto                 , 15, 2, '0') +
                    FTxt.VLFill(vOutraDespesas               , 15, 2, '0') +
                    FTxt.VLFill(vBCIcms                      , 15, 2, '0') +
                    FTxt.VLFill(vIcms                        , 15, 2, '0') +
                    FTxt.VLFill(vBCIcmsST                    , 15, 2, '0') +
                    FTxt.VLFill(vIcmsST                      , 15, 2, '0') +
                    FTxt.VLFill(vIcmsRetido                  , 15, 2, '0') +
                    FTxt.VLFill(vImpImportacao               , 15, 2, '0') +
                    FTxt.VLFill(vIPI                         , 15, 2, '0') +
                    FTxt.VLFill(vPIS                         , 15, 2, '0') +
                    FTxt.VLFill(vCofins                      , 15, 2, '0') +
                    FTxt.VLFill(vFrete                       , 15, 2, '0') +
                    FTxt.VLFill(vIcmsFrete                   , 13, 2, '0') +
                    FTxt.VLFill(vIcmsFreteST                 , 13, 2, '0') +
                    FTxt.VLFill(vIssFrete                    , 13, 2, '0') +
                    FTxt.RFill(Filler                        ,  5) ) ;
    end;
  end
  else
    raise Exception.Create('Erro: Registro com Valores da Nota Fiscal não Informado...,'+#13+
                           'Este registro é Obrigatório !!'  );
end;

procedure TACBrEDINotaFiscais.GerarComplementoNF( Registro: TComplementoNF ) ;
var
  xInicio, xTermino: String ;
begin
  // Registro 333 Complemento da Nota Fiscal para versão 3.10 ou anterior
  if Registro.IdRegistro <> '' then
  begin
    with Registro do
    begin
      xInicio  := Copy(TimeToStr(hrInicio),1,5) ;
      xTermino := Copy(TimeToStr(hrTermino),1,5) ;
      Conteudo.Add( IdRegistro +
                    FTxt.LFill(CFOP                            ,  4, False) +
                    FTxt.LFill(TipoEntrega                     ,  1) +
                    FTxt.LFill(dtInicio                        , 'ddmmyyyy', false ) +
                    FTxt.RFill(OnlyNumber(xInicio)             ,  4) +
                    FTxt.LFill(dtTermino                       , 'ddmmyyyy', false ) +
                    FTxt.RFill(OnlyNumber(xTermino)            ,  4) +
                    FTxt.RFill(LocDesembarque                  , 15) +
                    FTxt.RFill(SimNaoEdiToStr(FreteDifer)      ,  1) +
                    FTxt.RFill(TabelaFrete                     , 10) +
                    FTxt.LFill(OnlyNumber(EmissorNF1)          , 15) +
                    FTxt.RFill(SerieNF1                        ,  3) +
                    FTxt.LFill(NumeroNF1                       ,  8) +
                    FTxt.LFill(OnlyNumber(EmissorNF2)          , 15) +
                    FTxt.RFill(SerieNF2                        ,  3) +
                    FTxt.LFill(NumeroNF2                       ,  8) +
                    FTxt.LFill(OnlyNumber(EmissorNF3)          , 15) +
                    FTxt.RFill(SerieNF3                        ,  3) +
                    FTxt.LFill(NumeroNF3                       ,  8) +
                    FTxt.LFill(OnlyNumber(EmissorNF4)          , 15) +
                    FTxt.RFill(SerieNF4                        ,  3) +
                    FTxt.LFill(NumeroNF4                       ,  8) +
                    FTxt.LFill(OnlyNumber(EmissorNF5)          , 15) +
                    FTxt.RFill(SerieNF5                        ,  3) +
                    FTxt.LFill(NumeroNF5                       ,  8) +
                    FTxt.VLFill(vDespesas                      , 15, 2, '0') +
                    FTxt.RFill(MeioTransporteToStr(TipoVeiculo), 5) +
                    FTxt.RFill(EmissorCTe                      , 10) +
                    FTxt.RFill(SerieCTe                        ,  5) +
                    FTxt.RFill(NumeroCTe                       , 12) +
                    FTxt.RFill(Filler                          ,  5) );
    end;
  end
  else
    raise Exception.Create('Erro: Registro Informções Complementares da Nota Fiscal não Informado...,'+#13+
                           'Este registro é Obrigatório !!'  );
end;

procedure TACBrEDINotaFiscais.GerarCalculoFrete( Registro: TCalculoFrete ) ;
begin
  // Registro 507 Cálculo de Frete versão 5.0
  if Registro.IdRegistro <> '' then
  begin
    with Registro do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.VLFill(qVolumes        ,  8, 2, '0') +
                    FTxt.VLFill(qPesoBruto      ,  9, 3, '0') +
                    FTxt.VLFill(qPesoCubado     , 10, 4, '0') +
                    FTxt.VLFill(qPesoDensidade  , 10, 4, '0') +
                    FTxt.VLFill(vTotFrete       , 15, 2, '0') +
                    FTxt.VLFill(vTotFretePeso   , 15, 2, '0') +
                    FTxt.VLFill(vFrete          , 15, 2, '0') +
                    FTxt.VLFill(vAdValorem      , 15, 2, '0') +
                    FTxt.VLFill(vSecCat         , 15, 2, '0') +
                    FTxt.VLFill(vItrGris        , 15, 2, '0') +
                    FTxt.VLFill(vDespacho       , 15, 2, '0') +
                    FTxt.VLFill(vPedagio        , 15, 2, '0') +
                    FTxt.VLFill(vAdemeGris      , 15, 2, '0') +
                    FTxt.VLFill(vDespesas       , 15, 2, '0') +
                    FTxt.VLFill(vBCIcms         , 15, 2, '0') +
                    FTxt.VLFill(pAliqIcms       ,  5, 2, '0') +
                    FTxt.VLFill(vIcms           , 15, 2, '0') +
                    FTxt.RFill(SimNaoSTToStr(ST), 1) +
                    FTxt.VLFill(vBCIcmsST       , 15, 2, '0') +
                    FTxt.VLFill(pAliqIcmsST     ,  5, 2, '0') +
                    FTxt.VLFill(vIcmsST         , 15, 2, '0') +
                    FTxt.VLFill(vBcISS          , 15, 2, '0') +
                    FTxt.VLFill(pAliqISS        ,  5, 2, '0') +
                    FTxt.VLFill(vISS            , 15, 2, '0') +
                    FTxt.VLFill(vIR             , 15, 2, '0') +
                    FTxt.RFill(DireitoFiscalToStr(DireitoFiscal),  3) +
                    FTxt.RFill(TipoImpostoToStr(TipoImposto),  4) +
                    FTxt.RFill(UFGerador,  2) +
                    FTxt.RFill(Filler,  1) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarIdCargas( Registro: TIdCargas ) ;
var
  i: Integer ;
begin
  // Registro 508 Identificação da Carga versão 5.0
  for i := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[i] do
    begin
      Conteudo.Add(IdRegistro +
                   FTxt.RFill(xMarca   ,  50) +
                   FTxt.RFill(nroVolume,  50) +
                   FTxt.RFill(nroLacre ,  50) +
                   FTxt.RFill(Filler   , 167) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarInfoEntrega( Registro: TInfoEntrega) ;
var
  i: Integer ;
begin
  // Registros 509 ou 312 Informações da Entrega
  for i := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[i] do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.LFill(OnlyNumber(CNPJEmissorNF1) , 14) +
                    FTxt.RFill(NomeEmissorNF1             , 50) +
                    FTxt.RFill(SerieNF1                   ,  3) +
                    FTxt.LFill(NumeroNF1                  ,  9) +
                    FTxt.LFill(OnlyNumber(CNPJEmissorNF2) , 14) +
                    FTxt.RFill(NomeEmissorNF2             , 50) +
                    FTxt.RFill(SerieNF2                   ,  3) +
                    FTxt.LFill(NumeroNF2                  ,  9) +
                    FTxt.RFill(OnlyNumber(CNPJEmissorNF3) , 14) +
                    FTxt.RFill(NomeEmissorNF3             , 50) +
                    FTxt.RFill(SerieNF3                   ,  3) +
                    FTxt.LFill(NumeroNF3                  ,  9) +
                    FTxt.RFill(FilEmissorCT               , 10) +
                    FTxt.RFill(SerieConhecto              ,  5) +
                    FTxt.RFill(NumeroConhecto             , 12) +
                    FTxt.RFill(OnlyNumber(CNPJContratante), 14) +
                    FTxt.RFill(Filler                     , 48) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarItensNF( Registro: TItensNF ) ;
var
  I: Integer ;
begin
  // Registros 511 ou 314 Itens da Nota Fiscal
  for i := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[i] do
    begin
      if Versao = ve50 then
      begin
        Conteudo.Add( IdRegistro +
                      FTxt.VLFill(qVolumes ,  6, 2, '0') +
                      FTxt.RFill(xEspecie  , 15) +
                      FTxt.RFill(cItem     , 20) +
                      FTxt.RFill(xDescricao, 50) +
                      FTxt.LFill(CFOP      ,  4) +
                      FTxt.RFill(nroLote   , 20) +
                      FTxt.LFill(dtValidade, 'ddmmyyyy', false) +
                      FTxt.RFill(xMarca    , 50) +
                      FTxt.RFill(nroVolume , 50) +
                      FTxt.RFill(nroLacre  , 50) +
                      FTxt.VLFill(nroPedido, 20) +
                      FTxt.RFill(Filler    , 22) ) ;
      end
      else
      begin
        Conteudo.Add( IdRegistro +
                  FTxt.VLFill(qVolumes ,  5, 2, '0') +
                  FTxt.RFill(xEspecie  , 15) +
                  FTxt.RFill(xDescricao, 30) +
                  FTxt.RFill(Filler    , 29) ) ;
      end;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarInfoConsignatario( Registro: TInfoTomador ) ;
begin
  // Registros 513 ou 315
  if Registro.IdRegistro <> '' then
  begin
    with Registro do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.RFill(Razao              , IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(OnlyNumber(CNPJCPF), 14) +
                    FTxt.RFill(IE                 , 15) +
                    FTxt.RFill(Endereco           , IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(Bairro             , IfThen( Versao = ve50, 35, 20)) +
                    FTxt.RFill(Cidade             , 35) +
                    FTxt.LFill(Cep                , 9) +
                    FTxt.LFill(cMunicipio         , 9) +
                    FTxt.RFill(UF                 , 9) +
                    FTxt.RFill(Telefone           , 35) +
                    FTxt.RFill(Filler             , IfThen( Versao = ve50, 56, 11)) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarInfoRedespacho( Registro: TInfoRedespacho ) ;
begin
  // Registros 514 ou 316
  if Registro.IdRegistro <> '' then
  begin
    with Registro do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.RFill(Razao              , IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(OnlyNumber(CNPJCPF), 14) +
                    FTxt.RFill(IE                 , 15) +
                    FTxt.RFill(Endereco           , IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(Bairro             , IfThen( Versao = ve50, 35, 20)) +
                    FTxt.RFill(Cidade             , 35) +
                    FTxt.LFill(Cep                ,  9) +
                    FTxt.LFill(cMunicipio         ,  9) +
                    FTxt.RFill(UF                 ,  9) +

                    IfThen( Versao = ve50, FTxt.RFill(Telefone, 35) +
                                        FTxt.RFill(AreaFrete, 4),
                                        FTxt.RFill(AreaFrete, 4) +
                                        FTxt.RFill(Telefone, 35) ) +

                    FTxt.RFill(Filler, IfThen( Versao = ve50, 52, 7)) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarInfoTomador( Registro: TInfoTomador ) ;
begin
  // Registros 515 ou 317
  if Registro.IdRegistro <> '' then
  begin
    with Registro do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.RFill(Razao              , IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(OnlyNumber(CNPJCPF), 14) +
                    FTxt.RFill(IE                 , 15) +
                    FTxt.RFill(Endereco           , IfThen( Versao = ve50, 50, 40)) +
                    FTxt.RFill(Bairro             , IfThen( Versao = ve50, 35, 20)) +
                    FTxt.RFill(Cidade             , 35) +
                    FTxt.LFill(Cep                ,  9) +
                    FTxt.LFill(cMunicipio         ,  9) +
                    FTxt.RFill(UF                 ,  9) +
                    FTxt.RFill(Telefone           , 35) +
                    FTxt.RFill(Filler             , IfThen( Versao = ve50, 56, 11)) ) ;
    end;
  end;
end;

procedure TACBrEDINotaFiscais.GerarTotalNotas( Registro: TTotalNotas ) ;
var
  xTexto: String ;
begin
  // Registros 519 ou 318
  if Registro.IdRegistro = '' then
    raise Exception.Create('Erro: Registro Totalizador das Notas não informado...,'+#13+
                           'Este registro é Obrigatório !!' );

  with Registro do
  begin
    xTexto := IdRegistro +
              FTxt.VLFill(vTotal    , 15, 2, '0') +
              FTxt.VLFill(qPesoBruto, 15, 2, '0') ;
    case Versao of
      ve50: begin
              xTexto := xTexto + FTxt.VLFill(qVolumes,  15, 2, '0')+
                                 FTxt.LFill (qNotas  ,  10) +
                                 FTxt.RFill (Filler  , 262) ;
            end;
     else   begin
              xTexto := xTexto +
                        FTxt.VLFill(qPesoDensi, 15, 2, '0') +
                        FTxt.VLFill(qVolumes  , 15, 2, '0') +
                        FTxt.VLFill(vCobrado  , 15, 2, '0') +
                        FTxt.VLFill(vSeguro   , 15, 2, '0') +
                        FTxt.RFill( Registro.Filler, 147) ;
            end;
    end;
  end;
  Conteudo.Add( xTexto ) ;
end;

end.
