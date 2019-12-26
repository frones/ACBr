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

unit ACBrEDINotaFisClass;

{$I ACBr.inc}

interface

uses Classes, Contnrs, SysUtils, pediConversao, ACBrEDIClass ;

type

   { TInfoTomador Responsável pelo pagamento do frete Regsitro 515 ou 315 }
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
     constructor Create ;
     destructor  Destroy ; override ;

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
     FTipo        : Integer ; // 1 - CNPJ/CGC  2 - CPF
     FTpComercio  : String ;  // C - Comercial I - Indústria N - Não Contribuinte
     FcPais       : String ;
     FInsSuframa  : String ;
     FLocalEntrega: TLocalEntrega ;
     FInfoNotas   : TInfoNotas ;
   public
     property Tipo        : Integer        read FTipo          write FTipo ;
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

implementation

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

{ TNotasFis }

constructor TNotasFis.Create;
begin
  inherited Create ;
  FValoresNF        := TValoresNF.Create ;
  FCalculoFrete     := TCalculoFrete.Create ;
  FIdCargas         := TIdCargas.Create ;
  FInfoEntrega      := TInfoEntrega.Create ;
  FItensNF          := TItensNF.Create ;
  FInfoConsignatario:= TInfoTomador.Create ;
  FInfoRedespacho   := TInfoRedespacho.Create ;
  FInfoTomador      := TInfoTomador.Create ;
end;

destructor TNotasFis.Destroy;
begin
  FValoresNF.Free ;
  FCalculoFrete.Free ;
  FIdCargas.Free ;
  FInfoEntrega.Free ;
  FItensNF.Free ;
  FInfoConsignatario.Free ;
  FInfoRedespacho.Free ;
  FInfoTomador.Free ;
  inherited;
end;

end.
