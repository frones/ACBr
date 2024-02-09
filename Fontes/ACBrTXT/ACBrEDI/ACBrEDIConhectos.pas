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

unit ACBrEDIConhectos;

interface

uses SysUtils, Classes, ACBrBase,
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
    pediConversao, ACBrTXTClass, Contnrs, ACBrEDIClass ;

type

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

   TComplConhecto = class( TPersistent ) // Usado nas Versões 3.0, 3.0a e 3.1
   private
     FIdRegistro    : String ;
     FtpMeioTransp  : tmTransporte ;
     FvTotDespesa   : Currency ;
     FvTotISS       : Currency ;
     FflContratante : String ;
     FxSerieContata : String ;
     FCTeContratante: String ;
     FcColeta       : String ;
     FdocViagemEmb  : String ;
     FdocAutorizacao: String ;
     FxChaveAcesso  : String ;
     FcTipoDocto    : tpDocto ;
     FFiller        : String ;
   published
     property IdRegistro    : String       read FIdRegistro     write FIdRegistro ;
     property tpMeioTransp  : tmTransporte read FtpMeioTransp   write FtpMeioTransp;
     property vTotDespesa   : Currency     read FvTotDespesa    write FvTotDespesa ;
     property vTotISS       : Currency     read FvTotISS        write FvTotISS ;
     property flContratante : String       read FflContratante  write FflContratante ;
     property xSerieContata : String       read FxSerieContata  write FxSerieContata ;
     property CTeContratante: String       read FCTeContratante write FCTeContratante ;
     property cColeta       : String       read FcColeta        write FcColeta ;
     property docViagemEmb  : String       read FdocViagemEmb   write FdocViagemEmb ;
     property docAutorizacao: String       read FdocAutorizacao write FdocAutorizacao ;
     property xChaveAcesso  : String       read FxChaveAcesso   write FxChaveAcesso ;
     property cTipoDocto    : tpDocto      read FcTipoDocto     write FcTipoDocto ;
     property Filler        : String       read FFiller         write FFiller ;
   end;

   TEntrega = class  // Usado EDI 5.0
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
     FFilContratante : String ;
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
     property FilContratante : String  read FFilContratante  write FFilContratante ;
     property SerieConhecto  : String  read FSerieConhecto   write FSerieConhecto ;
     property NumeroConhecto : String  read FNumeroConhecto  write FNumeroConhecto ;
     property CNPJContratante: String  read FCNPJContratante write FCNPJContratante ;
     property Filler         : String  read FFiller          write FFiller ;
   end;

   TInfoEntrega = class( TObjectList )
   private
     function  GetItem(Index: Integer): TEntrega;
     procedure SetItem(Index: Integer; Value: TEntrega);
   public
     function  New: TEntrega;
     property  Items[Index: Integer]: TEntrega read GetItem write SetItem; default;
   end;

   TConsignatario = class  // Usado EDI 5.0
   private
     FIdRegistro: String ;
     FRazao     : String ;
     FCNPJ      : String ;
     FIE        : String ;
     FEndereco  : String ;
     FBairro    : String ;
     FMunicipio : String ;
     FcMunicipio: Integer ;
     FCep       : String ;
     FUF        : String ;
     FTelefone  : String ;
     FFiller    : String ;
   public
     property IdRegistro: String  read FIdRegistro  write FIdRegistro ;
     property Razao     : String  read FRazao       write FRazao ;
     property CNPJ      : String  read FCNPJ        write FCNPJ ;
     property IE        : String  read FIE          write FIE ;
     property Endereco  : String  read FEndereco    write FEndereco ;
     property Bairro    : String  read FBairro      write FBairro ;
     property Municipio : String  read FMunicipio   write FMunicipio ;
     property cMunicipio: Integer read FcMunicipio  write FcMunicipio ;
     property Cep       : String  read FCep         write FCep ;
     property UF        : String  read FUF          write FUF ;
     property Telefone  : String  read FTelefone    write FTelefone ;
     property Filler    : String  read FFiller      write FFiller ;
   end;

   TValoresConhecto = class( TPersistent ) // Usado no EDI 5.0
   private
     FIdRegistro    : String ;
     FqTotVolumes   : Double ;
     FqTotPesoBruto : Double ;
     FqTotPesoCubado: Double ;
     FqPesoDensidade: Double ;
     FvTotFrete     : Currency ;
     FvTotFretePeso : Currency ;
     FvFrete        : Currency ;
     FvAdValorem    : Currency ;
     FvSecCat       : Currency ;
     FvITR          : Currency ;
     FvDespacho     : Currency ;
     FvPedagio      : Currency ;
     FvAdemeGris    : Currency ;
     FvDespesas     : Currency ;
     FvDescAcrescimo: Currency ;
     FIDescAcrescimo: tDesctoAcrescimo ;
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
     FFiller        : String ;
   published
     property IdRegistro    : String            read FIdRegistro     write FIdRegistro ;
     property qTotVolumes   : Double            read FqTotVolumes    write FqTotVolumes ;
     property qTotPesoBruto : Double            read FqTotPesoBruto  write FqTotPesoBruto ;
     property qTotPesoCubado: Double            read FqTotPesoCubado write FqTotPesoCubado ;
     property qPesoDensidade: Double            read FqPesoDensidade write FqPesoDensidade ;
     property vTotFrete     : Currency          read FvTotFrete      write FvTotFrete ;
     property vTotFretePeso : Currency          read FvTotFretePeso  write FvTotFretePeso ;
     property vFrete        : Currency          read FvFrete         write FvFrete ;
     property vAdValorem    : Currency          read FvAdValorem     write FvAdValorem ;
     property vSecCat       : Currency          read FvSecCat        write FvSecCat ;
     property vITR          : Currency          read FvITR           write FvITR ;
     property vDespacho     : Currency          read FvDespacho      write FvDespacho ;
     property vPedagio      : Currency          read FvPedagio       write FvPedagio ;
     property vAdemeGris    : Currency          read FvAdemeGris     write FvAdemeGris ;
     property vDespesas     : Currency          read FvDespesas      write FvDespesas ;
     property vDescAcrescimo: Currency          read FvDescAcrescimo write FvDescAcrescimo ;
     property IDescAcrescimo: tDesctoAcrescimo  read FIDescAcrescimo write FIDescAcrescimo ;
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
     property Filler        : String            read FFiller         write FFiller ;
   end;

   TNotasConEmb = class( TObjectList )
   private
     function  GetItem(Index: Integer): TNFCT;
     procedure SetItem(Index: Integer; Value: TNFCT);
   public
     function  New: TNFCT;
     property  Items[Index: Integer]: TNFCT read GetItem write SetItem; default;
   end;

   TConhecimentos = class
   private
     FIdRegistro       : String ;
     FFilial           : String ;
     FSerie            : String ;
     FnCTe             : String ;
     FdtEmissao        : TDate ;
     FtpFrete          : tCondicaoFrete ;
     FFiller           : String ;
     FCNPJEmissor      : String ;
     FCNPJEmbarq       : String ;
     FAcao             : tpAcao ;
     FTipoCTe          : tpCTe ;
     FContinua         : String ;
     FCFOP             : String ;
     FCNPJDevolucao    : String ;     //////////////////////////////////////
     FCNPJConsignatario: String ;     //
     FCNPJDestinatario : String ;     //
     FPlacaVeiculo     : String ;     //
     FRomaneio         : String ;     //    Estes campos
     FNumeroSAP1       : String ;     //
     FNumeroSAP2       : String ;     //    são Utilizados
     FNumeroSAP3       : String ;     //
     FIdDocAutorizacao : String ;     //    pelo EDI 5.0
     FChaveCTe         : String ;     //
     FProtocoloCTe     : String ;     //
     FMeioTransporte   : tmTransporte;//
     FcCTe             : String ;     //
     FTranspContratante: String ;     //
     FSerieContratante : String ;     //
     FCTeContratante   : String ;     //
     FTipoFrete        : tpFrete ;    //
     FFreteDiferenciado: tediSimNao ; //
     FTabelaFrete      : String ;     //
     FCargaRapida      : tediSimNao ; //
     FUFEmbarcador     : String ;     //
     FUFEmissorCTe     : String ;     //
     FUFDestinatario   : String ;     /////////////////////////////////////
     FValoresConhecto  : TValoresConhecto ;
     FNotasConEmb      : TNotasConEmb ;
     FComplConhecto    : TComplConhecto ;
     FInfoEntrega      : TInfoEntrega ;
     FConsignatario    : TConsignatario ;
   public
     property IdRegistro       : String             read FIdRegistro        write FIdRegistro ;
     property Filial           : String             read FFilial            write FFilial ;
     property Serie            : String             read FSerie             write FSerie;
     property nCTe             : String             read FnCTe              write FnCTe;
     property dtEmissao        : TDate              read FdtEmissao         write FdtEmissao ;
     property tpFrete          : tCondicaoFrete     read FtpFrete           write FtpFrete ;
     property Filler           : String             read FFiller            write FFiller ;
     property Continua         : String             read FContinua          write FContinua ;
     property CNPJEmissor      : String             read FCNPJEmissor       write FCNPJEmissor ;
     property CNPJEmbarq       : String             read FCNPJEmbarq        write FCNPJEmbarq ;
     property Acao             : tpAcao             read FAcao              write FAcao ;
     property TipoCTe          : tpCTe              read FTipoCTe           write FTipoCTe ;
     property CFOP             : String             read FCFOP              write FCFOP ;
     property CNPJDevolucao    : String             read FCNPJDevolucao     write FCNPJDevolucao ;
     property CNPJConsignatario: String             read FCNPJConsignatario write FCNPJConsignatario ;
     property CNPJDestinatario : String             read FCNPJDestinatario  write FCNPJDestinatario ;
     property PlacaVeiculo     : String             read FPlacaVeiculo      write FPlacaVeiculo ;
     property Romaneio         : String             read FRomaneio          write FRomaneio ;
     property NumeroSAP1       : String             read FNumeroSAP1        write FNumeroSAP1 ;
     property NumeroSAP2       : String             read FNumeroSAP2        write FNumeroSAP2 ;
     property NumeroSAP3       : String             read FNumeroSAP3        write FNumeroSAP3 ;
     property IdDocAutorizacao : String             read FIdDocAutorizacao  write FIdDocAutorizacao ;
     property ChaveCTe         : String             read FChaveCTe          write FChaveCTe ;
     property ProtocoloCTe     : String             read FProtocoloCTe      write FProtocoloCTe ;
     property cCTe             : String             read FcCTe              write FcCTe ;
     property MeioTransporte   : tmTransporte       read FMeioTransporte    write FMeioTransporte ;
     property TranspContratante: String             read FTranspContratante write FTranspContratante ;
     property SerieContratante : String             read FSerieContratante  write FSerieContratante ;
     property CTeContratante   : String             read FCTeContratante    write FCTeContratante ;
     property TipoFrete        : tpFrete            read FTipoFrete         write FTipoFrete ;
     property FreteDiferenciado: tediSimNao         read FFreteDiferenciado write FFreteDiferenciado ;
     property TabelaFrete      : String             read FTabelaFrete       write FTabelaFrete ;
     property CargaRapida      : tediSimNao         read FCargaRapida       write FCargaRapida ;
     property UFEmbarcador     : String             read FUFEmbarcador      write FUFEmbarcador ;
     property UFEmissorCTe     : String             read FUFEmissorCTe      write FUFEmissorCTe ;
     property UFDestinatario   : String             read FUFDestinatario    write FUFDestinatario ;
     property ValoresConhecto  : TValoresConhecto   read FValoresConhecto   write FValoresConhecto ;
     property NotasConEmb      : TNotasConEmb       read FNotasConEmb       write FNotasConEmb ;
     property InfoEntrega      : TInfoEntrega       read FInfoEntrega       write FInfoEntrega ;
     property ComplConhecto    : TComplConhecto     read FComplConhecto     write FComplConhecto ;
     property Consignatario    : TConsignatario     read FConsignatario     write FConsignatario ;
   end;

   TConhectoEmbarcado = class( TObjectList )
   private
     function  GetItem(Index: Integer): TConhecimentos;
     procedure SetItem(Index: Integer; Value: TConhecimentos);
   public
     function  New: TConhecimentos;
     property  Items[Index: Integer]: TConhecimentos read GetItem write SetItem; default;
   end;

   TTotConEmb = class
   private
     FIdRegistro: String ;
     FQtde      : Integer ;
     FvTotal    : Currency ;
     FFiller    : String ;
   public
     property IdRegistro: String   read FIdRegistro write FIdRegistro ;
     property nQtde     : integer  read FQtde       write FQtde ;
     property vTotal    : Currency read FvTotal     write FvTotal ;
     property Filler    : String   read FFiller     write FFiller ;
   end;

   TTransportadora = class(TTransportador)
   private
      FConhectoEmbarcado: TConhectoEmbarcado ;
   public
      constructor Create;
      destructor  Destroy; Override ;

      property ConhectoEmbarcado: TConhectoEmbarcado read FConhectoEmbarcado write FConhectoEmbarcado ;
   end;

   { TConEmb Identificação do Documento }
   TConEmb = class
   private
     FIdRegistro     : String ;  // Identificador do Registro
     FIdDocto        : String ;
     FFiller         : String ;
     FTransportadora : TTransportadora ;
     FTotConEmb      : TTotConEmb ;
   public
     property IdRegistro     : String           read FIdRegistro      write FIdRegistro ;
     property IdDocto        : String           read FIdDocto         write FIdDocto ;
     property Filler         : String           read FFiller          write FFiller ;
     property Transportadora : TTransportadora  read FTransportadora  write FTransportadora ;
     property TotConEmb      : TTotConEmb       read FTotConEmb       write FTotConEmb ;
   end;

   { TInfoConEmb Registros de Identificação do Documento }
   TInfoConEmb = class( TObjectList )
   private
     function  GetItem(Index: Integer): TConEmb;
     procedure SetItem(Index: Integer; Value: TConEmb);
   public
     function  New: TConEmb;
     property  Items[Index: Integer]: TConEmb read GetItem write SetItem; default;
   end;

   {$IFDEF RTL230_UP}
   [ComponentPlatformsAttribute(piacbrAllPlatforms)]
   {$ENDIF RTL230_UP}
   TACBrEDIConhectos = class(TACBrComponent)
   private
      FTxt       : TACBrTxtClass ;
      FVersao    : tveEdi ;

      FConteudo  : TStringList ;
      FCabecalho : TCabecalhoEdi ;
      FInfoConEmb: TInfoConEmb ;

      procedure GerarCabecalho ;
      procedure GerarIdConEmb         ( Registro: TConEmb ) ;
      procedure GerarTransportadora   ( Registro: TTransportadora ) ;
      procedure GerarConhectoEmbarcado( Registro: TConhectoEmbarcado ) ;
      procedure GerarComplConhecto    ( Registro: TComplConhecto ) ;
      procedure GerarTotConEmb        ( Registro: TTotConEmb ) ;
      procedure GerarConhectosV3      ( Registro: TConhecimentos ) ;
      procedure GerarConhectosV5      ( Registro: TConhecimentos ) ;
      procedure GerarValoresConhecto  ( Registro: TValoresConhecto ) ;
      procedure GerarNotasConhecto    ( Registro: TNotasConEmb ) ;
      procedure GerarInfoEntrega      ( Registro: TInfoEntrega ) ;
      procedure GerarConsignatario    ( Registro: TConsignatario )  ;

      procedure LerCabecalho ;
      procedure LerIdConEmb         ( Registro: TInfoConEmb; nRow: Integer ) ;
      function  LerTransportadora   ( Registro: TTransportadora;    nRow: Integer ): Integer ;
      function  LerConhectoEmbarcado( Registro: TConhectoEmbarcado; nRow: Integer ): Integer ;
      function  LerComplConhecto    ( Registro: TComplConhecto;     nRow: Integer ): Integer ;
      function  LerTotConEmb        ( Registro: TTotConEmb;         nRow: Integer ): Integer ;
      function  LerValoresConhecto  ( Registro: TValoresConhecto;   nRow: Integer ): Integer ;
      function  LerNotasConhecto    ( Registro: TNotasConEmb;       nRow: Integer ): Integer ;
      function  LerInfoEntrega      ( Registro: TInfoEntrega;       nRow: Integer ): Integer ;
      function  LerConsignatario    ( Registro: TConsignatario;     nRow: Integer ): Integer ;
   public
     constructor Create(AOwner: TComponent); Override ;
     destructor  Destroy; Override ;

     property Cabecalho : TCabecalhoEdi  read FCabecalho   write FCabecalho ;
     property InfoConEmb: TInfoConEmb    read FInfoConEmb  write FInfoConEmb ;
     property Conteudo  : TStringList    read FConteudo    write FConteudo ;
     property Versao    : tveEdi         read FVersao      write FVersao ;

     procedure GravarArquivo(const xArquivo: String);
     procedure LerArquivo(const xArquivo: String);

     procedure LimpaRegistros ;
   end;

implementation

uses
  StrUtils,
  Math,
  ACBrUtil.Strings;

{ TConhectoEmbarcado }

function TConhectoEmbarcado.New: TConhecimentos;
begin
  Result                 := TConhecimentos.Create ;
  Result.ValoresConhecto := TValoresConhecto.Create ;
  Result.NotasConEmb     := TNotasConEmb.Create ;
  Result.InfoEntrega     := TInfoEntrega.Create ;
  Result.Consignatario   := TConsignatario.Create ;
  Result.ComplConhecto   := TComplConhecto.Create ;
  Add(Result) ;
end;

function TConhectoEmbarcado.GetItem(Index: Integer): TConhecimentos;
begin
  Result := TConhecimentos(inherited GetItem(Index)) ;
end;

procedure TConhectoEmbarcado.SetItem(Index: Integer; Value: TConhecimentos);
begin
  Put(Index, Value);
end;

{ TNotasConhecto }

function TNotasConEmb.New: TNFCT;
begin
  Result := TNFCT.Create ;
  Add(Result) ;
end;

function TNotasConEmb.GetItem(Index: Integer): TNFCT;
begin
  Result := TNFCT(inherited GetItem(Index)) ;
end;

procedure TNotasConEmb.SetItem(Index: Integer; Value: TNFCT);
begin
  Put(Index, Value);
end;

{ TConEmb }

procedure TACBrEDIConhectos.GerarCabecalho ;
var
  i: Integer ;
begin
  // Registro 000 Cabeçalho do Arquivo ConEmb

  case Versao of
     ve30,
    ve30a: if Cabecalho.Id = '' then
             Cabecalho.Id := 'CON' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     Copy(OnlyNumber(TimeToStr(FCabecalho.Hora)),1,4)      +
                     IntToStr(Cabecalho.Sequencia) ;
     ve31: if Cabecalho.Id = '' then
             Cabecalho.Id := 'CON31' +
                    Copy(FTxt.LFill(FCabecalho.Data, 'ddmmyy', false),1,4) +
                    FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
     ve50: if Cabecalho.Id = '' then
             Cabecalho.Id := 'CON50'+
                    Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                    FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
  end;

  Conteudo.Add( Cabecalho.IdRegistro +
                FTxt.RFill(Cabecalho.Remetente,35) +
                FTxt.RFill(Cabecalho.Destinatario,35) +
                FTxt.LFill(Cabecalho.Data, 'ddmmyy', false) +
                FTxt.RFill(OnlyNumber(TimeToStr(Cabecalho.Hora)),4) +
                FTxt.RFill(Cabecalho.Id,12) +
                FTxt.RFill(Cabecalho.Filler, IfThen( Versao = ve50, 255, 585)) ) ;

  for i := 0 to InfoConEmb.Count - 1 do
  begin
    GerarIdConEmb      ( InfoConEmb.Items[i] ) ;
    GerarTransportadora( InfoConEmb.Items[i].Transportadora ) ;
    GerarTotConEmb     ( InfoConEmb.Items[i].TotConEmb ) ;
  end;

end;

procedure TACBrEDIConhectos.GerarIdConEmb( Registro: TConEmb ) ;
begin
  // Registro 520 ou 320 Identificação do Arquivo ConEmb
  case Versao of
     ve30,
    ve30a: if Registro.IdDocto = '' then
             Registro.IdDocto := 'CONHE' +
                   Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                   Copy(OnlyNumber(TimeToStr(Cabecalho.Hora)),1,4)       +
                   IntToStr(Cabecalho.Sequencia) ;
     ve31: if Registro.IdDocto = '' then
             Registro.IdDocto := 'CON31' +
                   Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                   FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
     ve50: if Registro.IdDocto = '' then
             Registro.IdDocto := 'CONHE50'+
                   Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                   FTxt.VLFill(Cabecalho.Sequencia, 3, 0, '0') ;
  end;
  Conteudo.Add( Registro.IdRegistro +
                FTxt.RFill(Registro.IdDocto, 14) +
                FTxt.RFill(Registro.Filler, IfThen( Versao = ve50, 333, 663)) ) ;
end;

procedure TACBrEDIConhectos.GerarTransportadora( Registro: TTransportadora ) ;
begin
  // Registro 521 ou 321 Identificação da Transportadora
  Conteudo.Add( Registro.IdRegistro +
                FTxt.LFill(OnlyNumber(Registro.CNPJ), 14, false, '0') +
                FTxt.RFill(Registro.Razao, IfThen( Versao = ve50, 50, 40)) +
                FTxt.RFill(Registro.Filler, IfThen( Versao = ve50, 283, 623)) ) ;

  GerarConhectoEmbarcado( Registro.ConhectoEmbarcado ) ;
end;

procedure TACBrEDIConhectos.GerarConhectosV3( Registro: TConhecimentos );
var
  xTexto, xNotas: String ;
  n: Integer ;
begin
  xTexto := Registro.IdRegistro +
            FTxt.RFill(Registro.Filial , 10)                                +
            FTxt.RFill(Registro.Serie  ,  5)                                +
            FTxt.RFill(Registro.nCTe   , 12)                                +
            FTxt.LFill(Registro.dtEmissao, 'ddmmyyyy', false)               +
            CondicaoFreteToStr(Registro.tpFrete)                            +
            FTxt.VLFill(Registro.ValoresConhecto.qTotPesoBruto,  7, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.FvTotFrete   , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vBCIcms      , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.pAliqIcms    ,  4, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vIcms        , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vTotFretePeso, 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vFrete       , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vSecCat      , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vITR         , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vDespacho    , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vPedagio     , 15, 2, '0') +
            FTxt.VLFill(Registro.ValoresConhecto.vAdemeGris   , 15, 2, '0') +
            SimNaoSTToStr(Registro.ValoresConhecto.ST)                      +
            FTxt.RFill( IfThen( Versao=ve31,Registro.Filler,Registro.CFOP), 3) +
            FTxt.RFill(OnlyNumber(Registro.CNPJEmissor), 14, '0')           +
            FTxt.RFill(OnlyNumber(Registro.CNPJEmbarq) , 14, '0') ;

  if Registro.NotasConEmb.Count > 39 then
    Registro.Continua := 'C'  // informa que existem mais de 40 notas
  else
    Registro.Continua := 'U' ; // informa que existem menos ou até 40 notas

  xNotas := '' ;
  for n := 0 to Registro.NotasConEmb.Count -1 do
  begin
    xNotas := Trim(xNotas) + FTxt.RFill(Registro.NotasConEmb.Items[n].xSerie,3) +
                             FTxt.LFill(Registro.NotasConEmb.Items[n].xNumero,8) ;
  end;
  xTexto := xTexto + FTxt.RFill(xNotas,440)         +
                     AcaoEdiToStr(Registro.Acao)    +
                     TipoCTeToStr(Registro.TipoCte) ;

  if FVersao = ve31 then
    xTexto := xTexto + Registro.Continua + FTxt.RFill(Registro.CFOP,5) ;

  Conteudo.Add(xTexto) ;
end;

procedure TACBrEDIConhectos.GerarConhectosV5( Registro: TConhecimentos );
begin
  // Registro 522 Informações do Conhecimento Versão 5.0
  Conteudo.Add( Registro.IdRegistro +
                FTxt.RFill(Registro.Filial,10)                              +
                FTxt.RFill(Registro.Serie,5)                                +
                FTxt.LFill(Registro.nCTe, 12)                               +
                FTxt.LFill(Registro.dtEmissao, 'ddmmyyyy', false)           +
                CondicaoFreteToStr(Registro.tpFrete)                        +
                FTxt.LFill(OnlyNumber(Registro.CNPJEmissor)      , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJEmbarq)       , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJDevolucao)    , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJDestinatario) , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJConsignatario), 14)      +
                FTxt.RFill(Registro.CFOP, 5)                                +
                FTxt.RFill(Registro.PlacaVeiculo, 9)                        +
                FTxt.RFill(Registro.Romaneio, 20)                           +
                FTxt.RFill(Registro.NumeroSAP1, 20)                         +
                FTxt.RFill(Registro.NumeroSAP2, 20)                         +
                FTxt.RFill(Registro.NumeroSAP3, 20)                         +
                FTxt.RFill(Registro.IdDocAutorizacao, 15)                   +
                FTxt.RFill(Registro.ChaveCTe, 45)                           +
                FTxt.RFill(Registro.ProtocoloCTe, 15)                       +
                FTxt.RFill(Registro.cCTe, 9, ' ')                           +
                FTxt.RFill(Registro.TranspContratante, 10)                  +
                FTxt.RFill(Registro.SerieContratante, 5)                    +
                FTxt.RFill(Registro.CTeContratante, 12)                     +
                FTxt.RFill(MeioTransporteToStr(Registro.MeioTransporte), 5) +
                TipoCTeToStr(Registro.TipoCTe)                              +
                TipoFreteToStr(Registro.TipoFrete)                          +
                AcaoEdiToStr(Registro.Acao)                                 +
                SimNaoEdiToStr(Registro.FreteDiferenciado)                  +
                FTxt.RFill(Registro.TabelaFrete, 10)                        +
                SimNaoEdiToStr(Registro.CargaRapida)                        +
                FTxt.RFill(Registro.UFEmbarcador, 2)                        +
                FTxt.RFill(Registro.UFEmissorCTe, 2)                        +
                FTxt.RFill(Registro.UFDestinatario, 2)                      +
                FTxt.RFill(Registro.Filler, 10) ) ;
end;

procedure TACBrEDIConhectos.GerarValoresConhecto( Registro: TValoresConhecto ) ;
begin
  // Registro 523 323 valores do conhecimento
  if not Assigned(Registro) then
    raise Exception.Create('Registro com informações dos valores do Conhecimento não informado...,'+#13+
                           'Esta Informação é Obrigatória !!') ;

  Conteudo.Add( Registro.IdRegistro +
                FTxt.VLFill(Registro.qTotVolumes   , 8,  2, '0')  +
                FTxt.VLFill(Registro.qTotPesoBruto , 9,  3, '0')  +
                FTxt.VLFill(Registro.qTotPesoCubado, 10, 4, '0') +
                FTxt.VLFill(Registro.qPesoDensidade, 10, 4, '0') +
                FTxt.VLFill(Registro.vTotFrete     , 15, 2, '0') +
                FTxt.VLFill(Registro.vTotFretePeso , 15, 2, '0') +
                FTxt.VLFill(Registro.vFrete        , 15, 2, '0') +
                FTxt.VLFill(Registro.vAdValorem    , 15, 2, '0') +
                FTxt.VLFill(Registro.vSecCat       , 15, 2, '0') +
                FTxt.VLFill(Registro.vITR          , 15, 2, '0') +
                FTxt.VLFill(Registro.vDespacho     , 15, 2, '0') +
                FTxt.VLFill(Registro.vPedagio      , 15, 2, '0') +
                FTxt.VLFill(Registro.vAdemeGris    , 15, 2, '0') +
                FTxt.VLFill(Registro.vDespesas     , 15, 2, '0') +
                FTxt.VLFill(Registro.vDescAcrescimo, 15, 2, '0') +
                DesctoAcrescimoToStr(Registro.IDescAcrescimo)    +
                FTxt.VLFill(Registro.vBCIcms       , 15, 2, '0') +
                FTxt.VLFill(Registro.pAliqIcms     , 5, 2, '0')  +
                FTxt.VLFill(Registro.vIcms         , 15, 2, '0') +
                SimNaoSTToStr(Registro.ST)                       +
                FTxt.VLFill(Registro.vBCIcmsST     , 15, 2, '0') +
                FTxt.VLFill(Registro.pAliqIcmsST   , 5, 2, '0')  +
                FTxt.VLFill(Registro.vIcmsST       , 15, 2, '0') +
                FTxt.VLFill(Registro.vBcISS        , 15, 2, '0') +
                FTxt.VLFill(Registro.pAliqISS      , 5, 2, '0')  +
                FTxt.VLFill(Registro.vISS          , 15, 2, '0') +
                FTxt.VLFill(Registro.vIR           , 15, 2, '0') +
                DireitoFiscalToStr(Registro.DireitoFiscal)       +
                TipoImpostoToStr(Registro.TipoImposto)           +
                FTxt.RFill(Registro.Filler, 16) );
end;

procedure TACBrEDIConhectos.GerarComplConhecto( Registro: TComplConhecto );
begin
  if Registro.IdRegistro <> '' then
    Conteudo.Add( Registro.IdRegistro +
                FTxt.RFill(MeioTransporteToStr(Registro.tpMeioTransp),  5) +
                FTxt.VLFill(Registro.vTotDespesa                     , 15,  2, '0') +
                FTxt.VLFill(Registro.vTotISS                         , 15,  2, '0') +
                FTxt.RFill(Registro.flContratante                    , 10) +
                FTxt.RFill(Registro.xSerieContata                    ,  5) +
                FTxt.RFill(Registro.CTeContratante                   , 12) +
                FTxt.RFill(Registro.cColeta                          , 15) +
                FTxt.RFill(Registro.docViagemEmb                     , 20) +
                FTxt.RFill(Registro.docAutorizacao                   , 20) +
                FTxt.RFill(Registro.xChaveAcesso                     , 44) +
                FTxt.RFill(TpDoctoToStr(Registro.cTipoDocto)         ,  3) +
                FTxt.RFill(Registro.Filler, 513) ) ;
end;

procedure TACBrEDIConhectos.GerarNotasConhecto( Registro: TNotasConEmb ) ;
var
  i: Integer ;
begin
  // Registro 524 ou 324 notas fiscais do Conhecimento
  if Registro.Count = 0 then
  begin
    raise Exception.Create('Conhecimentos Embarcados, Notas Fiscal(is) não Informada(s)...,'+#13+
                           'Esta Informação é Obrigatória !!') ;
  end ;

  for i := 0 to Registro.Count - 1 do
  begin
    Conteudo.Add( Registro.Items[i].IdRegistro +
                  FTxt.RFill(OnlyNumber(Registro.Items[i].CNPJEmissor), 14, '0')+
                  FTxt.LFill(Registro.Items[i].xNumero, 9, false, '0')          +
                  FTxt.RFill(Registro.Items[i].xSerie, 3)                       +
                  FTxt.LFill(Registro.Items[i].dtEmissao, 'ddmmyyyy', false)    +
                  FTxt.VLFill(Registro.Items[i].vNF    , 15, 2, '0')            +
                  FTxt.VLFill(Registro.Items[i].qVolume,  8, 2, '0')            +
                  FTxt.VLFill(Registro.Items[i].qPesoNF,  9, 3, '0')            +
                  FTxt.VLFill(Registro.Items[i].qPesoDensidade, 10, 4, '0')     +
                  FTxt.VLFill(Registro.Items[i].qPesoCubado, 10, 4, '0')        +
                  FTxt.RFill(Registro.Items[i].IdPedido, 20)                    +
                  FTxt.RFill(Registro.Items[i].Romaneio, 20)                    +
                  FTxt.RFill(Registro.Items[i].NumeroSAP1, 20)                  +
                  FTxt.RFill(Registro.Items[i].NumeroSAP2, 20)                  +
                  FTxt.RFill(Registro.Items[i].NumeroSAP3, 20)                  +
                  SimNaoEDIToStr(Registro.Items[i].Devolucao)                   +
                  IntToStr(Registro.Items[i].TipoNF)                            +
                  SimNaoEdiToStr(Registro.Items[i].Bonificacao)                 +
                  FTxt.RFill(Registro.Items[i].CFOP, 4, '0')                    +
                  FTxt.RFill(Registro.Items[i].UFGerador, 2)                    +
                  FTxt.RFill(Registro.Items[i].Desdobro, 10)                    +
                  FTxt.RFill(Registro.Items[i].Filler, 142) ) ;
  end;
end;

procedure TACBrEDIConhectos.GerarInfoEntrega( Registro: TInfoEntrega) ;
var
  i: Integer ;
begin
  for i := 0 to Registro.Count - 1 do
  begin
    Conteudo.Add( Registro.Items[i].IdRegistro +
                  FTxt.LFill(OnlyNumber(Registro.Items[i].CNPJEmissorNF1),14)  +
                  FTxt.RFill(Registro.Items[i].NomeEmissorNF1, 50)             +
                  FTxt.RFill(Registro.Items[i].SerieNF1, 3)                    +
                  FTxt.LFill(Registro.Items[i].NumeroNF1, 9 )                  +
                  FTxt.LFill(OnlyNumber(Registro.Items[i].CNPJEmissorNF2), 14) +
                  FTxt.RFill(Registro.Items[i].NomeEmissorNF2, 50)             +
                  FTxt.RFill(Registro.Items[i].SerieNF2, 3)                    +
                  FTxt.LFill(Registro.Items[i].NumeroNF2, 9)                   +
                  FTxt.LFill(OnlyNumber(Registro.Items[i].CNPJEmissorNF3),14)  +
                  FTxt.RFill(Registro.Items[i].NomeEmissorNF3, 50)             +
                  FTxt.RFill(Registro.Items[i].SerieNF3, 3)                    +
                  FTxt.LFill(Registro.Items[i].NumeroNF3, 9)                   +
                  FTxt.RFill(Registro.Items[i].FilContratante,10)              +
                  FTxt.RFill(Registro.Items[i].SerieConhecto, 5)               +
                  FTxt.RFill(Registro.Items[i].NumeroConhecto, 12)             +
                  FTxt.LFill(OnlyNumber(Registro.Items[i].CNPJContratante),14) +
                  FTxt.RFill(Registro.Items[i].Filler, 78) ) ;
  end;
end;

procedure TACBrEDIConhectos.GerarConsignatario( Registro: TConsignatario) ;
begin
  if Registro.Razao <> '' then
  begin
    Conteudo.Add( Registro.IdRegistro               +
                  FTxt.RFill(Registro.Razao,60)     +
                  FTxt.RFill(OnlyNumber(Registro.CNPJ),14)+
                  FTxt.RFill(Registro.IE,15)        +
                  FTxt.RFill(Registro.Endereco,60)  +
                  FTxt.RFill(Registro.Bairro,35)    +
                  FTxt.RFill(Registro.Municipio,35) +
                  FTxt.RFill(Registro.Cep, 9)       +
                  FTxt.LFill(Registro.cMunicipio, 9)+
                  FTxt.RFill(Registro.UF, 9)        +
                  FTxt.RFill(Registro.Telefone, 35) +
                  FTxt.RFill(Registro.Filler, 66) ) ;
  end;
end;

procedure TACBrEDIConhectos.GerarConhectoEmbarcado( Registro: TConhectoEmbarcado ) ;
var
  c: Integer ;
begin
  if Registro.Count = 0 then
    raise Exception.Create('Registro de identificação do Conhecimento não informado...,'+#13+
                           'Esta Informação é Obrigatória !!') ;

  case FVersao of
     ve30,
    ve30a,
    ve31 : begin
             for c := 0 to Registro.Count - 1 do
             begin
               GerarConhectosV3  ( Registro.Items[c] ) ;
               GerarComplConhecto( Registro.Items[c].ComplConhecto ) ;
             end;
           end;
    ve50 : begin
             for c := 0 to Registro.Count - 1 do
             begin
               GerarConhectosV5    ( Registro.Items[c] ) ;
               GerarValoresConhecto( Registro.Items[c].ValoresConhecto ) ;
               GerarNotasConhecto  ( Registro.Items[c].NotasConEmb ) ;
               GerarInfoEntrega    ( Registro.Items[c].InfoEntrega ) ;
               GerarConsignatario  ( Registro.Items[c].Consignatario ) ;
             end;
           end;
  end;
end;

procedure TACBrEDIConhectos.GerarTotConEmb( Registro: TTotConEmb );
begin
  Conteudo.Add( Registro.IdRegistro +
                FormatFloat('0000',Registro.nQtde)       +
                FTxt.VLFill(Registro.vTotal, 15, 2, '0') +
                FTxt.RFill(Registro.Filler, IfThen( Versao = ve50, 328, 658)) ) ;
end;

constructor TACBrEDIConhectos.Create(AOwner: TComponent) ;
begin
  inherited Create(AOwner) ;
  FTxt                := TACBrTxtClass.Create ;
  FConteudo           := TStringList.Create ;
  FCabecalho          := TCabecalhoEdi.Create ;
  FInfoConEmb         := TInfoConEmb.Create ;

  FVersao             := ve50 ;
end;

destructor TACBrEDIConhectos.Destroy;
begin
  FTxt.Free ;
  FConteudo.Free ;
  FCabecalho.Free ;
  FInfoConEmb.Free ;
  inherited ;
end;

procedure TACBrEDIConhectos.GravarArquivo( const xArquivo: String ) ;
begin
  Conteudo.Clear ;

  GerarCabecalho ;
  Conteudo.SaveToFile( xArquivo ) ;

  Conteudo.Clear ;
end;

procedure TACBrEDIConhectos.LerArquivo(const xArquivo: String ) ;
begin
  Conteudo.Clear ;

  if not FileExists(xArquivo) then
    raise Exception.Create('Erro: Arquivo especificado não encontrado !!');

  Conteudo.LoadFromFile( xArquivo ) ;
  LerCabecalho ;

  Conteudo.Clear ;
end;

procedure TACBrEDIConhectos.LimpaRegistros;
begin
  InfoConEmb.Clear ;
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

{ TInfoConEmb }

function TInfoConEmb.New: TConEmb;
begin
  Result := TConEmb.Create;
  Result.Transportadora := TTransportadora.Create ;
  Result.TotConEmb      := TTotConEmb.Create ;
  Add(Result);
end;

function TInfoConEmb.GetItem(Index: Integer): TConEmb;
begin
  Result := TConEmb(inherited GetItem(Index)) ;
end;

procedure TInfoConEmb.SetItem(Index: Integer; Value: TConEmb);
begin
  Put(Index, Value) ;
end;

{ TTransportadora }

constructor TTransportadora.Create;
begin
  inherited Create ;
  FConhectoEmbarcado := TConhectoEmbarcado.Create ;
end;

destructor TTransportadora.Destroy;
begin
  FConhectoEmbarcado.Free ;
  inherited;
end;

procedure TACBrEDIConhectos.LerCabecalho ;
var
  nRow: Integer ;
begin
  // Registro 000 Cabeçalho do Arquivo ConEmb

  nRow := 0 ;
  Cabecalho.IdRegistro   := Copy(Conteudo.Strings[nRow]             ,  1,  3) ;
  Cabecalho.Remetente    := Copy(Conteudo.Strings[nRow]             ,  4, 35) ;
  Cabecalho.Destinatario := Copy(Conteudo.Strings[nRow]             , 39, 35) ;
  Cabecalho.Data         := StringToDate(Copy(Conteudo.Strings[nRow], 74,  6));
  Cabecalho.Hora         := StringToTime(Copy(Conteudo.Strings[nRow], 80,  4));
  Cabecalho.Id           := Copy(Conteudo.Strings[nRow]             , 84, 12) ;
  case Versao of
    ve50: Cabecalho.Filler := Copy(Conteudo.Strings[nRow]           , 96, 255) ;
    else  Cabecalho.Filler := Copy(Conteudo.Strings[nRow]           , 96, 585) ;
  end;
  inc(nRow) ;
  LerIdConEmb( InfoConEmb, nRow ) ;
end;

procedure TACBrEDIConhectos.LerIdConEmb( Registro: TInfoConEmb; nRow: Integer ) ;
var
  cReg: String ;
begin
  // Registro 520 ou 320 Identificação do Arquivo ConEmb
  cReg := IfThen( Versao = ve50, '520', '320') ;
  if Copy(Conteudo.Strings[nRow], 1, 3) <> cReg then
    raise Exception.Create('Erro: Nenhum Registro de Identificação não Informado...,'+#13+
                           'Este registro é obrigatório !!' );

  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro := cReg ;
      IdDocto    := Copy(Conteudo.Strings[nRow], 4, 14) ;
      case Versao of
        ve50: Filler := Copy(Conteudo.Strings[nRow], 18, 333) ;
        else  Filler := Copy(Conteudo.Strings[nRow], 18, 663) ;
      end;
      inc(nRow) ;
      nRow := LerTotConEmb( TotConEmb, LerTransportadora( Transportadora, nRow ) ) ;
    end;
    if nRow > (Conteudo.Count - 1) then
      Break ;
  end;
end;

function TACBrEDIConhectos.LerTransportadora( Registro: TTransportadora; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  // Registro 521 ou 321 Identificação da Transportadora
  cReg := IfThen( Versao = ve50, '521', '321') ;
  if Copy(Conteudo.Strings[nRow], 1, 3) <> cReg then
    raise Exception.Create('Erro: Nenhum Registro da Transportadora não Informado...,'+#13+
                           'Este registro é obrigatório !!' );

  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with Registro do
    begin
      IdRegistro := cReg ;
      CNPJ       := Copy(Conteudo.Strings[nRow], 4, 14) ;
      case Versao of
        ve50: begin
                Razao  := Copy(Conteudo.Strings[nRow], 18,  50) ;
                Filler := Copy(Conteudo.Strings[nRow], 68, 283) ;
              end;
        else  begin
                Razao  := Copy(Conteudo.Strings[nRow], 18,  40) ;
                Filler := Copy(Conteudo.Strings[nRow], 58, 623) ;
              end;
      end;
      Inc(nRow) ;
      nRow := LerConhectoEmbarcado( ConhectoEmbarcado, nRow ) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDIConhectos.LerConhectoEmbarcado( Registro: TConhectoEmbarcado; nRow: Integer ): Integer ;
var
  cReg: String ;
  ok  : Boolean ;
  n   : Integer ;
begin
  cReg := IfThen( Versao = ve50, '522', '322') ;
  if Copy(Conteudo.Strings[nRow], 1, 3) <> cReg then
    raise Exception.Create('Erro: Nenhum Registro de Conhecimentos não Informado...,'+#13+
                           'Este registro é obrigatório !!' );

  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro := cReg ;
      Filial     := Copy(Conteudo.Strings[nRow],  4, 10) ;
      Serie      := Copy(Conteudo.Strings[nRow], 14,  5) ;
      nCTe       := Copy(Conteudo.Strings[nRow], 19, 12) ;
      dtEmissao  := StringToDate(Copy(Conteudo.Strings[nRow], 31, 8)) ;
      tpFrete    := StrToCondicaoFrete(ok, Copy(Conteudo.Strings[nRow], 39, 1)) ;
      case Versao of
        ve50: begin
                CNPJEmissor       := Copy(Conteudo.Strings[nRow],  40, 14) ;
                CNPJEmbarq        := Copy(Conteudo.Strings[nRow],  54, 14) ;
                CNPJDevolucao     := Copy(Conteudo.Strings[nRow],  68, 14) ;
                CNPJDestinatario  := Copy(Conteudo.Strings[nRow],  82, 14) ;
                CNPJConsignatario := Copy(Conteudo.Strings[nRow],  96, 14) ;
                CFOP              := Copy(Conteudo.Strings[nRow], 110,  5) ;
                PlacaVeiculo      := Copy(Conteudo.Strings[nRow], 115,  9) ;
                Romaneio          := Copy(Conteudo.Strings[nRow], 124, 20) ;
                NumeroSAP1        := Copy(Conteudo.Strings[nRow], 144, 20) ;
                NumeroSAP2        := Copy(Conteudo.Strings[nRow], 164, 20) ;
                NumeroSAP3        := Copy(Conteudo.Strings[nRow], 184, 20) ;
                IdDocAutorizacao  := Copy(Conteudo.Strings[nRow], 204, 15) ;
                ChaveCTe          := Copy(Conteudo.Strings[nRow], 219, 45) ;
                ProtocoloCTe      := Copy(Conteudo.Strings[nRow], 264, 15) ;
                cCTe              := Copy(Conteudo.Strings[nRow], 279,  9) ;
                TranspContratante := Copy(Conteudo.Strings[nRow], 288, 10) ;
                SerieContratante  := Copy(Conteudo.Strings[nRow], 298,  5) ;
                CTeContratante    := Copy(Conteudo.Strings[nRow], 303, 12) ;
                MeioTransporte    := StrToMeioTransporte(ok, Copy(Conteudo.Strings[nRow], 315, 5)) ;
                TipoCTe           := StrToTipoCTe(ok, Copy(Conteudo.Strings[nRow]  , 320, 1)) ;
                TipoFrete         := StrToTipoFrete(ok,Copy(Conteudo.Strings[nRow] , 321, 1)) ;
                Acao              := StrToAcaoEdi(ok, Copy(Conteudo.Strings[nRow]  , 322, 1)) ;
                FreteDiferenciado := StrToSimNaoEdi(ok, Copy(Conteudo.Strings[nRow], 323, 1)) ;
                TabelaFrete       := Copy(Conteudo.Strings[nRow], 324, 10) ;
                CargaRapida       := StrToSimNaoEdi(ok, Copy(Conteudo.Strings[nRow], 334, 1)) ;
                UFEmbarcador      := Copy(Conteudo.Strings[nRow], 335,  2) ;
                UFEmissorCTe      := Copy(Conteudo.Strings[nRow], 337,  2) ;
                UFDestinatario    := Copy(Conteudo.Strings[nRow], 339,  2) ;
                Filler            := Copy(Conteudo.Strings[nRow], 341, 10) ;
                inc(nRow) ;
                nRow := LerConsignatario( Consignatario,
                                  LerInfoEntrega( InfoEntrega ,
                                  LerNotasConhecto( NotasConEmb,
                                  LerValoresConhecto( ValoresConhecto,
                                  nRow ) ) ) ) ;
              end;
        else  begin
                ValoresConhecto.qTotPesoBruto := StringToDouble(Copy(Conteudo.Strings[nRow],  40,  7),  5, 2) ;
                ValoresConhecto.FvTotFrete    := StringToDouble(Copy(Conteudo.Strings[nRow],  47, 15), 13, 2) ;
                ValoresConhecto.vBCIcms       := StringToDouble(Copy(Conteudo.Strings[nRow],  62, 15), 13, 2) ;
                ValoresConhecto.pAliqIcms     := StringToDouble(Copy(Conteudo.Strings[nRow],  77,  4),  2, 2) ;
                ValoresConhecto.vIcms         := StringToDouble(Copy(Conteudo.Strings[nRow],  81, 15), 13, 2) ;
                ValoresConhecto.vTotFretePeso := StringToDouble(Copy(Conteudo.Strings[nRow],  96, 15), 13, 2) ;
                ValoresConhecto.vFrete        := StringToDouble(Copy(Conteudo.Strings[nRow], 111, 15), 13, 2) ;
                ValoresConhecto.vSecCat       := StringToDouble(Copy(Conteudo.Strings[nRow], 126, 15), 13, 2) ;
                ValoresConhecto.vITR          := StringToDouble(Copy(Conteudo.Strings[nRow], 141, 15), 13, 2) ;
                ValoresConhecto.vDespacho     := StringToDouble(Copy(Conteudo.Strings[nRow], 156, 15), 13, 2) ;
                ValoresConhecto.vPedagio      := StringToDouble(Copy(Conteudo.Strings[nRow], 171, 15), 13, 2) ;
                ValoresConhecto.vAdemeGris    := StringToDouble(Copy(Conteudo.Strings[nRow], 186, 15), 13, 2) ;
                ValoresConhecto.ST            := StrToSimNaoST(ok, Copy(Conteudo.Strings[nRow], 201, 1)) ;
                CNPJEmissor                   := Copy(Conteudo.Strings[nRow], 205, 14) ;
                CNPJEmbarq                    := Copy(Conteudo.Strings[nRow], 219, 14) ;

                n := 233 ;
                while n <= 665 do
                begin
                  with NotasConEmb.New do
                  begin
                    xSerie  := Copy(Conteudo.Strings[nRow], n, 3) ;
                    xNumero := Copy(Conteudo.Strings[nRow], n+3, 8) ;
                  end;
                  n := n + 11 ;
                end;
                Acao                          := StrToAcaoEdi(ok, Copy(Conteudo.Strings[nRow], 673, 1)) ;
                TipoCTe                       := StrToTipoCTe(ok, Copy(Conteudo.Strings[nRow], 674, 1)) ;

                case Versao of
                  ve31:  begin
                           Continua := Copy(Conteudo.Strings[nRow], 675, 1) ;
                           CFOP     := Copy(Conteudo.Strings[nRow], 676, 5) ;
                           Filler   := Copy(Conteudo.Strings[nRow], 202, 3) ;
                         end;
                   ve30,
                  ve30a: begin
                           CFOP     := Copy(Conteudo.Strings[nRow], 202, 3) ;
                         end;
                end;
                inc(nRow) ;
                nRow := LerComplConhecto( ComplConhecto, nRow ) ;
              end;
      end;
    end;
  end;
  result := nRow ;
end;

{function TACBrEDIConhectos.LerConhectosV5( Registro: TConhecimentos; nRow: Integer ): Integer ;
var
  cReg: String ;
  ok: Boolean ;
begin
  // Registro 522 ou 322 Conhecimentos Embarcados
  cReg := '522' ;
  if (Copy(Conteudo.Strings[nRow], 1, 3) <> cReg) and (Versao = ve50) then
    raise Exception.Create('Erro: Nenhum Registro de Valores do Conhecimento não Informado...,'+#13+
                           'Este registro é obrigatório !!' );

  if (Copy(Conteudo.Strings[nRow], 1, 3) = cReg) and (Versao = ve50) then
  begin
    with Registro. do
    begin
      IdRegistro     := cReg ;
                FTxt.RFill(Registro.Filial,10)                              +
                FTxt.RFill(Registro.Serie,5)                                +
                FTxt.LFill(Registro.nCTe, 12)                               +
                FTxt.LFill(Registro.dtEmissao, 'ddmmyyyy', false)           +
                CondicaoFreteToStr(Registro.tpFrete)                        +
                FTxt.LFill(OnlyNumber(Registro.CNPJEmissor)      , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJEmbarq)       , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJDevolucao)    , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJDestinatario) , 14)      +
                FTxt.LFill(OnlyNumber(Registro.CNPJConsignatario), 14)      +
                FTxt.RFill(Registro.CFOP, 5)                                +
                FTxt.RFill(Registro.PlacaVeiculo, 9)                        +
                FTxt.RFill(Registro.Romaneio, 20)                           +
                FTxt.RFill(Registro.NumeroSAP1, 20)                         +
                FTxt.RFill(Registro.NumeroSAP2, 20)                         +
                FTxt.RFill(Registro.NumeroSAP3, 20)                         +
                FTxt.RFill(Registro.IdDocAutorizacao, 15)                   +
                FTxt.RFill(Registro.ChaveCTe, 45)                           +
                FTxt.RFill(Registro.ProtocoloCTe, 15)                       +
                FTxt.RFill(Registro.cCTe, 9, ' ')                           +
                FTxt.RFill(Registro.TranspContratante, 10)                  +
                FTxt.RFill(Registro.SerieContratante, 5)                    +
                FTxt.RFill(Registro.CTeContratante, 12)                     +
                FTxt.RFill(MeioTransporteToStr(Registro.MeioTransporte), 5) +
                TipoCTeToStr(Registro.TipoCTe)                              +
                TipoFreteToStr(Registro.TipoFrete)                          +
                AcaoEdiToStr(Registro.Acao)                                 +
                SimNaoEdiToStr(Registro.FreteDiferenciado)                  +
                FTxt.RFill(Registro.TabelaFrete, 10)                        +
                SimNaoEdiToStr(Registro.CargaRapida)                        +
                FTxt.RFill(Registro.UFEmbarcador, 2)                        +
                FTxt.RFill(Registro.UFEmissorCTe, 2)                        +
                FTxt.RFill(Registro.UFDestinatario, 2)                      +
                FTxt.RFill(Registro.Filler, 10) ) ;
end; }

function TACBrEDIConhectos.LerValoresConhecto( Registro: TValoresConhecto; nRow: Integer ): Integer ;
var
  cReg: String ;
  ok: Boolean ;
begin
  // Registro 523 valores do conhecimento
  cReg := '523' ;
  if (Copy(Conteudo.Strings[nRow], 1, 3) <> cReg) and (Versao = ve50) then
    raise Exception.Create('Erro: Nenhum Registro de Valores do Conhecimento não Informado...,'+#13+
                           'Este registro é obrigatório !!' );

  if (Copy(Conteudo.Strings[nRow], 1, 3) = cReg) and (Versao = ve50) then
  begin
    with Registro do
    begin
      IdRegistro     := cReg ;
      qTotVolumes    := StringToDouble(Copy(Conteudo.Strings[nRow],   4,  8),  6, 2) ;
      qTotPesoBruto  := StringToDouble(Copy(Conteudo.Strings[nRow],  12,  9),  6, 3) ;
      qTotPesoCubado := StringToDouble(Copy(Conteudo.Strings[nRow],  21, 10),  6, 4) ;
      qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow],  31, 10),  6, 4) ;
      vTotFrete      := StringToDouble(Copy(Conteudo.Strings[nRow],  41, 15), 13, 2) ;
      vTotFretePeso  := StringToDouble(Copy(Conteudo.Strings[nRow],  56, 15), 13, 2) ;
      vFrete         := StringToDouble(Copy(Conteudo.Strings[nRow],  71, 15), 13, 2) ;
      vAdValorem     := StringToDouble(Copy(Conteudo.Strings[nRow],  86, 15), 13, 2) ;
      vSecCat        := StringToDouble(Copy(Conteudo.Strings[nRow], 101, 15), 13, 2) ;
      vITR           := StringToDouble(Copy(Conteudo.Strings[nRow], 116, 15), 13, 2) ;
      vDespacho      := StringToDouble(Copy(Conteudo.Strings[nRow], 131, 15), 13, 2) ;
      vPedagio       := StringToDouble(Copy(Conteudo.Strings[nRow], 146, 15), 13, 2) ;
      vAdemeGris     := StringToDouble(Copy(Conteudo.Strings[nRow], 161, 15), 13, 2) ;
      vDespesas      := StringToDouble(Copy(Conteudo.Strings[nRow], 176, 15), 13, 2) ;
      vDescAcrescimo := StringToDouble(Copy(Conteudo.Strings[nRow], 191, 15), 13, 2) ;
      IDescAcrescimo := StrToDesctoAcrescimo(ok, Copy(Conteudo.Strings[nRow], 206, 1)) ;
      vBCIcms        := StringToDouble(Copy(Conteudo.Strings[nRow], 207, 15), 13, 2) ;
      pAliqIcms      := StringToDouble(Copy(Conteudo.Strings[nRow], 222,  5),  3, 2) ;
      vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow], 227, 15), 13, 2) ;
      ST             := StrToSimNaoST(ok, Copy(Conteudo.Strings[nRow], 242, 1)) ;
      vBCIcmsST      := StringToDouble(Copy(Conteudo.Strings[nRow], 243, 15), 13, 2) ;
      pAliqIcmsST    := StringToDouble(Copy(Conteudo.Strings[nRow], 258,  5),  3, 2) ;
      vIcmsST        := StringToDouble(Copy(Conteudo.Strings[nRow], 263, 15), 13, 2) ;
      vBcISS         := StringToDouble(Copy(Conteudo.Strings[nRow], 278, 15), 13, 2) ;
      pAliqISS       := StringToDouble(Copy(Conteudo.Strings[nRow], 293,  5),  3, 2) ;
      vISS           := StringToDouble(Copy(Conteudo.Strings[nRow], 298, 15), 13, 2) ;
      vIR            := StringToDouble(Copy(Conteudo.Strings[nRow], 313, 15), 13, 2) ;
      DireitoFiscal  := StrToDireitoFiscal(ok, Copy(Conteudo.Strings[nRow], 328, 3));
      TipoImposto    := StrToTipoImposto(ok, Copy(Conteudo.Strings[nRow], 331, 4)) ;
      Filler         := Copy(Conteudo.Strings[nRow], 335, 16) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDIConhectos.LerNotasConhecto( Registro: TNotasConEmb; nRow: Integer ): Integer ;
var
  cReg: String ;
  ok: Boolean ;
begin
  // Registro 524 ou 324 notas fiscais do Conhecimento
  cReg := IfThen( Versao = ve50, '524', '324') ;
  if (Copy(Conteudo.Strings[nRow], 1, 3) <> cReg) and (Versao = ve50) then
    raise Exception.Create('Erro: Nenhum Registro de Notas Fiscais não Informado...,'+#13+
                           'Este registro é obrigatório !!' );

  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro     := cReg ;
      CNPJEmissor    := Copy(Conteudo.Strings[nRow]               ,   4,  14) ;
      xNumero        := Copy(Conteudo.Strings[nRow]               ,  18,   9) ;
      xSerie         := Copy(Conteudo.Strings[nRow]               ,  27,   3) ;
      dtEmissao      := StringToDate(Copy(Conteudo.Strings[nRow]  ,  30,   8)) ;
      vNF            := StringToDouble(Copy(Conteudo.Strings[nRow],  38,  15), 13, 2) ;
      qVolume        := StringToDouble(Copy(Conteudo.Strings[nRow],  53,   8),  6, 2) ;
      qPesoNF        := StringToDouble(Copy(Conteudo.Strings[nRow],  61,   9),  9, 3) ;
      qPesoDensidade := StringToDouble(Copy(Conteudo.Strings[nRow],  70,  10), 6, 4) ;
      qPesoCubado    := StringToDouble(Copy(Conteudo.Strings[nRow],  80,  10), 6, 4) ;
      IdPedido       := Copy(Conteudo.Strings[nRow]               ,  90,  20) ;
      Romaneio       := Copy(Conteudo.Strings[nRow]               , 110,  20) ;
      NumeroSAP1     := Copy(Conteudo.Strings[nRow]               , 130,  20) ;
      NumeroSAP2     := Copy(Conteudo.Strings[nRow]               , 150,  20) ;
      NumeroSAP3     := Copy(Conteudo.Strings[nRow]               , 170,  20) ;
      Devolucao      := StrToSimNaoEDI(ok, Copy(Conteudo.Strings[nRow], 190,   1)) ;
      TipoNF         := StrToInt(trim(Copy(Conteudo.Strings[nRow] , 191,1))) ;
      Bonificacao    := StrToSimNaoEdi(ok, Copy(Conteudo.Strings[nRow], 192, 1) );
      CFOP           := Copy(Conteudo.Strings[nRow]               , 193,   4) ;
      UFGerador      := Copy(Conteudo.Strings[nRow]               , 197,   2) ;
      Desdobro       := Copy(Conteudo.Strings[nRow]               , 199,  10) ;
      Filler         := Copy(Conteudo.Strings[nRow]               , 209, 142) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDIConhectos.LerInfoEntrega( Registro: TInfoEntrega; nRow: Integer): Integer ;
var
  cReg: String ;
begin
  // Registro 525 Dados da Entrega Redespacho
  cReg := '525' ;

  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with Registro.New do
    begin
      IdRegistro      := cReg ;
      CNPJEmissorNF1  := Copy(Conteudo.Strings[nRow],   4, 14) ;
      NomeEmissorNF1  := Copy(Conteudo.Strings[nRow],  18, 50) ;
      SerieNF1        := Copy(Conteudo.Strings[nRow],  68,  3) ;
      NumeroNF1       := StrToInt(trim(Copy(Conteudo.Strings[nRow],  71,  9))) ;
      CNPJEmissorNF2  := Copy(Conteudo.Strings[nRow],  80, 14) ;
      NomeEmissorNF2  := Copy(Conteudo.Strings[nRow],  94, 50) ;
      SerieNF2        := Copy(Conteudo.Strings[nRow], 144,  3) ;
      NumeroNF2       := StrToInt(trim(Copy(Conteudo.Strings[nRow], 147,  9))) ;
      CNPJEmissorNF3  := Copy(Conteudo.Strings[nRow], 156, 14) ;
      NomeEmissorNF3  := Copy(Conteudo.Strings[nRow], 170, 50) ;
      SerieNF3        := Copy(Conteudo.Strings[nRow], 220,  3) ;
      NumeroNF3       := StrToInt(trim(Copy(Conteudo.Strings[nRow], 223,  9))) ;
      FilContratante  := Copy(Conteudo.Strings[nRow], 232, 10) ;
      SerieConhecto   := Copy(Conteudo.Strings[nRow], 242,  5) ;
      NumeroConhecto  := Copy(Conteudo.Strings[nRow], 247, 12) ;
      CNPJContratante := Copy(Conteudo.Strings[nRow], 259, 14) ;
      Filler          := Copy(Conteudo.Strings[nRow], 273, 78) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDIConhectos.LerConsignatario( Registro: TConsignatario; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  // Registro 525 Dados da Entrega Redespacho
  cReg := '527' ;

  if Copy(Conteudo.Strings[nRow], 1, 3) = cReg then
  begin
    with Registro do
    begin
      IdRegistro := cReg ;
      Razao      := Copy(Conteudo.Strings[nRow],   4, 60) ;
      CNPJ       := Copy(Conteudo.Strings[nRow],  64, 14) ;
      IE         := Copy(Conteudo.Strings[nRow],  78, 15) ;
      Endereco   := Copy(Conteudo.Strings[nRow],  93, 60) ;
      Bairro     := Copy(Conteudo.Strings[nRow], 153, 35) ;
      Municipio  := Copy(Conteudo.Strings[nRow], 188, 35) ;
      Cep        := Copy(Conteudo.Strings[nRow], 223,  9) ;
      cMunicipio := StrToInt(trim(Copy(Conteudo.Strings[nRow], 232, 9))) ;
      UF         := Copy(Conteudo.Strings[nRow], 241,  9) ;
      Telefone   := Copy(Conteudo.Strings[nRow], 250, 35) ;
      Filler     := Copy(Conteudo.Strings[nRow], 285, 66) ;
    end;
    inc(nRow)
  end;
  result := nRow ;
end;

function TACBrEDIConhectos.LerComplConhecto( Registro: TComplConhecto; nRow: Integer ): Integer;
var
  cReg: String ;
  ok  : Boolean ;
begin
  // Registro 329 Complemento Conhecimentos Embaracados
  cReg := '329' ;

  if (Copy(Conteudo.Strings[nRow], 1, 3) = '329') and (Versao = ve31) then
  begin
    with Registro do
    begin
      IdRegistro     := cReg ;
      tpMeioTransp   := StrToMeioTransporte(ok, Copy(Conteudo.Strings[nRow], 4, 5)) ;
      vTotDespesa    := StringToDouble(Copy(Conteudo.Strings[nRow]  ,   9, 15), 13, 2) ;
      vTotISS        := StringToDouble(Copy(Conteudo.Strings[nRow]  ,  24, 15), 13, 2) ;
      flContratante  := Copy(Conteudo.Strings[nRow]                 ,  39, 10) ;
      xSerieContata  := Copy(Conteudo.Strings[nRow]                 ,  49,  5) ;
      CTeContratante := Copy(Conteudo.Strings[nRow]                 ,  54, 12) ;
      cColeta        := Copy(Conteudo.Strings[nRow]                 ,  66, 15) ;
      docViagemEmb   := Copy(Conteudo.Strings[nRow]                 ,  81, 20) ;
      docAutorizacao := Copy(Conteudo.Strings[nRow]                 , 101, 20) ;
      xChaveAcesso   := Copy(Conteudo.Strings[nRow]                 , 121, 44) ;
      cTipoDocto     := StrToTpDocto(ok, Copy(Conteudo.Strings[nRow], 165,  2)) ;
      Filler         := Copy(Conteudo.Strings[nRow]                 , 167, 513) ;
      inc(nRow) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDIConhectos.LerTotConEmb( Registro: TTotConEmb; nRow: Integer ): Integer;
var
  cReg: String ;
begin
  // Registro 529 ou 323 Totalizador de Conhecimentos Embarcados
  cReg := IfThen( Versao = ve50, '529', '323') ;
  if Copy(Conteudo.Strings[nRow], 1, 3) <> cReg then
    raise Exception.Create('Erro: Nenhum Registro Totalizafdor não Informado...,'+#13+
                           'Este registro é obrigatório !!' );

  with Registro do
  begin
    IdRegistro := cReg ;
    nQtde      := StrToInt(Trim(Copy(Conteudo.Strings[nRow] , 4, 4))) ;
    vTotal     := StringToDouble(Copy(Conteudo.Strings[nRow], 8, 15), 13, 2) ;
    case Versao of
      ve50: Filler := Copy(Conteudo.Strings[nRow], 23, 328) ;
      else  Filler := Copy(Conteudo.Strings[nRow], 23, 658) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

end.
