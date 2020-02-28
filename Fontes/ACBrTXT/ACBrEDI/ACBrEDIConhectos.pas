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
    pediConversao, ACBrTxtClass, Contnrs, ACBrEDIClass ;

type

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}

   TComplConhecto = class( TPersistent ) // Usado nas Versões 3.0 a 3.1
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

   TACBrEDIConhectos = class( TComponent )
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
   public
     constructor Create(AOwner: TComponent); Override ;
     destructor  Destroy; Override ;

     property Cabecalho : TCabecalhoEdi read FCabecalho   write FCabecalho ;
     property InfoConEmb: TInfoConEmb   read FInfoConEmb  write FInfoConEmb ;
     property Conteudo  : TStringList   read FConteudo    write FConteudo ;
     property Versao    : tveEdi        read FVersao      write FVersao ;

     procedure GravarArquivo(const xArquivo: String);
     procedure LimpaRegistros ;
   end;

implementation

uses pcnAuxiliar, ACBrUtil ;

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
  case FVersao of
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
                FTxt.RFill(Cabecalho.Remetente,35)                  +
                FTxt.RFill(Cabecalho.Destinatario,35)               +
                FTxt.LFill(Cabecalho.Data, 'ddmmyy', false)         +
                FTxt.RFill(OnlyNumber(TimeToStr(Cabecalho.Hora)),4) +
                FTxt.RFill(Cabecalho.Id,12)                     +
                FTxt.RFill(Cabecalho.Filler, iif( Versao = ve50, 255, 585)) ) ;

  for i := 0 to InfoConEmb.Count - 1 do
  begin
    GerarIdConEmb      ( InfoConEmb.Items[i] ) ;
    GerarTransportadora( InfoConEmb.Items[i].Transportadora ) ;
    GerarTotConEmb     ( InfoConEmb.Items[i].TotConEmb ) ;
  end;

end;

procedure TACBrEDIConhectos.GerarIdConEmb( Registro: TConEmb ) ;
begin
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
                FTxt.RFill(Registro.Filler, iif( Versao = ve50, 333, 663)) ) ;
end;

procedure TACBrEDIConhectos.GerarTransportadora( Registro: TTransportadora ) ;
begin
  Conteudo.Add( Registro.IdRegistro +
                FTxt.RFill(OnlyNumber(Registro.CNPJ), 14, '0') +
                FTxt.RFill(Registro.Razao, iif( Versao = ve50, 50, 40)) +
                FTxt.RFill(Registro.Filler, iif( Versao = ve50, 283, 623)) ) ;

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
            FTxt.RFill( iif( Versao=ve31,Registro.Filler,Registro.CFOP), 3) +
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
                FTxt.RFill(Registro.cCTe, 9, '0')                           +
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
  Conteudo.Add( Registro.IdRegistro +
                FTxt.VLFill(Registro.qTotVolumes   , 8, 2, '0')  +
                FTxt.VLFill(Registro.qTotPesoBruto , 9, 3, '0')  +
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

procedure TACBrEDIConhectos.GerarNotasConhecto( Registro: TNotasConEmb ) ;
var
  i: Integer ;
begin
  for i := 0 to Registro.Count - 1 do
  begin
    Conteudo.Add( Registro.Items[i].IdRegistro +
                  FTxt.RFill(OnlyNumber(Registro.Items[i].CNPJEmissor), 14, '0')+
                  FTxt.RFill(Registro.Items[i].xNumero, 9, '0')                 +
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
                  OnlyNumber(Registro.CNPJ)         +
                  FTxt.LFill(Registro.IE,15)        +
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
  case FVersao of
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
var
  tmFiller: Integer ;
begin
  tmFiller := 0 ;
  case FVersao of
    ve30a,
    ve31 : tmFiller := 658 ;
    ve50 : tmFiller := 328 ;
  end;
  Conteudo.Add( Registro.IdRegistro + FormatFloat('0000',Registro.nQtde)       +
                                      FTxt.VLFill(Registro.vTotal, 15, 2, '0') +
                                      FTxt.RFill(Registro.Filler, tmFiller) ) ;
end;

procedure TACBrEDIConhectos.GerarComplConhecto( Registro: TComplConhecto );
begin
  if Registro.IdRegistro <> '' then
    Conteudo.Add( Registro.IdRegistro +
                MeioTransporteToStr(Registro.tpMeioTransp)    +
                FTxt.VLFill(Registro.vTotDespesa, 13, 2, '0') +
                FTxt.VLFill(Registro.vTotISS, 13, 2, '0')     +
                Copy(Registro.flContratante,1,10)             +
                FTxt.RFill(Registro.xSerieContata,5)          +
                FTxt.RFill(Registro.CTeContratante, 12)       +
                FTxt.RFill(Registro.cColeta,15)               +
                FTxt.RFill(Registro.docViagemEmb,20)          +
                FTxt.RFill(Registro.docAutorizacao,20)        +
                FTxt.RFill(Registro.xChaveAcesso,44)          +
                TpDoctoToStr(Registro.cTipoDocto)             +
                FTxt.RFill(Registro.Filler, 513) ) ;
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

end.
