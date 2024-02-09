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

unit ACBrEDIPreFatura;

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

   { TValoresFrete Registro referente ao Cálculo efetuado do Frete a ser Cobrado }
   TCalculoFrete = class
   private
     FIdRegistro    : String ;
     FvFretePeso    : Currency ;
     FvSecCat       : Currency ;
     FvItrGris      : Currency ;
     FvPedagio      : Currency ;
     FvDiversos     : Currency ;
     FvDesconto     : Currency ;
     FvAdemeGris    : Currency ;
     FpAliqISS      : Currency ;
     FvISS          : Currency ;
     FvBCIcms       : Currency ;
     FpAliqIcms     : Currency ;
     FvIcms         : Currency ;
     FvAdValorem    : Currency ;
     FvDespacho     : Currency ;
     FqPesoBruto    : Double ;
     FFiller        : String ;
   public
     property IdRegistro    : String            read FIdRegistro     write FidRegistro ;
     property qPesoBruto    : Double            read FqPesoBruto     write FqPesoBruto ;
     property vTotFretePeso : Currency          read FvFretePeso     write FvFretePeso ;
     property vAdValorem    : Currency          read FvAdValorem     write FvAdValorem ;
     property vSecCat       : Currency          read FvSecCat        write FvSecCat ;
     property vItrGris      : Currency          read FvItrGris       write FvItrGris ;
     property vDespacho     : Currency          read FvDespacho      write FvDespacho ;
     property vPedagio      : Currency          read FvPedagio       write FvPedagio ;
     property vAdemeGris    : Currency          read FvAdemeGris     write FvAdemeGris ;
     property vDiversos     : Currency          read FvDiversos      write FvDiversos ;
     property vBCIcms       : Currency          read FvBCIcms        write FvBCIcms ;
     property pAliqIcms     : Currency          read FpAliqIcms      write FpAliqIcms ;
     property vIcms         : Currency          read FvIcms          write FvIcms ;
     property pAliqISS      : Currency          read FpAliqISS       write FpAliqISS ;
     property vISS          : Currency          read FvISS           write FvISS ;
     property vDesconto     : Currency          read FvDesconto      write FvDesconto ;
     property Filler        : String            read FFiller         write FFiller ;
   end;

  { TLiebrados Registro dos Documentos Liberados para Faturamento }
   TLiberados = class
   private
     FIdRegistro      : String ;
     FCnpjEmissor     : String ;
     FSerieDocto      : String ;
     FIdDocto         : String ;
     FdtEmissao       : TDateTime ;
     FSerieCTe        : String ;
     FnroCTe          : String ;
     FdtEmissaoCTe    : TDateTime ;
     FCnpjOrigem      : String ;
     FCnpjDestino     : String ;
     FtpCnpjDestino   : Integer ;
     FvFreteEmbarcador: Currency ;
     FvFreteTransporte: Currency ;
     FtpDiferenca     : Integer ;
     FvDiferenca      : Currency ;
     FDoctoInterno    : String ;
     FFiller          : String ;
     FCalculoFrete    : TCalculoFrete ;
   public
     property IdRegistro      : String     read FIdRegistro       write FIdRegistro ;
     property CnpjEmissor     : String     read FCnpjEmissor      write FCnpjEmissor ;
     property SerieDocto      : String     read FSerieDocto       write FSerieDocto ;
     property IdDocto         : String     read FIdDocto          write FIdDocto ;
     property dtEmissao       : TDateTime  read FdtEmissao        write FdtEmissao ;
     property SerieCTe        : String     read FSerieCTe         write FSerieCTe ;
     property nroCTe          : String     read FnroCTe           write FnroCTe ;
     property dtEmissaoCTe    : TDateTime  read FdtEmissaoCTe     write FdtEmissaoCTe ;
     property CnpjOrigem      : String     read FCnpjOrigem       write FCnpjOrigem ;
     property CnpjDestino     : String     read FCnpjDestino      write FCnpjDestino ;
     property tpCnpjDestino   : Integer    read FtpCnpjDestino    write FtpCnpjDestino ;
     property vFreteEmbarcador: Currency   read FvFreteEmbarcador write FvFreteEmbarcador ;
     property vFreteTransporte: Currency   read FvFreteTransporte write FvFreteTransporte ;
     property tpDiferenca     : Integer    read FtpDiferenca      write FtpDiferenca ;
     property vDiferenca      : Currency   read FvDiferenca       write FvDiferenca ;
     property DoctoInterno    : String     read FDoctoInterno     write FDoctoInterno ;
     property Filler          : String     read FFiller           write FFiller ;

     property CalculoFrete    : TCalculoFrete read FCalculoFrete  write FCalculoFrete ;
   end;

   { TDoctosLiberados Informações dos Documentos Liberados contidos no arquivo }
   TDoctosLiberados = class( TObjectList )
   private
     function  GetItem(Index: Integer): TLiberados;
     procedure SetItem(Index: Integer; Value: TLiberados);
   public
     function  New: TLiberados;
     property  Items[Index: Integer]: TLiberados read GetItem write SetItem; default;
   end;

   { TNotas Constantes no Conhecimento }
   TNotas = class
   private
     FSerieNF   : String ;
     FNumeroNF  : Integer ;
   public
     property SerieNF   : String  read FSerieNF    write FSerieNF ;
     property NumeroNF  : Integer read FNumeroNF   write FNumeroNF ;
   end;

   TNFS = class( TObjectList )
   private
     function  GetItem(Index: Integer): TNotas;
     procedure SetItem(Index: Integer; Value: TNotas);
   public
     function  New: TNotas;
     property  Items[Index: Integer]: TNotas read GetItem write SetItem; default;
   end;

   TIdNotas = class
   private
     FIdRegistro: String ;
     FFiller    : String ;
     FInfoNotas : TNFS ;
   public
     property IdRegistro: String  read FIdRegistro write FIdRegistro ;
     property Filler    : String  read FFiller     write FFiller ;
     property InfoNotas : TNFS    read FInfoNotas  write FInfoNotas ;
   end;

   { TInfoEntrega Informações do Local de Entrega Regsitro 509 ou 309 }
   TNotasCTe = class( TObjectList )
   private
     function  GetItem(Index: Integer): TIdNotas;
     procedure SetItem(Index: Integer; Value: TIdNotas);
   public
     function  New: TIdNotas;
     property  Items[Index: Integer]: TIdNotas read GetItem write SetItem; default;
   end;

   { TCargas Registro para Identificação das Cargas Regsitro 508 ou 333 }
   TPreFatura = class
   private
     FIdRegistro     : String ;
     FIdPreFatura    : String ;
     FdtEmissao      : TDate ;
     FdtPagamento    : TDate ;
     FqtdeDoctos     : Integer ;
     FvPreFatura     : Double ;
     FAcao           : string ;
     FFiller         : String ;
     FDoctosLiberados: TDoctosLiberados ;
     FNotasCTe       : TNotasCTe ;
   public
     property IdRegistro : String  read FIdRegistro  write FIdRegistro ;
     property IdPreFatura: String  read FIdPreFatura write FIdPreFatura ;
     property dtEmissao  : TDate   read FdtEmissao   write FdtEmissao ;
     property dtPagamento: TDate   read FdtPagamento write FdtPagamento ;
     property qtdeDoctos : Integer read FqtdeDoctos  write FqtdeDoctos ;
     property vPreFatura : Double  read FvPreFatura  write FvPreFatura ;
     property Acao       : string  read FAcao        write FAcao ;
     property Filler     : String  read FFiller      write FFiller ;

     property DoctosLiberados: TDoctosLiberados read FDoctosLiberados write FDoctosLiberados ;
     property NotasCTe       : TNotasCTe        read FNotasCTe        write FNotasCTe ;
   end;

   { TIdCargas Registro de Identifacação da Carga }
   TInfoPreFatura = class( TObjectList )
   private
     function  GetItem(Index: Integer): TPreFatura;
     procedure SetItem(Index: Integer; Value: TPreFatura);
   public
     function  New: TPreFatura;
     property  Items[Index: Integer]: TPreFatura read GetItem write SetItem; default;
   end;

   { TTotalNotas  Registo de Totalização do Arquivo Registro }
   TTotalPreFat = class
   private
     FIdRegistro: String ;
     FvTotal    : Currency ;
     FqPreFatura: Integer ;
     FFiller    : String ;
   public
     property IdRegistro: String   read FIdRegistro write FIdRegistro ;
     property vTotal    : Currency read FvTotal     write FvTotal ;
     property qPreFatura: Integer  read FqPreFatura write FqPreFatura ;
     property Filler    : String   read FFiller     write FFiller ;
   end;

   { TInfoEmbarcadora Registro de Informações da Embarcadora Registro 501 ou 311 }
   TPagadora = class
   private
     FIdRegistro   : String ;
     FCNPJ         : String ;
     FIE           : String ;
     FRazao        : String ;
     FFiller       : String ;
     FInfoPreFatura: TInfoPreFatura ;
   public
     constructor Create ; reintroduce ;
     destructor  Destroy ; override ;

     property IdRegistro   : String         read FIdRegistro    write FIdRegistro ;
     property CNPJ         : String         read FCNPJ          write FCNPJ ;
     property IE           : String         read FIE            write FIE ;
     property Razao        : String         read FRazao         write FRazao ;
     property InfoPreFatura: TInfoPreFatura read FInfoPreFatura write FInfoPreFatura ;
     property Filler       : String         read FFiller        write FFiller ;
   end;

   { TIdentDocto Registros de Identificação do Documento }
   TInfoPagadora = class( TObjectList )
   private
     function  GetItem(Index: Integer): TPagadora;
     procedure SetItem(Index: Integer; Value: TPagadora);
   public
     function  New: TPagadora;
     property  Items[Index: Integer]: TPagadora read GetItem write SetItem; default;
   end;

   { TDocto Identificação do Documento }
   TDocto = class
   private
     FIdRegistro  : String ;  // Identificador do Registro
     FIdDocto     : String ;
     FFiller      : String ;
     FInfoPagadora: TInfoPagadora ;
     FTotalPreFat: TTotalPreFat ;
   public
     property IdRegistro  : String        read FIdRegistro   write FIdRegistro ;
     property IdDocto     : String        read FIdDocto      write FIdDocto ;
     property Filler      : String        read FFiller       write FFiller ;
     property InfoPagadora: TInfoPagadora read FInfoPagadora write FInfoPagadora ;
     property TotalPreFat : TTotalPreFat  read FTotalPreFat  write FTotalPreFat ;
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
   TACBrEDIPreFatura = class(TACBrComponent)
   private
      FTxt        : TACBrTxtClass ;
      FVersao     : tveEdi ;
      FConteudo   : TStringList ;

      FCabecalho  : TCabecalhoEDI ;
      FInfoDocto  : TInfoDocto ;

      procedure GerarCabecalho ;
      procedure GerarInfoDocto      ( Registro: TDocto ) ;
      procedure GerarPagadora       ( Registro: TInfoPagadora ) ;
      procedure GerarPreFatura      ( Registro: TInfoPreFatura ) ;
      procedure GerarDoctosLiberados( Registro: TDoctosLiberados ) ;
      procedure GerarCalculoFrete   ( Registro: TCalculoFrete ) ;
      procedure GerarNotasCTe       ( Registro: TNotasCTe ) ;
      procedure GerarTotalPreFat    ( Registro: TTotalPreFat ) ;

      procedure LerCabecalho ;
      function  LerCabecalhoDocto ( nRow: Integer ): Integer ;
      function  LerInfoPagadora   ( Registro: TInfoPagadora;    nRow: Integer ): Integer ;
      function  LerPreFatura      ( Registro: TInfoPreFatura;   nRow: Integer ): Integer ;
      function  LerDoctosLiberados( Registro: TDoctosLiberados; nRow: Integer ): Integer ;
      function  LerCalculoFrete   ( Registro: TCalculoFrete;    nRow: Integer ): Integer ;
      function  LerNotasCTe       ( Registro: TNotasCTE;        nRow: Integer ): Integer ;
      function  LerTotalPreFat    ( Registro: TTotalPreFat;     nRow: Integer ): Integer ;
   public
     constructor Create(AOwner: TComponent); Override ;
     destructor  Destroy; Override ;

     property Cabecalho  : TCabecalhoEdi  read FCabecalho    write FCabecalho ;
     property InfoDocto  : TInfoDocto     read FInfoDocto    write FInfoDocto ;
     property Conteudo   : TStringList    read FConteudo     write FConteudo ;
     property Versao     : tveEdi         read FVersao       write FVersao ;

     procedure GravarArquivo(const xArquivo: String);
     procedure LerArquivo(const xArquivo: String);

     procedure LimpaRegistros ;
   end;

implementation

uses
  ACBrUtil.Strings;

{ TNotaFiscais }

procedure TACBrEDIPreFatura.LerCabecalho;
var
  nRow: Integer ;
begin
  nRow := 0 ;

  Cabecalho.IdRegistro   := Copy(Conteudo.Strings[nRow],  1,  3) ;
  Cabecalho.Remetente    := Copy(Conteudo.Strings[nRow],  4, 35) ;
  Cabecalho.Destinatario := Copy(Conteudo.Strings[nRow], 39, 35) ;
  Cabecalho.Data         := StringToDate(Copy(Conteudo.Strings[nRow], 74,  6)) ;
  Cabecalho.Hora         := StringToTime(Copy(Conteudo.Strings[nRow], 80,  4)) ;
  Cabecalho.Id           := Copy(Conteudo.Strings[nRow], 84, 12) ;
  Cabecalho.Filler       := Copy(Conteudo.Strings[nRow], 96, 105) ;

  Inc(nRow) ;
  LerCabecalhoDocto( nRow ) ;
end;

function TACBrEDIPreFatura.LerCabecalhoDocto( nRow: Integer): Integer ;
var
  cReg: string ;
begin
  cReg := Copy(Conteudo.Strings[nRow], 1, 3) ;
  while Copy(Conteudo.Strings[nRow], 1, 3) = cReg do
  begin
    with InfoDocto.New do
    begin
      IdRegistro := Copy(Conteudo.Strings[nRow],  1,  3) ;
      IdDocto    := Copy(Conteudo.Strings[nRow],  4, 14) ;
      Filler     := Copy(Conteudo.Strings[nRow], 18, 183) ;

      Inc(nRow) ;
      nRow := LerTotalPreFat( TotalPreFat, LerInfoPagadora( InfoPagadora, nRow ) ) ;
    end;
    if nRow > (Conteudo.Count - 1) then
      Break ;
 end ;
  result := nRow ;
end;

procedure TACBrEDIPreFatura.LimpaRegistros;
begin
  InfoDocto.Clear ;
end;

function TACBrEDIPreFatura.LerInfoPagadora( Registro: TInfoPagadora; nRow: Integer ): Integer ;
begin
  while Copy(Conteudo.Strings[nRow], 1, 3) = '391' do
  begin
    with Registro.New do
    begin
      IdRegistro := Copy(Conteudo.Strings[nRow],   1,  3) ;
      CNPJ       := Copy(Conteudo.Strings[nRow],  4,  15) ;
      IE         := Copy(Conteudo.Strings[nRow], 19,  15) ;
      Razao      := Copy(Conteudo.Strings[nRow], 34,  40) ;
      Filler     := Copy(Conteudo.Strings[nRow], 74, 127) ;

      Inc(nRow) ;
      nRow := LerPreFatura( InfoPreFatura, nRow ) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDIPreFatura.LerPreFatura( Registro: TInfoPreFatura; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := Copy(Conteudo.Strings[nRow], 1, 3) ;

  while cReg = Copy(Conteudo.Strings[nRow], 1, 3) do
  begin
    with Registro.New do
    begin
      IdRegistro  := Copy(Conteudo.Strings[nRow]               ,  1,   3) ;
      IdPreFatura := Copy(Conteudo.Strings[nRow]               ,  4,  20) ;
      dtEmissao   := StringToDate(Copy(Conteudo.Strings[nRow]  , 24,   8)) ;
      dtPagamento := StringToDate(Copy(Conteudo.Strings[nRow]  , 32,   8)) ;
      qtdeDoctos  := StrToInt(Copy(Conteudo.Strings[nRow]      , 40,   4)) ;
      vPreFatura  := StringToDouble(Copy(Conteudo.Strings[nRow], 44,  15), 13, 2) ;
      Acao        := Copy(Conteudo.Strings[nRow]               , 59,   1) ;
      Filler      := Copy(Conteudo.Strings[nRow]               , 60, 141) ;

      inc(nRow) ;
      nRow        := LerNotasCTe( NotasCTe, LerDoctosLiberados( DoctosLiberados, nRow )) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDIPreFatura.LerDoctosLiberados( Registro: TDoctosLiberados; nRow: Integer ): Integer ;
var
  cReg: String ;
begin
  cReg := Copy(Conteudo.Strings[nRow], 1, 3) ;

  while cReg = Copy(Conteudo.Strings[nRow], 1, 3) do
  begin
    with Registro.New do
    begin
      IdRegistro       := Copy(Conteudo.Strings[nRow]               ,   1,  3) ;
      CnpjEmissor      := Copy(Conteudo.Strings[nRow]               ,   4, 15) ;
      SerieDocto       := Copy(Conteudo.Strings[nRow]               ,  19,  5) ;
      IdDocto          := Copy(Conteudo.Strings[nRow]               ,  24, 20) ;
      dtEmissao        := StringToDate(Copy(Conteudo.Strings[nRow]  ,  44,  8)) ;
      SerieCTe         := Copy(Conteudo.Strings[nRow]               ,  52,  5) ;
      nroCTe           := Copy(Conteudo.Strings[nRow]               ,  57, 12) ;
      dtEmissaoCTe     := StringToDate(Copy(Conteudo.Strings[nRow]  ,  69,  8)) ;
      CnpjOrigem       := Copy(Conteudo.Strings[nRow]               ,  77, 15) ;
      CnpjDestino      := Copy(Conteudo.Strings[nRow]               ,  92, 15) ;
      tpCNPJDestino    := StrToInt(Copy(Conteudo.Strings[nRow]      , 107,  1)) ;
      vFreteEmbarcador := StringToDouble(Copy(Conteudo.Strings[nRow], 108, 15), 13, 2) ;
      vFreteTransporte := StringToDouble(Copy(Conteudo.Strings[nRow], 123, 15), 13, 2) ;
      tpDiferenca      := StrToInt(Copy(Conteudo.Strings[nRow]      , 138,  1)) ;
      vDiferenca       := StringToDouble(Copy(Conteudo.Strings[nRow], 139, 15), 13, 2) ;
      DoctoInterno     := Copy(Conteudo.Strings[nRow]               , 154, 15) ;
      Filler           := Copy(Conteudo.Strings[nRow]               , 169, 32) ;
      Inc(nRow) ;
      nRow := LerCalculoFrete( CalculoFrete, nRow ) ;
    end;
  end;
  result := nRow ;
end;

function TACBrEDIPreFatura.LerCalculoFrete( Registro: TCalculoFrete; nRow: Integer ): Integer ;
begin
  if Copy(Conteudo.Strings[nRow], 1, 3) = '394' then
  begin
    Registro.IdRegistro     := Copy(Conteudo.Strings[nRow]               ,   1,  3) ;
    Registro.vTotFretePeso  := StringToDouble(Copy(Conteudo.Strings[nRow],   4, 15), 13, 2) ;
    Registro.vSecCat        := StringToDouble(Copy(Conteudo.Strings[nRow],  19, 15), 13, 2) ;
    Registro.vItrGris       := StringToDouble(Copy(Conteudo.Strings[nRow],  34, 15), 13, 2) ;
    Registro.vPedagio       := StringToDouble(Copy(Conteudo.Strings[nRow],  49, 15), 13, 2) ;
    Registro.vDiversos      := StringToDouble(Copy(Conteudo.Strings[nRow],  64, 15), 13, 2) ;
    Registro.vDesconto      := StringToDouble(Copy(Conteudo.Strings[nRow],  79, 15), 13, 2) ;
    Registro.vAdemeGris     := StringToDouble(Copy(Conteudo.Strings[nRow],  94, 15), 13, 2) ;
    Registro.pAliqISS       := StringToDouble(Copy(Conteudo.Strings[nRow], 109,  5),  3, 2) ;
    Registro.vISS           := StringToDouble(Copy(Conteudo.Strings[nRow], 114, 15), 13, 2) ;
    Registro.vBCIcms        := StringToDouble(Copy(Conteudo.Strings[nRow], 129, 15), 13, 2) ;
    Registro.pAliqIcms      := StringToDouble(Copy(Conteudo.Strings[nRow], 144,  5),  3, 2) ;
    Registro.vIcms          := StringToDouble(Copy(Conteudo.Strings[nRow], 149, 15), 13, 2) ;
    Registro.vAdValorem     := StringToDouble(Copy(Conteudo.Strings[nRow], 164, 15), 13, 2) ;
    Registro.vDespacho      := StringToDouble(Copy(Conteudo.Strings[nRow], 179, 15), 13, 2) ;
    Registro.qPesoBruto     := StringToDouble(Copy(Conteudo.Strings[nRow], 194,  7),  5, 2) ;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDIPreFatura.LerNotasCTe(Registro: TNotasCTE; nRow: Integer): Integer;
var
  nf: integer ;
begin
  nf := 4 ;
  while Copy(Conteudo.Strings[nRow], 1, 3) = '396' do
  begin
    with Registro.New do
    begin
      IdRegistro := Copy(Conteudo.Strings[nRow],    1,  3) ;
      while trim(Copy(Conteudo.Strings[nRow], nf+3, 8)) <> '' do
      begin
        with InfoNotas.New do
        begin
          SerieNF  := Copy(Conteudo.Strings[nRow],   nf,  3) ;
          NumeroNF := StrToInt(Copy(Conteudo.Strings[nRow], nf+3, 8)) ;
        end;
        nf := nf + 11 ;
      end;
      Filler := Copy(Conteudo.Strings[nRow],  169, 31) ;
    end;
    Inc(nRow) ;
  end;
  result := nRow ;
end;

function TACBrEDIPreFatura.LerTotalPreFat( Registro: TTotalPreFat; nRow: Integer ): Integer ;
begin
  with Registro do
  begin
    IdRegistro := Copy(Conteudo.Strings[nRow]               ,  1,  3) ;
    vTotal     := StringToDouble(Copy(Conteudo.Strings[nRow],  4, 15), 13, 2) ;
    qPreFatura := StrToInt(Copy(Conteudo.Strings[nRow]      , 19, 15)) ;
    Filler     := Copy(Conteudo.Strings[nRow]               , 34, 167) ;
    inc(nRow) ;
  end;
  result := nRow ;
end;

constructor TACBrEDIPreFatura.Create(AOwner: TComponent);
begin
  inherited Create(AOwner) ;
  FTxt         := TACBrTxtClass.Create ;
  FConteudo    := TStringList.Create ;
  FCabecalho   := TCabecalhoEdi.Create ;
  FInfoDocto   := TInfoDocto.Create ;

  FVersao      := ve50 ;
end;

destructor TACBrEDIPreFatura.Destroy;
begin
  FTxt.Free ;
  FConteudo.Free ;
  FCabecalho.Free ;
  FInfoDocto.Free ;
  inherited ;
end;

procedure TACBrEDIPreFatura.LerArquivo( const xArquivo: String );
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
  Result.FInfoPagadora := TInfoPagadora.Create ;
  Result.FTotalPreFat  := TTotalPreFat.Create ;
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

{ TInfoPagadora }

function TInfoPagadora.GetItem(Index: Integer): TPagadora;
begin
  Result := TPagadora(inherited GetItem(Index)) ;
end;

function TInfoPagadora.New: TPagadora;
begin
  Result := TPagadora.Create;
  Result.InfoPreFatura := TInfoPreFatura.Create ;
  Add(Result);
end;

procedure TInfoPagadora.SetItem(Index: Integer; Value: TPagadora);
begin
  Put(Index, Value) ;
end;

{ TPagadora }

constructor TPagadora.Create;
begin
  inherited Create ;
end;

destructor TPagadora.Destroy;
begin
  inherited Destroy;
end;

{ TNotasCTe }

function TNotasCTe.GetItem(Index: Integer): TIdNotas;
begin
  Result := TIdNotas(inherited GetItem(Index)) ;
end;

function TNotasCTe.New: TIdNotas;
begin
  Result := TIdNotas.Create;
  Result.InfoNotas := TNFS.Create ;
  Add(Result);
end;

procedure TNotasCTe.SetItem(Index: Integer; Value: TIdNotas);
begin
  Put(Index, Value) ;
end;

{ TInfoPreFatura }

function TInfoPrefatura.GetItem(Index: Integer): TPreFatura;
begin
  Result := TPreFatura(inherited GetItem(Index)) ;
end;

function TInfoPrefatura.New: TPreFatura;
begin
  Result := TPreFatura.Create ;
  Result.DoctosLiberados := TDoctosLiberados.Create ;
  Result.NotasCTe        := TNotasCTE.Create ;
  Add(Result) ;
end;

procedure TInfoPrefatura.SetItem(Index: Integer; Value: TPreFatura);
begin
  Put(Index, Value);
end;

{ TDoctosLiberados }

function TDoctosLiberados.GetItem(Index: Integer): TLiberados;
begin
  Result := TLiberados(inherited GetItem(Index)) ;
end;

function TDoctosLiberados.New: TLiberados;
begin
  Result := TLiberados.Create ;
  Result.CalculoFrete := TCalculoFrete.Create ;
  Add(Result) ;
end;

procedure TDoctosLiberados.SetItem(Index: Integer; Value: TLiberados);
begin
  Put(Index, Value);
end;

procedure TACBrEDIPreFatura.GravarArquivo( const xArquivo: String ) ;
begin
  Conteudo.Clear ;

  GerarCabecalho ;
  Conteudo.SaveToFile( xArquivo ) ;

  Conteudo.Clear ;
end;

procedure TACBrEDIPreFatura.GerarCabecalho ;
var
  i: Integer ;
begin
  // Registro 000 cabeçalho do pré-fatura
  if Cabecalho.Id = '' then
    Cabecalho.Id := 'PRE' +
                     Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                     Copy(OnlyNumber(TimeToStr(FCabecalho.Hora)),1,4)      +
                     IntToStr(Cabecalho.Sequencia) ;

  Conteudo.Add( Cabecalho.IdRegistro +
                FTxt.RFill(Cabecalho.Remetente                  , 35) +
                FTxt.RFill(Cabecalho.Destinatario               , 35) +
                FTxt.LFill(Cabecalho.Data                       , 'ddmmyy', false)+
                FTxt.RFill(OnlyNumber(TimeToStr(Cabecalho.Hora)),  4, '0') +
                FTxt.RFill(Cabecalho.Id                         , 12) +
                FTxt.RFill(Cabecalho.Filler, 105) ) ;

  for i := 0 to InfoDocto.Count - 1 do
    GerarInfoDocto( InfoDocto.Items[i] ) ;

end;

procedure TACBrEDIPreFatura.GerarInfoDocto( Registro: TDocto ) ;
begin
  // Registro 390 identificação do pré-fatura
  if Registro.IdDocto = '' then
    Registro.IdDocto := 'PREFA' +
                        Copy(FTxt.LFill(Cabecalho.Data, 'ddmmyy', false),1,4) +
                        Copy(OnlyNumber(TimeToStr(Cabecalho.Hora)),1,4)       +
                        IntToStr(Cabecalho.Sequencia) ;

  Conteudo.Add( Registro.IdRegistro +
                FTxt.RFill(Registro.IdDocto, 14) +
                FTxt.RFill(Registro.Filler, 127) ) ;

  GerarPagadora( Registro.InfoPagadora )  ;
  GerarTotalPreFat( Registro.TotalPreFat ) ;
end;

procedure TACBrEDIPreFatura.GerarPagadora( Registro: TInfoPagadora ) ;
var
  p: integer ;
begin
  // Registro 391 identificação da empresa pagadora
  for p := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[p] do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.RFill( OnlyNumber( CNPJ ), 15) +
                    FTxt.RFill( OnlyNumber( IE )  , 15) +
                    FTxt.RFill( Razao             , 40) +
                    FTxt.RFill( Filler            , 127 ) ) ;
    end;
    GerarPreFatura( Registro.Items[p].InfoPreFatura )  ;
  end;
end;

procedure TACBrEDIPreFatura.GerarPreFatura( Registro: TInfoPreFatura ) ;
var
  p: integer ;
begin
  // Registro 392 IDENTIFICA O DOCUMENTO DE PAGAMENTO DO FRETE
  for p := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[p] do
    begin
      Conteudo.Add( IdRegistro + FTxt.RFill( IdPreFatura,  20) +
                                 FTxt.LFill( dtEmissao  , 'ddmmyyyy', false)+
                                 FTxt.LFill( dtPagamento, 'ddmmyyyy', false)+
                                 FTxt.VLFill(qtdeDoctos ,   4, 0, '0') +
                                 FTxt.VLFill(vPreFatura ,  15, 2, '0') +
                                 FTxt.RFill( Acao       ,   1) +
                                 FTxt.RFill( Filler     , 141) ) ;
      GerarDoctosLiberados( DoctosLiberados ) ;
      GerarNotasCTe( NotasCTe ) ;
    end;
  end;
end;

procedure TACBrEDIPreFatura.GerarDoctosLiberados( Registro: TDoctosLiberados ) ;
var
  d: integer ;
begin
  // Registro 393 IDENTIFICA OS DOCUMENTOS LIBERADOS PARA O PAGAMENTO DO FRETE
  for d := 0 to Registro.Count - 1 do
  begin
    with Registro.Items[d] do
    begin
      Conteudo.Add( IdRegistro +
                    FTxt.RFill ( OnlyNumber(CnpjEmissor), 15) +
                    FTxt.RFill ( SerieDocto             ,  5) +
                    FTxt.RFill ( IdDocto                , 20) +
                    FTxt.LFill ( dtEmissao              , 'ddmmyyyy', false)+
                    FTxt.RFill ( SerieCTe               ,  5) +
                    FTxt.RFill ( nroCTe                 , 12) +
                    FTxt.LFill ( dtEmissaoCTe           , 'ddmmyyyy', false)+
                    FTxt.RFill ( OnlyNumber(CnpjOrigem) , 15) +
                    FTxt.RFill ( OnlyNumber(CnpjDestino), 15) +
                    FTxt.LFill ( tpCNPJDestino          ,  1) +
                    FTxt.VLFill( vFreteEmbarcador       , 15, 2, '0') +
                    FTxt.VLFill( vFreteTransporte       , 15, 2, '0') +
                    FTxt.LFill ( tpDiferenca            ,  1) +
                    FTxt.VLFill( vDiferenca             , 15, 2, '0') +
                    FTxt.LFill ( DoctoInterno           , 15) +
                    FTxt.LFill ( Filler                 , 32) ) ;
      GerarCalculoFrete( CalculoFrete ) ;
    end;
  end;
end;

procedure TACBrEDIPreFatura.GerarCalculoFrete( Registro: TCalculoFrete ) ;
begin
  // Registro 394 INDICA OS VALORES QUE COMPÕEM O CÁLCULO DO VALOR DO FRETE
  if Registro.IdRegistro <> '' then
  begin
    Conteudo.Add( Registro.IdRegistro +
                  FTxt.VLFill( Registro.vTotFretePeso, 15, 2, '0') +
                  FTxt.VLFill( Registro.vSecCat      , 15, 2, '0') +
                  FTxt.VLFill( Registro.vItrGris     , 15, 2, '0') +
                  FTxt.VLFill( Registro.vPedagio     , 15, 2, '0') +
                  FTxt.VLFill( Registro.vDiversos    , 15, 2, '0') +
                  FTxt.VLFill( Registro.vDesconto    , 15, 2, '0') +
                  FTxt.VLFill( Registro.vAdemeGris   , 15, 2, '0') +
                  FTxt.VLFill( Registro.pAliqISS     ,  5, 2, '0') +
                  FTxt.VLFill( Registro.vISS         , 15, 2, '0') +
                  FTxt.VLFill( Registro.vBCIcms      , 15, 2, '0') +
                  FTxt.VLFill( Registro.pAliqIcms    ,  5, 2, '0') +
                  FTxt.VLFill( Registro.vIcms        , 15, 2, '0') +
                  FTxt.VLFill( Registro.vAdValorem   , 15, 2, '0') +
                  FTxt.VLFill( Registro.vDespacho    , 15, 2, '0') +
                  FTxt.VLFill( Registro.qPesoBruto   ,  7, 2, '0') ) ;
  end;
end;

procedure TACBrEDIPreFatura.GerarNotasCTe(Registro: TNotasCTE) ;
var
  n, n1: integer ;
  nf: string ;
begin
  // Registro 396 RELACIONA AS NOTAS FISCAIS CONTIDAS NO CONHECIMNETO LIBERADO
  for n := 0 to Registro.Count - 1 do
  begin
    nf := '' ;
    with Registro.Items[n] do
    begin
      nf := nf + FTxt.RFill( IdRegistro, 3) ;
      for n1 := 0 to InfoNotas.Count - 1 do
      begin
        nf := nf + FTxt.RFill( InfoNotas.Items[n1].SerieNF, 3) +
            FTxt.LFill( InfoNotas.Items[n1].NumeroNF, 8, false) ;
      end;
      nf := Ftxt.RFill(nf, 169) + Ftxt.RFill( Filler, 31) ;
    end;
    Conteudo.Add(nf) ;
  end;
end;

procedure TACBrEDIPreFatura.GerarTotalPreFat( Registro: TTotalPreFat ) ;
begin
  // Registro 399 TOTALIZA A QUANTIDADE DE VALORES DOS DOCUMENTOS DE PRÉ FATURA
  if Registro.IdRegistro = '' then
  begin
    raise Exception.Create('Registro Totalizador do Documento de Pre-Fatura não Informado...,'+#13+
                           'esta informação é obrigatória !!') ;
  end ;

  Conteudo.Add( Registro.IdRegistro +
                FTxt.VLFill(Registro.vTotal     , 15, 2, '0') +
                FTxt.VLFill(Registro.qPreFatura , 15, 2, '0') +
                FTxt.RFill (Registro.FFiller    , 167) ) ;
end;

{ TNFS }

function TNFS.GetItem(Index: Integer): TNotas;
begin
  Result := TNotas(inherited GetItem(Index)) ;
end;

function TNFS.New: TNotas;
begin
  Result := TNotas.Create;
  Add(Result);
end;

procedure TNFS.SetItem(Index: Integer; Value: TNotas);
begin
  Put(Index, Value) ;
end;

end.
