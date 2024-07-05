{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro e Claudio Roberto               }
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

unit ACBrEPCBloco_C_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEPCBloco_C, ACBrEPCBlocos,
     ACBrEPCBloco_0_Class;

type
  TWriteRegistroC481Event = procedure(const AC481: TObject;
                                      var ANullVL_BC_PIS,
                                          ANullALIQ_PIS,
                                          ANullQUANT_BC_PIS,
                                          ANullALIQ_PIS_QUANT,
                                          ANullVL_PIS: Boolean) of object;
  TWriteRegistroC485Event = procedure(const AC485: TObject;
                                      var ANullVL_BC_COFINS,
                                          ANullALIQ_COFINS,
                                          ANullQUANT_BC_COFINS,
                                          ANullALIQ_COFINS_QUANT,
                                          ANullVL_COFINS: Boolean) of object;
  TWriteRegistroC491Event = procedure(const AC491: TObject;
                                      var ANullVL_BC_PIS,
                                          ANullALIQ_PIS,
                                          ANullQUANT_BC_PIS,
                                          ANullALIQ_PIS_QUANT,
                                          ANullVL_PIS: Boolean) of object;
  TWriteRegistroC495Event = procedure(const AC495: TObject;
                                      var ANullVL_BC_COFINS,
                                          ANullALIQ_COFINS,
                                          ANullQUANT_BC_COFINS,
                                          ANullALIQ_COFINS_QUANT,
                                          ANullVL_COFINS: Boolean) of object;
  { TBloco_C }
  TBloco_C = class(TACBrSPED)
  private
    FRegistroC001: TRegistroC001;      /// BLOCO 0 - Registro0001
    FRegistroC990: TRegistroC990;      /// BLOCO 0 - Registro0990

    FRegistroC010Count: Integer;
    FRegistroC100Count: Integer;
    FRegistroC110Count: Integer;
    FRegistroC111Count: Integer;
    FRegistroC120Count: Integer;
    FRegistroC170Count: Integer;
    FRegistroC175Count: Integer;
    FRegistroC180Count: Integer;
    FRegistroC181Count: Integer;
    FRegistroC185Count: Integer;
    FRegistroC188Count: Integer;
    FRegistroC190Count: Integer;
    FRegistroC191Count: Integer;
    FRegistroC195Count: Integer;
    FRegistroC198Count: Integer;
    FRegistroC199Count: Integer;
    FRegistroC380Count: Integer;
    FRegistroC381Count: Integer;
    FRegistroC385Count: Integer;
    FRegistroC395Count: Integer;
    FRegistroC396Count: Integer;
    FRegistroC400Count: Integer;
    FRegistroC405Count: Integer;
    FRegistroC481Count: Integer;
    FRegistroC485Count: Integer;
    FRegistroC489Count: Integer;
    FRegistroC490Count: Integer;
    FRegistroC491Count: Integer;
    FRegistroC495Count: Integer;
    FRegistroC499Count: Integer;
    FRegistroC500Count: Integer;
    FRegistroC501Count: Integer;
    FRegistroC505Count: Integer;
    FRegistroC509Count: Integer;
    FRegistroC600Count: Integer;
    FRegistroC601Count: Integer;
    FRegistroC605Count: Integer;
    FRegistroC609Count: Integer;
    FRegistroC800Count: integer;
    FRegistroC810Count: integer;
    FRegistroC820Count: integer;
    FRegistroC830Count: integer;
    FRegistroC860Count: integer;
    FRegistroC870Count: integer;
    FRegistroC880Count: integer;
    FRegistroC890Count: integer;

    FBloco_0: TBloco_0;

    procedure WriteRegistroC010(RegC001: TRegistroC001);
    procedure WriteRegistroC100(RegC010: TRegistroC010);
    procedure WriteRegistroC110(RegC100: TRegistroC100);
    procedure WriteRegistroC111(RegC100: TRegistroC100);
    procedure WriteRegistroC120(RegC100: TRegistroC100);
    procedure WriteRegistroC170(RegC100: TRegistroC100);
    procedure WriteRegistroC175(RegC100: TRegistroC100);
    procedure WriteRegistroC180(RegC010: TRegistroC010);
    procedure WriteRegistroC181(RegC180: TRegistroC180);
    procedure WriteRegistroC185(RegC180: TRegistroC180);
    procedure WriteRegistroC188(RegC180: TRegistroC180);
    procedure WriteRegistroC190(RegC010: TRegistroC010);
    procedure WriteRegistroC191(RegC190: TRegistroC190);
    procedure WriteRegistroC195(RegC190: TRegistroC190);
    procedure WriteRegistroC198(RegC190: TRegistroC190);
    procedure WriteRegistroC199(RegC190: TRegistroC190);
    procedure WriteRegistroC380(RegC010: TRegistroC010);
    procedure WriteRegistroC381(RegC380: TRegistroC380);
    procedure WriteRegistroC385(RegC380: TRegistroC380);
    procedure WriteRegistroC395(RegC010: TRegistroC010);
    procedure WriteRegistroC396(RegC395: TRegistroC395);
    procedure WriteRegistroC400(RegC010: TRegistroC010);
    procedure WriteRegistroC405(RegC400: TRegistroC400);
    procedure WriteRegistroC481(RegC405: TRegistroC405);
    procedure WriteRegistroC485(RegC405: TRegistroC405);
    procedure WriteRegistroC489(RegC400: TRegistroC400);
    procedure WriteRegistroC490(RegC010: TRegistroC010);
    procedure WriteRegistroC491(RegC490: TRegistroC490);
    procedure WriteRegistroC495(RegC490: TRegistroC490);
    procedure WriteRegistroC499(RegC490: TRegistroC490);
    procedure WriteRegistroC500(RegC010: TRegistroC010);
    procedure WriteRegistroC501(RegC500: TRegistroC500);
    procedure WriteRegistroC505(RegC500: TRegistroC500);
    procedure WriteRegistroC509(RegC500: TRegistroC500);
    procedure WriteRegistroC600(RegC010: TRegistroC010);
    procedure WriteRegistroC601(RegC600: TRegistroC600);
    procedure WriteRegistroC605(RegC600: TRegistroC600);
    procedure WriteRegistroC609(RegC600: TRegistroC600);
    procedure WriteRegistroC860(RegC010: TRegistroC010);
    procedure WriteRegistroC870(RegC860: TRegistroC860);
    procedure WriteRegistroC880(RegC860: TRegistroC860);
    procedure WriteRegistroC890(RegC860: TRegistroC860);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  protected
    FOnBeforeWriteRegistroC481: TWriteRegistroC481Event;
    FOnBeforeWriteRegistroC485: TWriteRegistroC485Event;
    FOnBeforeWriteRegistroC491: TWriteRegistroC491Event;
    FOnBeforeWriteRegistroC495: TWriteRegistroC495Event;
  public
    constructor Create ;          /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function RegistroC001New: TRegistroC001;
    function RegistroC010New: TRegistroC010;
    function RegistroC100New: TRegistroC100;
    function RegistroC110New: TRegistroC110;
    function RegistroC111New: TRegistroC111;
    function RegistroC120New: TRegistroC120;
    function RegistroC170New: TRegistroC170;
    function RegistroC175New: TRegistroC175;
    function RegistroC180New: TRegistroC180;
    function RegistroC181New: TRegistroC181;
    function RegistroC185New: TRegistroC185;
    function RegistroC188New: TRegistroC188;
    function RegistroC190New: TRegistroC190;
    function RegistroC191New: TRegistroC191;
    function RegistroC195New: TRegistroC195;
    function RegistroC198New: TRegistroC198;
    function RegistroC199New: TRegistroC199;
    function RegistroC380New: TRegistroC380;
    function RegistroC381New: TRegistroC381;
    function RegistroC385New: TRegistroC385;
    function RegistroC395New: TRegistroC395;
    function RegistroC396New: TRegistroC396;
    function RegistroC400New: TRegistroC400;
    function RegistroC405New: TRegistroC405;
    function RegistroC481New: TRegistroC481;
    function RegistroC485New: TRegistroC485;
    function RegistroC489New: TRegistroC489;
    function RegistroC490New: TRegistroC490;
    function RegistroC491New: TRegistroC491;
    function RegistroC495New: TRegistroC495;
    function RegistroC499New: TRegistroC499;
    function RegistroC500New: TRegistroC500;
    function RegistroC501New: TRegistroC501;
    function RegistroC505New: TRegistroC505;
    function RegistroC509New: TRegistroC509;
    function RegistroC600New: TRegistroC600;
    function RegistroC601New: TRegistroC601;
    function RegistroC605New: TRegistroC605;
    function RegistroC609New: TRegistroC609;

    function RegistroC800New: TRegistroC800;
    function RegistroC810New: TRegistroC810;
    function RegistroC820New: TRegistroC820;
    function RegistroC830New: TRegistroC830;
    function RegistroC860New: TRegistroC860;
    function RegistroC870New: TRegistroC870;
    function RegistroC880New: TRegistroC880;
    function RegistroC890New: TRegistroC890;

    procedure WriteRegistroC001 ;
    procedure WriteRegistroC990 ;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroC001: TRegistroC001 read FRegistroC001 write FRegistroC001;
    property RegistroC990: TRegistroC990 read FRegistroC990 write FRegistroC990;

    property RegistroC010Count: Integer read FRegistroC010Count write FRegistroC010Count;
    property RegistroC100Count: Integer read FRegistroC100Count write FRegistroC100Count;
    property RegistroC110Count: Integer read FRegistroC110Count write FRegistroC110Count;
    property RegistroC111Count: Integer read FRegistroC111Count write FRegistroC111Count;
    property RegistroC120Count: Integer read FRegistroC120Count write FRegistroC120Count;
    property RegistroC170Count: Integer read FRegistroC170Count write FRegistroC170Count;
    property RegistroC175Count: Integer read FRegistroC175Count write FRegistroC175Count;
    property RegistroC180Count: Integer read FRegistroC180Count write FRegistroC180Count;
    property RegistroC181Count: Integer read FRegistroC181Count write FRegistroC181Count;
    property RegistroC185Count: Integer read FRegistroC185Count write FRegistroC185Count;
    property RegistroC188Count: Integer read FRegistroC188Count write FRegistroC188Count;
    property RegistroC190Count: Integer read FRegistroC190Count write FRegistroC190Count;
    property RegistroC191Count: Integer read FRegistroC191Count write FRegistroC191Count;
    property RegistroC195Count: Integer read FRegistroC195Count write FRegistroC195Count;
    property RegistroC198Count: Integer read FRegistroC198Count write FRegistroC198Count;
    property RegistroC199Count: Integer read FRegistroC199Count write FRegistroC199Count;
    property RegistroC380Count: Integer read FRegistroC380Count write FRegistroC380Count;
    property RegistroC381Count: Integer read FRegistroC381Count write FRegistroC381Count;
    property RegistroC385Count: Integer read FRegistroC385Count write FRegistroC385Count;
    property RegistroC395Count: Integer read FRegistroC395Count write FRegistroC395Count;
    property RegistroC396Count: Integer read FRegistroC396Count write FRegistroC396Count;
    property RegistroC400Count: Integer read FRegistroC400Count write FRegistroC400Count;
    property RegistroC405Count: Integer read FRegistroC405Count write FRegistroC405Count;
    property RegistroC481Count: Integer read FRegistroC481Count write FRegistroC481Count;
    property RegistroC485Count: Integer read FRegistroC485Count write FRegistroC485Count;
    property RegistroC489Count: Integer read FRegistroC489Count write FRegistroC489Count;
    property RegistroC490Count: Integer read FRegistroC490Count write FRegistroC490Count;
    property RegistroC491Count: Integer read FRegistroC491Count write FRegistroC491Count;
    property RegistroC495Count: Integer read FRegistroC495Count write FRegistroC495Count;
    property RegistroC499Count: Integer read FRegistroC499Count write FRegistroC499Count;
    property RegistroC500Count: Integer read FRegistroC500Count write FRegistroC500Count;
    property RegistroC501Count: Integer read FRegistroC501Count write FRegistroC501Count;
    property RegistroC505Count: Integer read FRegistroC505Count write FRegistroC505Count;
    property RegistroC509Count: Integer read FRegistroC509Count write FRegistroC509Count;
    property RegistroC600Count: Integer read FRegistroC600Count write FRegistroC600Count;
    property RegistroC601Count: Integer read FRegistroC601Count write FRegistroC601Count;
    property RegistroC605Count: Integer read FRegistroC605Count write FRegistroC605Count;
    property RegistroC609Count: Integer read FRegistroC609Count write FRegistroC609Count;
    property RegistroC800Count: Integer read FRegistroC800Count write FRegistroC800Count;
    property RegistroC810Count: Integer read FRegistroC810Count write FRegistroC810Count;
    property RegistroC820Count: Integer read FRegistroC820Count write FRegistroC820Count;
    property RegistroC830Count: Integer read FRegistroC830Count write FRegistroC830Count;
    property RegistroC860Count: Integer read FRegistroC860Count write FRegistroC860Count;
    property RegistroC870Count: Integer read FRegistroC870Count write FRegistroC870Count;
    property RegistroC880Count: Integer read FRegistroC880Count write FRegistroC880Count;
    property RegistroC890Count: Integer read FRegistroC890Count write FRegistroC890Count;

    property OnBeforeWriteRegistroC481: TWriteRegistroC481Event read FOnBeforeWriteRegistroC481 write FOnBeforeWriteRegistroC481;
    property OnBeforeWriteRegistroC485: TWriteRegistroC485Event read FOnBeforeWriteRegistroC485 write FOnBeforeWriteRegistroC485;
    property OnBeforeWriteRegistroC491: TWriteRegistroC491Event read FOnBeforeWriteRegistroC491 write FOnBeforeWriteRegistroC491;
    property OnBeforeWriteRegistroC495: TWriteRegistroC495Event read FOnBeforeWriteRegistroC495 write FOnBeforeWriteRegistroC495;

    property OnAfterWriteRegistroC481: TWriteRegistroC481Event read FOnBeforeWriteRegistroC481 write FOnBeforeWriteRegistroC481;
    property OnAfterWriteRegistroC485: TWriteRegistroC485Event read FOnBeforeWriteRegistroC485 write FOnBeforeWriteRegistroC485;
    property OnAfterWriteRegistroC491: TWriteRegistroC491Event read FOnBeforeWriteRegistroC491 write FOnBeforeWriteRegistroC491;
    property OnAfterWriteRegistroC495: TWriteRegistroC495Event read FOnBeforeWriteRegistroC495 write FOnBeforeWriteRegistroC495;
  end;

implementation

uses ACBrTXTUtils;

{ TBloco_A }

constructor TBloco_C.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_C.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_C.CriaRegistros;
begin
  FRegistroC001           := TRegistroC001.Create;
  FRegistroC990           := TRegistroC990.Create;

  FRegistroC010Count      := 0;
  FRegistroC100Count      := 0;
  FRegistroC110Count      := 0;
  FRegistroC111Count      := 0;
  FRegistroC120Count      := 0;
  FRegistroC170Count      := 0;
  FRegistroC175Count      := 0;
  FRegistroC180Count      := 0;
  FRegistroC181Count      := 0;
  FRegistroC185Count      := 0;
  FRegistroC188Count      := 0;
  FRegistroC190Count      := 0;
  FRegistroC191Count      := 0;
  FRegistroC195Count      := 0;
  FRegistroC198Count      := 0;
  FRegistroC199Count      := 0;
  FRegistroC380Count      := 0;
  FRegistroC381Count      := 0;
  FRegistroC385Count      := 0;
  FRegistroC395Count      := 0;
  FRegistroC396Count      := 0;
  FRegistroC400Count      := 0;
  FRegistroC405Count      := 0;
  FRegistroC481Count      := 0;
  FRegistroC485Count      := 0;
  FRegistroC489Count      := 0;
  FRegistroC490Count      := 0;
  FRegistroC491Count      := 0;
  FRegistroC495Count      := 0;
  FRegistroC499Count      := 0;
  FRegistroC500Count      := 0;
  FRegistroC501Count      := 0;
  FRegistroC505Count      := 0;
  FRegistroC509Count      := 0;
  FRegistroC600Count      := 0;
  FRegistroC601Count      := 0;
  FRegistroC605Count      := 0;
  FRegistroC609Count      := 0;
  FRegistroC800Count      := 0;
  FRegistroC810Count      := 0;
  FRegistroC820Count      := 0;
  FRegistroC830Count      := 0;
  FRegistroC860Count      := 0;
  FRegistroC870Count      := 0;
  FRegistroC880Count      := 0;
  FRegistroC890Count      := 0;

  FRegistroC990.QTD_LIN_C := 0;
end;

procedure TBloco_C.LiberaRegistros;
begin
  FRegistroC001.Free;
  FRegistroC990.Free;
end;

procedure TBloco_C.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_C.RegistroC001New: TRegistroC001;
begin
   Result := FRegistroC001;
end;

function TBloco_C.RegistroC010New: TRegistroC010;
begin
   Result := FRegistroC001.RegistroC010.New;
end;

function TBloco_C.RegistroC100New: TRegistroC100;
begin
   Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC100.New;
end;

function TBloco_C.RegistroC110New: TRegistroC110;
  var
    C010Count : integer;
    C100Count : integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C100Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Items[C100Count].RegistroC110.New;
end;

function TBloco_C.RegistroC111New: TRegistroC111;
  var
    C010Count: integer;
    C100Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C100Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Items[C100Count].RegistroC111.New;
end;

function TBloco_C.RegistroC120New: TRegistroC120;
  var
    C010Count: integer;
    C100Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C100Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Items[C100Count].RegistroC120.New;
end;

function TBloco_C.RegistroC170New: TRegistroC170;
var
  C010Count: integer;
  C100Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C100Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Items[C100Count].RegistroC170.New;
end;

function TBloco_C.RegistroC175New: TRegistroC175;
var
  C010Count: integer;
  C100Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C100Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC100.Items[C100Count].RegistroC175.New;
end;

function TBloco_C.RegistroC180New: TRegistroC180;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC180.New;
end;

function TBloco_C.RegistroC181New: TRegistroC181;
  var
    C010Count: integer;
    C180Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C180Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC180.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC180.Items[C180Count].RegistroC181.New;
end;

function TBloco_C.RegistroC185New: TRegistroC185;
  var
    C010Count: integer;
    C180Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C180Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC180.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC180.Items[C180Count].RegistroC185.New;
end;

function TBloco_C.RegistroC188New: TRegistroC188;
  var
    C010Count: integer;
    C180Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C180Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC180.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC180.Items[C180Count].RegistroC188.New;
end;

function TBloco_C.RegistroC190New: TRegistroC190;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC190.New;
end;

function TBloco_C.RegistroC191New: TRegistroC191;
  var
    C010Count: integer;
    C190Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C190Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Count -1;
   //
   Result := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Items[C190Count].RegistroC191.New;
end;

function TBloco_C.RegistroC195New: TRegistroC195;
  var
    C010Count: integer;
    C190Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C190Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Items[C190Count].RegistroC195.New;
end;

function TBloco_C.RegistroC198New: TRegistroC198;
  var
    C010Count: integer;
    C190Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C190Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Items[C190Count].RegistroC198.New;
end;

function TBloco_C.RegistroC199New: TRegistroC199;
  var
    C010Count: integer;
    C190Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C190Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC190.Items[C190Count].RegistroC199.New;
end;

function TBloco_C.RegistroC380New: TRegistroC380;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC380.New;
end;

function TBloco_C.RegistroC381New: TRegistroC381;
  var
    C010Count: integer;
    C380Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C380Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC380.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC380.Items[C380Count].RegistroC381.New;
end;

function TBloco_C.RegistroC385New: TRegistroC385;
  var
    C010Count: integer;
    C380Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C380Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC380.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC380.Items[C380Count].RegistroC385.New;
end;

function TBloco_C.RegistroC395New: TRegistroC395;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC395.New;
end;

function TBloco_C.RegistroC396New: TRegistroC396;
  var
    C010Count: integer;
    C395Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C395Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC395.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC395.Items[C395Count].RegistroC396.New;
end;

function TBloco_C.RegistroC400New: TRegistroC400;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC400.New;
end;

function TBloco_C.RegistroC405New: TRegistroC405;
  var
    C010Count: integer;
    C400Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C400Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Items[C400Count].RegistroC405.New;
end;

function TBloco_C.RegistroC481New: TRegistroC481;
  var
    C010Count: integer;
    C400Count: integer;
    C405Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C400Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Items[C400Count].RegistroC405.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC481.New;
end;

function TBloco_C.RegistroC485New: TRegistroC485;
  var
    C010Count: integer;
    C400Count: integer;
    C405Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C400Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Items[C400Count].RegistroC405.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC485.New;
end;

function TBloco_C.RegistroC489New: TRegistroC489;
  var
    C010Count: integer;
    C400Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C400Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC400.Items[C400Count].RegistroC489.New;
end;

function TBloco_C.RegistroC490New: TRegistroC490;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC490.New;
end;

function TBloco_C.RegistroC491New: TRegistroC491;
  var
    C010Count: integer;
    C490Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C490Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC490.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC490.Items[C490Count].RegistroC491.New;
end;

function TBloco_C.RegistroC495New: TRegistroC495;
  var
    C010Count: integer;
    C490Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C490Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC490.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC490.Items[C490Count].RegistroC495.New;
end;

function TBloco_C.RegistroC499New: TRegistroC499;
  var
    C010Count: integer;
    C490Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C490Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC490.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC490.Items[C490Count].RegistroC499.New;
end;

function TBloco_C.RegistroC500New: TRegistroC500;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC500.New;
end;

function TBloco_C.RegistroC501New: TRegistroC501;
  var
    C010Count: integer;
    C500Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C500Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC500.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC500.Items[C500Count].RegistroC501.New;
end;

function TBloco_C.RegistroC505New: TRegistroC505;
  var
    C010Count: integer;
    C500Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C500Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC500.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC500.Items[C500Count].RegistroC505.New;
end;

function TBloco_C.RegistroC509New: TRegistroC509;
  var
    C010Count: integer;
    C500Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C500Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC500.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC500.Items[C500Count].RegistroC509.New;
end;

function TBloco_C.RegistroC600New: TRegistroC600;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC600.New;
end;

function TBloco_C.RegistroC601New: TRegistroC601;
  var
    C010Count: integer;
    C600Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C600Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC600.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC600.Items[C600Count].RegistroC601.New;
end;

function TBloco_C.RegistroC605New: TRegistroC605;
  var
    C010Count: integer;
    C600Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C600Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC600.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC600.Items[C600Count].RegistroC605.New;
end;

function TBloco_C.RegistroC609New: TRegistroC609;
  var
    C010Count: integer;
    C600Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C600Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC600.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC600.Items[C600Count].RegistroC609.New;
end;


function TBloco_C.RegistroC800New: TRegistroC800;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC800.New;
end;

function TBloco_C.RegistroC810New: TRegistroC810;
  var
    C010Count: integer;
    C800Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C800Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC800.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC800.Items[C800Count].RegistroC810.New;
end;

function TBloco_C.RegistroC820New: TRegistroC820;
  var
    C010Count: integer;
    C800Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C800Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC800.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC800.Items[C800Count].RegistroC820.New;
end;

function TBloco_C.RegistroC830New: TRegistroC830;
  var
    C010Count: integer;
    C800Count: integer;
begin
   C010Count := FRegistroC001.RegistroC010.Count -1;
   C800Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC800.Count -1;
   //
   Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC800.Items[C800Count].RegistroC830.New;
end;

function TBloco_C.RegistroC860New: TRegistroC860;
begin
  Result := FRegistroC001.RegistroC010.Items[FRegistroC001.RegistroC010.Count -1].RegistroC860.New;
end;

function TBloco_C.RegistroC870New: TRegistroC870;
var
  C010Count: integer;
  C860Count: integer;
begin
 C010Count := FRegistroC001.RegistroC010.Count -1;
 C860Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC860.Count -1;
 //
 Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC860.Items[C860Count].RegistroC870.New;
end;

function TBloco_C.RegistroC880New: TRegistroC880;
var
  C010Count: integer;
  C860Count: integer;
begin
 C010Count := FRegistroC001.RegistroC010.Count -1;
 C860Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC860.Count -1;
 //
 Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC860.Items[C860Count].RegistroC880.New;
end;

function TBloco_C.RegistroC890New: TRegistroC890;
var
  C010Count: integer;
  C860Count: integer;
begin
 C010Count := FRegistroC001.RegistroC010.Count -1;
 C860Count := FRegistroC001.RegistroC010.Items[C010Count].RegistroC860.Count -1;
 //
 Result    := FRegistroC001.RegistroC010.Items[C010Count].RegistroC860.Items[C860Count].RegistroC890.New;
end;

procedure TBloco_C.WriteRegistroC001 ;
begin
  if Assigned(FRegistroC001) then
  begin
     if (RegistroC990.QTD_LIN_C = 0) then   // Já gravou o C001 ?
     begin
        with FRegistroC001 do
        begin
           Add( LFill( 'C001' ) +
                LFill( Integer(IND_MOV), 0 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     if FRegistroC001.IND_MOV = imComDados then
     begin
       WriteRegistroC010(FRegistroC001) ;
     end;
  end;
end;

procedure TBloco_C.WriteRegistroC010(RegC001: TRegistroC001) ;
  var
    strIND_ESCRI: string;
    intFor: Integer;
begin
  if Assigned(RegC001.RegistroC010) then
  begin
    if (FRegistroC010Count<RegC001.RegistroC010.Count) then // Algum C010 ainda nao gravado?
    begin
      for intFor := FRegistroC010Count to RegC001.RegistroC010.Count - 1 do
      begin
        with RegC001.RegistroC010.Items[intFor] do
        begin
          Check(funChecaCNPJ(CNPJ), '(C-010) ESTABELECIMENTO: O CNPJ "%s" digitado é inválido!', [CNPJ]);

          case IND_ESCRI of
             IndEscriConsolidado       : strIND_ESCRI := '1';
             IndEscriIndividualizado   : strIND_ESCRI := '2';
          end;

          Add( LFill('C010')   +
               LFill(CNPJ, 14) +
               LFill(strIND_ESCRI) ) ;
        end;
        // Registros FILHOS
        WriteRegistroC100( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC180( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC190( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC380( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC395( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC400( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC490( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC500( RegC001.RegistroC010.Items[intFor] );
        WriteRegistroC600( RegC001.RegistroC010.Items[intFor] );
        if DT_INI >= StrToDate('01/05/2015') then
          WriteRegistroC860( RegC001.RegistroC010.Items[intFor] );
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
      // Variavél para armazenar a quantidade de registro do tipo.
      FRegistroC010Count := FRegistroC010Count + RegC001.RegistroC010.Count;
    end
    else // apenas gravar os registros FILHOS do ultimo C010 existente
    begin
      // Registros FILHOS
      WriteRegistroC100( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC180( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC190( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC380( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC395( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC400( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC490( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC500( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      WriteRegistroC600( RegC001.RegistroC010.Items[FRegistroC010Count-1] );
      if DT_INI >= StrToDate('01/05/2015') then
        WriteRegistroC860( RegC001.RegistroC010.Items[FRegistroC010Count-1] );

    end;
  end;
end;

procedure TBloco_C.WriteRegistroC100(RegC010: TRegistroC010);
  var
    intFor         : integer;
    strIND_EMIT    : String;
    strIND_PGTO    : String;
    booNFCancelada : Boolean;
begin
  if Assigned(RegC010.RegistroC100) then
  begin
     for intFor := 0 to RegC010.RegistroC100.Count - 1 do
     begin
        with RegC010.RegistroC100.Items[intFor] do
        begin
          case IND_EMIT of
            edEmissaoPropria  : strIND_EMIT := '0';
            edTerceiros       : strIND_EMIT := '1';
          end;
          // Para cada registro C100, obrigatoriamente deve ser  apresentado, pelo menos, um registro C170,
          // exceto em relação aos documentos fiscais referentes à nota fiscal cancelada (código “02” ou “03”),
          // Nota Fiscal Eletrônica (NF-e) denegada (código “04”) ou numeração inutilizada (código “05”),
          // os quais não devem ser escriturados os registrosfilhos de C100.
          //
          // preencher somente os campos :
          // indicador do emitente,
          // código da situação,
          // indicador de operação,
          // código do modelo,
          // série do documento ,
          // número do documento
          // chave do documento eletrônico.
          if COD_SIT in [sdfCancelado{02}, sdfExtCancelado{03}, sdfDenegado{04}, sdfInutilizado{05}] then
          begin
            booNFCancelada := true;

            DT_DOC         := 0;
            DT_E_S         := 0;
            IND_FRT        := tfNenhum;
            IND_PGTO       := tpNenhum;
            COD_PART       := '';
            DT_DOC         := 0;
            DT_E_S         := 0;
            VL_DOC         := 0;
            VL_DESC        := 0;
            VL_ABAT_NT     := 0;
            VL_MERC        := 0;
            VL_FRT         := 0;
            VL_SEG         := 0;
            VL_OUT_DA      := 0;
            VL_BC_ICMS     := 0;
            VL_ICMS        := 0;
            VL_BC_ICMS_ST  := 0;
            VL_ICMS_ST     := 0;
            VL_IPI         := 0;
            VL_PIS         := 0;
            VL_COFINS      := 0;
            VL_PIS_ST      := 0;
            VL_COFINS_ST   := 0;
            if COD_SIT in [sdfInutilizado{05}] then
               CHV_NFE := '';
          end
          else
            booNFCancelada := false;

          case IND_PGTO of
            tpVista : strIND_PGTO := '0';
            tpPrazo : strIND_PGTO := '1';
            tpSemPagamento :
            begin
               if DT_INI >= EncodeDate(2012,07,01) then
                  strIND_PGTO := '2'
               else
                  strIND_PGTO := '9';
            end;
            tpNenhum : strIND_PGTO := '';
          end;

          Add( LFill('C100')                               +
               LFill( IndTipoOperToStr(IND_OPER) )         +
               LFill( strIND_EMIT )                        +
               LFill( COD_PART )                           +
               LFill( COD_MOD )                            +
               LFill( CodSitToStr(COD_SIT) )               +
               LFill( SER )                                +
               LFill( NUM_DOC )                            +
               LFill( CHV_NFE )                            +
               LFill( DT_DOC, 'ddmmyyyy' )                 +
               LFill( DT_E_S, 'ddmmyyyy' )                 +
               LFill( VL_DOC,0,2, booNFCancelada  )        +
               LFill( strIND_PGTO )                        +
               LFill( VL_DESC,0,2, booNFCancelada  )       +
               LFill( VL_ABAT_NT,0,2, booNFCancelada  )    +
               LFill( VL_MERC,0,2, booNFCancelada )        +
               LFill( IndFrtToStr(IND_FRT) )               +
               LFill( VL_FRT,0,2, booNFCancelada  )        +
               LFill( VL_SEG,0,2, booNFCancelada  )        +
               LFill( VL_OUT_DA,0,2, booNFCancelada  )     +
               LFill( VL_BC_ICMS,0,2, booNFCancelada  )    +
               LFill( VL_ICMS,0,2, booNFCancelada  )       +
               LFill( VL_BC_ICMS_ST,0,2, booNFCancelada  ) +
               LFill( VL_ICMS_ST,0,2, booNFCancelada  )    +
               LFill( VL_IPI,0,2, booNFCancelada  )        +
               LFill( VL_PIS,0,2, booNFCancelada  )        +
               LFill( VL_COFINS,0,2, booNFCancelada  )     +
               LFill( VL_PIS_ST,0,2, booNFCancelada  )     +
               LFill( VL_COFINS_ST,0,2, booNFCancelada  ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroC110( RegC010.RegistroC100.Items[intFor] );
        WriteRegistroC111( RegC010.RegistroC100.Items[intFor] );
        WriteRegistroC120( RegC010.RegistroC100.Items[intFor] );
        WriteRegistroC170( RegC010.RegistroC100.Items[intFor] );
        WriteRegistroC175( RegC010.RegistroC100.Items[intFor] );
        ///
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC100Count := FRegistroC100Count + RegC010.RegistroC100.Count;
     //
     RegC010.RegistroC100.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC110(RegC100: TRegistroC100);
  var
    intFor: integer;
begin
  if Assigned(RegC100.RegistroC110) then
  begin
    for intFor := 0 to RegC100.RegistroC110.Count - 1 do
    begin
      with RegC100.RegistroC110.Items[intFor] do
      begin
        Add( LFill('C110')  +
             LFill(COD_INF) +
             LFill(TXT_COMPL) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC110Count := FRegistroC110Count + RegC100.RegistroC110.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC111(RegC100: TRegistroC100);
var
  intFor      : integer;
begin
  if Assigned(RegC100.RegistroC111) then
  begin
    for intFor := 0 to RegC100.RegistroC111.Count - 1 do
    begin
      with RegC100.RegistroC111.Items[intFor] do
      begin

        Add( LFill('C111')   +
             LFill(NUM_PROC) +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC111Count := FRegistroC111Count + RegC100.RegistroC111.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC120(RegC100: TRegistroC100);
  var
    intFor: integer;
    strCOD_DOC_IMP: string;
begin
  if Assigned(RegC100.RegistroC120) then
  begin
    for intFor := 0 to RegC100.RegistroC120.Count - 1 do
    begin
      with RegC100.RegistroC120.Items[intFor] do
      begin
        case COD_DOC_IMP of
          diImportacao         :    strCOD_DOC_IMP := '0';
          diSimplificadaImport :    strCOD_DOC_IMP := '1';
        end;

        Add( LFill('C120')          +
             LFill(strCOD_DOC_IMP)  +
             LFill(NUM_DOC__IMP)    +
             LFill(PIS_IMP,0,2)     +
             LFill(COFINS_IMP,0,2)  +
             LFill(NUM_ACDRAW, 20)) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC120Count := FRegistroC120Count + RegC100.RegistroC120.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC170(RegC100: TRegistroC100);
var
  intFor        : integer;
  strIND_MOV    : string;
  strIND_APUR   : string;
begin
  if Assigned( RegC100.RegistroC170 ) then
  begin
     for intFor := 0 to RegC100.RegistroC170.Count - 1 do
     begin
        with RegC100.RegistroC170.Items[intFor] do
        begin
          case IND_MOV of
            mfSim       : strIND_MOV := '0';
            mfNao       : strIND_MOV := '1';
          end;

          case IND_APUR of
            iaVazio     : strIND_APUR := '';
            iaMensal    : strIND_APUR := '0';
            iaDecendial : strIND_APUR := '1';
          end;

          Add(
              {01} LFill('C170')                 +
              {02} LFill( NUM_ITEM )             +
              {03} LFill( COD_ITEM )             +
              {04} LFill( DESCR_COMPL )          +
              {05} VDFill( QTD, 5 )              +
              {06} LFill( UNID )                 +
              {07} LFill( VL_ITEM,0,2 )          +
              {08} LFill( VL_DESC,0,2 )          +
              {09} LFill( strIND_MOV )           +
              {10} LFill( CstIcmsToStr(CST_ICMS) ) +
              {11} LFill( CFOP,4 )               +
              {12} LFill( COD_NAT )              +
              {13} LFill( VL_BC_ICMS,0,2 )       +
              {14} LFill( ALIQ_ICMS,0,2 )        +
              {15} LFill( VL_ICMS,0,2 )          +
              {16} LFill( VL_BC_ICMS_ST,0,2 )    +
              {17} LFill( ALIQ_ST,0,2 )          +
              {18} LFill( VL_ICMS_ST,0,2 )       +
              {19} LFill( strIND_APUR )          +
              {20} LFill( CstIpiToStr(CST_IPI) ) +
              {21} LFill( COD_ENQ )              +
              {22} LFill( VL_BC_IPI,0,2,True )   +
              {23} LFill( ALIQ_IPI,0,2,True )    +
              {24} LFill( VL_IPI,0,2,True )      +
              {25} LFill( CstPisToStr(CST_PIS) ) +
              {26} VDFill( VL_BC_PIS,     2) +
              {27} VDFill( ALIQ_PIS_PERC, 4) +
              {28} VDFill( QUANT_BC_PIS,  3) +
              {29} VDFill( ALIQ_PIS_R,    4) +
              {30} LFill( VL_PIS, 0, 2 )     +
              {31} LFill( CstCofinsToStr(CST_COFINS) ) +
              {32} VDFill( VL_BC_COFINS,     2) +
              {33} VDFill( ALIQ_COFINS_PERC, 4) +
              {34} VDFill( QUANT_BC_COFINS,  3) +
              {35} VDFill( ALIQ_COFINS_R,    4) +
              {36} LFill( VL_COFINS, 0, 2)      +
              {37} LFill( COD_CTA ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC170Count := FRegistroC170Count + RegC100.RegistroC170.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC175(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  //--
  if Assigned( RegC100.RegistroC175 ) then
  begin
     for intFor := 0 to RegC100.RegistroC175.Count - 1 do
     begin
        with RegC100.RegistroC175.Items[intFor] do
        begin
          Add(
              {01}  LFill('C175')                   +
              {02}  LFill( CFOP, 4 )                +
              {03} VLFill( VL_OPR, 0, 2 )           +
              {04} VLFill( VL_DESC, 0, 2 )          +
              {05}  LFill( CstPisToStr(CST_PIS) )   +
              {06} VLFill( VL_BC_PIS, 0, 2)        +
              {07} VLFill( ALIQ_PIS, 8, 4)         +
              {08} VLFill( QUANT_BC_PIS, 0, 3)     +
              {09} VLFill( ALIQ_PIS_QUANT, 0, 4)   +
              {10} VLFill( VL_PIS, 0, 2)           +
              {11}  LFill( CstCofinsToStr(CST_COFINS) ) +
              {12} VLFill( VL_BC_COFINS, 0, 2)     +
              {13} VLFill( ALIQ_COFINS, 8, 4)      +
              {14} VLFill( QUANT_BC_COFINS, 0, 3)  +
              {15} VLFill( ALIQ_COFINS_QUANT, 0, 4)+
              {16} VLFill( VL_COFINS, 0, 2)        +
              {17}  LFill( COD_CTA )                +
              {18}  LFill( INFO_COMPL)) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     //-- Variável para armazenar a quantidade de registro do tipo.
     FRegistroC175Count := FRegistroC175Count + RegC100.RegistroC175.Count;
  end;

end;

procedure TBloco_C.WriteRegistroC180(RegC010: TRegistroC010);
  var
    intFor: integer;
begin
  if Assigned(RegC010.RegistroC180) then
  begin
    for intFor := 0 to RegC010.RegistroC180.Count - 1 do
    begin
      with RegC010.RegistroC180.Items[intFor] do
      begin
        Add( LFill('C180')                 +
             LFill(COD_MOD,2)              +
             LFill(DT_DOC_INI, 'ddmmyyyy') +
             LFill(DT_DOC_FIN, 'ddmmyyyy') +
             LFill(COD_ITEM)               +
             LFill(COD_NCM)                +
             LFill(EX_IPI)                 +
             LFill(VL_TOT_ITEM,0,2)) ;
      end;
      // Registros FILHOS
      WriteRegistroC181( RegC010.RegistroC180.Items[intFor] );
      WriteRegistroC185( RegC010.RegistroC180.Items[intFor] );
      WriteRegistroC188( RegC010.RegistroC180.Items[intFor] );
      //
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC180Count := FRegistroC180Count + RegC010.RegistroC180.Count;
    //
    RegC010.RegistroC180.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC181(RegC180: TRegistroC180);
var
  intFor     : integer;
begin
  if Assigned(RegC180.RegistroC181) then
  begin
    for intFor := 0 to RegC180.RegistroC181.Count - 1 do
    begin
      with RegC180.RegistroC181.Items[intFor] do
      begin
        Add( LFill('C181')             +
             LFill( CstPisToStr(CST_PIS)) +
             LFill( CFOP, 4)               +
             VLFill(VL_ITEM,0,2)        +
             VLFill(VL_DESC,0,2)        +
             VLFill(VL_BC_PIS,0,2) +
             VDFill(ALIQ_PIS,4)    +
             VLFill(QUANT_BC_PIS,0,3) +
             VDFill(ALIQ_PIS_QUANT,4) +
             VLFill(VL_PIS,0,2)         +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC181Count := FRegistroC181Count + RegC180.RegistroC181.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC185(RegC180: TRegistroC180);
  var
    intFor: integer;
begin
  if Assigned(RegC180.RegistroC185) then
  begin
    for intFor := 0 to RegC180.RegistroC185.Count - 1 do
    begin
      with RegC180.RegistroC185.Items[intFor] do
      begin

        Add( LFill('C185')                     +
             LFill(CstCofinsToStr(CST_COFINS)) +
             LFill(CFOP, 4)                       +
             VLFill(VL_ITEM,0,2)         +
             VLFill(VL_DESC,0,2)         +
             VLFill(VL_BC_COFINS,0,2)    +
             VDFill(ALIQ_COFINS,4)       +
             VLFill(QUANT_BC_COFINS,0,3) +
             VDFill(ALIQ_COFINS_QUANT,4) +
             VLFill(VL_COFINS,0,2)       +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC185Count := FRegistroC185Count + RegC180.RegistroC185.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC188(RegC180: TRegistroC180);
var
  intFor      : integer;
begin
  if Assigned(RegC180.RegistroC188) then
  begin
    for intFor := 0 to RegC180.RegistroC188.Count - 1 do
    begin
      with RegC180.RegistroC188.Items[intFor] do
      begin
        Add( LFill('C188')   +
             LFill(NUM_PROC) +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC188Count := FRegistroC188Count + RegC180.RegistroC188.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC190(RegC010: TRegistroC010);
  var
    intFor: integer;
begin
  if Assigned(RegC010.RegistroC190) then
  begin
    for intFor := 0 to RegC010.RegistroC190.Count - 1 do
    begin
      with RegC010.RegistroC190.Items[intFor] do
      begin
        Add( LFill('C190')                 +
             LFill(COD_MOD,2)              +
             LFill(DT_REF_INI, 'ddmmyyyy') +
             LFill(DT_REF_FIN, 'ddmmyyyy') +
             LFill(COD_ITEM)               +
             LFill(COD_NCM)                +
             LFill(EX_IPI)                 +
             LFill(VL_TOT_ITEM,0,2)) ;
      end;
      // Registros FILHOS
      WriteRegistroC191( RegC010.RegistroC190.Items[intFor] );
      WriteRegistroC195( RegC010.RegistroC190.Items[intFor] );
      WriteRegistroC198( RegC010.RegistroC190.Items[intFor] );
      WriteRegistroC199( RegC010.RegistroC190.Items[intFor] );
      //
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC190Count := FRegistroC190Count + RegC010.RegistroC190.Count;
    //
    RegC010.RegistroC190.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC191(RegC190: TRegistroC190);
  var
    intFor: integer;
begin
  if Assigned(RegC190.RegistroC191) then
  begin
    for intFor := 0 to RegC190.RegistroC191.Count - 1 do
    begin
      with RegC190.RegistroC191.Items[intFor] do
      begin
        Add( LFill('C191')             +
             LFill(CNPJ_CPF_PART)      +
             LFill(CstPisToStr(CST_PIS)) +
             LFill(CFOP,4)             +
             LFill(VL_ITEM,  0, 2)     +
             VLFill(VL_DESC, 0, 2)     +
             VLFill(VL_BC_PIS,      2) +
             VDFill(ALIQ_PIS,       4) +
             VLFill(QUANT_BC_PIS,   3) +
             VDFill(ALIQ_PIS_QUANT, 4) +
             VLFill(VL_PIS, 0, 2)      +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC191Count := FRegistroC191Count + RegC190.RegistroC191.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC195(RegC190: TRegistroC190);
  var
    intFor: integer;
begin
  if Assigned(RegC190.RegistroC195) then
  begin
    for intFor := 0 to RegC190.RegistroC195.Count - 1 do
    begin
      with RegC190.RegistroC195.Items[intFor] do
      begin
        Add( LFill('C195')               +
             LFill(CNPJ_CPF_PART)        +
             LFill(CstCofinsToStr(CST_COFINS)) +
             LFill(CFOP, 4)               +
             LFill(VL_ITEM, 0, 2)         +
             VLFill(VL_DESC, 0, 2)        +
             VLFill(VL_BC_COFINS, 2)      +
             VDFill(ALIQ_COFINS, 4)       +
             VLFill(QUANT_BC_COFINS, 3)   +
             VDFill(ALIQ_COFINS_QUANT, 4) +
             VLFill(VL_COFINS, 0, 2)      +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC195Count := FRegistroC195Count + RegC190.RegistroC195.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC198(RegC190: TRegistroC190);
var
  intFor      : integer;
begin
  if Assigned(RegC190.RegistroC198) then
  begin
    for intFor := 0 to RegC190.RegistroC198.Count - 1 do
    begin
      with RegC190.RegistroC198.Items[intFor] do
      begin
        Add( LFill('C198')   +
             LFill(NUM_PROC) +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC198Count := FRegistroC198Count + RegC190.RegistroC198.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC199(RegC190: TRegistroC190);
  var
    intFor         : integer;
    strCOD_DOC_IMP : string;
begin
  if Assigned(RegC190.RegistroC199) then
  begin
    for intFor := 0 to RegC190.RegistroC199.Count - 1 do
    begin
      with RegC190.RegistroC199.Items[intFor] do
      begin
        case COD_DOC_IMP of
          diImportacao         :    strCOD_DOC_IMP := '0';
          diSimplificadaImport :    strCOD_DOC_IMP := '1';
        end;

        Add( LFill('C199')            +
             LFill(strCOD_DOC_IMP)    +
             LFill(NUM_DOC__IMP)      +
             LFill(VL_PIS_IMP,0,2)    +
             LFill(VL_COFINS_IMP,0,2) +
             LFill(NUM_ACDRAW) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC199Count := FRegistroC199Count + RegC190.RegistroC199.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC380(RegC010: TRegistroC010);
  var
    intFor: integer;
begin
  if Assigned(RegC010.RegistroC380) then
  begin
    for intFor := 0 to RegC010.RegistroC380.Count - 1 do
    begin
      with RegC010.RegistroC380.Items[intFor] do
      begin
        Add( LFill('C380')                 +
             LFill(COD_MOD,2)              +
             LFill(DT_DOC_INI, 'ddmmyyyy') +
             LFill(DT_DOC_FIN, 'ddmmyyyy') +
             LFill(NUM_DOC_INI, 6, True)   +
             LFill(NUM_DOC_FIN, 6, True)   +
             LFill(VL_DOC,0,2)             +
             LFill(VL_DOC_CANC,0,2)) ;
      end;
      // Registros FILHOS
      WriteRegistroC381( RegC010.RegistroC380.Items[intFor] );
      WriteRegistroC385( RegC010.RegistroC380.Items[intFor] );
      //
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC380Count := FRegistroC380Count + RegC010.RegistroC380.Count;
    //
    RegC010.RegistroC380.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC381(RegC380: TRegistroC380);
  var
    intFor     : integer;
begin
  if Assigned(RegC380.RegistroC381) then
  begin
    for intFor := 0 to RegC380.RegistroC381.Count - 1 do
    begin
      with RegC380.RegistroC381.Items[intFor] do
      begin
        Add( LFill('C381')               +
             LFill(CstPisToStr(CST_PIS)) +
             LFill(COD_ITEM)             +
             LFill(VL_ITEM,0,2)          +
             VLFill(VL_BC_PIS, 0,2)      +
             VDFill(ALIQ_PIS, 4)         +
             VLFill(QUANT_BC_PIS, 3)     +
             VDFill(ALIQ_PIS_QUANT, 4)   +
             VLFill(VL_PIS, 0, 2)        +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC381Count := FRegistroC381Count + RegC380.RegistroC381.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC385(RegC380: TRegistroC380);
  var
    intFor        : integer;
begin
  if Assigned(RegC380.RegistroC385) then
  begin
    for intFor := 0 to RegC380.RegistroC385.Count - 1 do
    begin
      with RegC380.RegistroC385.Items[intFor] do
      begin
        Add( LFill('C385')                     +
             LFill(CstCofinsToStr(CST_COFINS)) +
             LFill(COD_ITEM)              +
             LFill(VL_ITEM, 0, 2)         +
             VLFill(VL_BC_COFINS, 0, 2)   +
             VDFill(ALIQ_COFINS, 4)       +
             VLFill(QUANT_BC_COFINS, 3)   +
             VDFill(ALIQ_COFINS_QUANT, 4) +
             VLFill(VL_COFINS, 0, 2)      +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC385Count := FRegistroC385Count + RegC380.RegistroC385.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC395(RegC010: TRegistroC010);
  var
    intFor: integer;
begin
  if Assigned(RegC010.RegistroC395) then
  begin
    for intFor := 0 to RegC010.RegistroC395.Count - 1 do
    begin
      with RegC010.RegistroC395.Items[intFor] do
      begin
        Add( LFill('C395')             +
             LFill(COD_MOD)            +
             LFill(COD_PART)           +
             LFill(SER)                +
             LFill(SUB_SER)            +
             LFill(NUM_DOC)            +
             LFill(DT_DOC, 'ddmmyyyy') +
             LFill(VL_DOC,0,2) );
      end;
      // Registros FILHOS
      WriteRegistroC396( RegC010.RegistroC395.Items[intFor] );
      //
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC395Count := FRegistroC395Count + RegC010.RegistroC395.Count;
    //
    RegC010.RegistroC395.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC396(RegC395: TRegistroC395);
  var
    intFor         : integer;
    strNAT_BC_CRED : String;

begin
  if Assigned(RegC395.RegistroC396) then
  begin
    for intFor := 0 to RegC395.RegistroC396.Count - 1 do
    begin
      with RegC395.RegistroC396.Items[intFor] do
      begin
        case NAT_BC_CRED of
          bccAqBensRevenda                 : strNAT_BC_CRED := '01';
          bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
          bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
          bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
          bccAluguelPredios                : strNAT_BC_CRED := '05';
          bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
          bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
          bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
          bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
          bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
          bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
          bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
          bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
          bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
          bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
          bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
          bccAtPresServ                    : strNAT_BC_CRED := '17';
          bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
        end;

        Add( LFill('C396')           +
             LFill(COD_ITEM)         +
             LFill(VL_ITEM,0,2)      +
             LFill(VL_DESC,0,2)      +
             LFill(strNAT_BC_CRED)   +
             LFill(CstPisToStr(CST_PIS)) +
             LFill(VL_BC_PIS,0,2)    +
             DFill(ALIQ_PIS, 4)      +
             LFill(VL_PIS,0,2)       +
             LFill(CstCofinsToStr(CST_COFINS)) +
             LFill(VL_BC_COFINS,0,2) +
             DFill(ALIQ_COFINS, 4)   +
             LFill(VL_COFINS,0,2)    +
             LFill(COD_CTA) );
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC396Count := FRegistroC396Count + RegC395.RegistroC396.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC400(RegC010: TRegistroC010);
  var
    intFor: integer;
begin
  if Assigned( RegC010.RegistroC400 ) then
  begin
     for intFor := 0 to RegC010.RegistroC400.Count - 1 do
     begin
        with RegC010.RegistroC400.Items[intFor] do
        begin
          Add( LFill('C400')   +
               LFill(COD_MOD ) +
               LFill(ECF_MOD)  +
               LFill(ECF_FAB)  +
               LFill(ECF_CX, 3) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC405( RegC010.RegistroC400.Items[intFor] );
        WriteRegistroC489( RegC010.RegistroC400.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC400Count := FRegistroC400Count + RegC010.RegistroC400.Count;

     RegC010.RegistroC400.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC405(RegC400: TRegistroC400);
  var
    intFor: integer;
begin
  if Assigned( RegC400.RegistroC405 ) then
  begin
     for intFor := 0 to RegC400.RegistroC405.Count - 1 do
     begin
        with RegC400.RegistroC405.Items[intFor] do
        begin
          Add( LFill('C405')           +
               LFill( DT_DOC )         +
               LFill( CRO,3 )          +
               LFill( CRZ,6 )          +
               LFill( NUM_COO_FIN,6  ) +
               LFill( GT_FIN,0,2  )    +
               LFill( VL_BRT,0,2  ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC481( RegC400.RegistroC405.Items[intFor] ) ;
        WriteRegistroC485( RegC400.RegistroC405.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC405Count := FRegistroC405Count + RegC400.RegistroC405.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC481(RegC405: TRegistroC405);
var
  intFor: integer;
  NullVL_BC_PIS: Boolean;
  NullALIQ_PIS: Boolean;
  NullQUANT_BC_PIS: Boolean;
  NullALIQ_PIS_QUANT: Boolean;
  NullVL_PIS: Boolean;
begin
   NullVL_BC_PIS := False;
   NullALIQ_PIS  := False;
   NullQUANT_BC_PIS := False;
   NullALIQ_PIS_QUANT := False;
   NullVL_PIS := False;
   //--
   if Assigned(RegC405.RegistroC481) then
   begin
      for intFor := 0 to RegC405.RegistroC481.Count - 1 do
      begin
        if Assigned(FOnBeforeWriteRegistroC481) then
        begin
           FOnBeforeWriteRegistroC481(RegC405.RegistroC481.Items[intFor],
                                      NullVL_BC_PIS,
                                      NullALIQ_PIS,
                                      NullQUANT_BC_PIS,
                                      NullALIQ_PIS_QUANT,
                                      NullVL_PIS);
          with RegC405.RegistroC481.Items[intFor] do
          begin
            Add( LFill('C481')             +
                 LFill(CstPisToStr(CST_PIS)) +
                 LFill(VL_ITEM,0,2)        +
                 DFill(VL_BC_PIS,      2, NullVL_BC_PIS) +
                 DFill(ALIQ_PIS,       4, NullALIQ_PIS) +
                 DFill(QUANT_BC_PIS,   3, NullQUANT_BC_PIS) +
                 DFill(ALIQ_PIS_QUANT, 4, NullALIQ_PIS_QUANT) +
                 LFill(VL_PIS,0,       2, NullVL_PIS)         +
                 LFill(COD_ITEM)           +
                 LFill(COD_CTA) ) ;
            //
            RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
          end;

        end
        else
        begin
          with RegC405.RegistroC481.Items[intFor] do
          begin
            Add( LFill('C481')             +
                 LFill(CstPisToStr(CST_PIS)) +
                 LFill(VL_ITEM, 0, 2)      +
                 VDFill(VL_BC_PIS,      2) +
                 VDFill(ALIQ_PIS,       4) +
                 VDFill(QUANT_BC_PIS,   3) +
                 VDFill(ALIQ_PIS_QUANT, 4) +
                 VDFill(VL_PIS,         2) +
                 LFill(COD_ITEM)           +
                 LFill(COD_CTA) ) ;
            //
            RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
          end;
        end;
     end;
     // Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC481Count := FRegistroC481Count + RegC405.RegistroC481.Count;
   end;
end;

procedure TBloco_C.WriteRegistroC485(RegC405: TRegistroC405);
var
  intFor: integer;
  NullVL_BC_COFINS: Boolean;
  NullALIQ_COFINS: Boolean;
  NullQUANT_BC_COFINS: Boolean;
  NullALIQ_COFINS_QUANT: Boolean;
  NullVL_COFINS: Boolean;
begin
   NullVL_BC_COFINS := False;
   NullALIQ_COFINS  := False;
   NullQUANT_BC_COFINS := False;
   NullALIQ_COFINS_QUANT := False;
   NullVL_COFINS := False;
   //--
   if Assigned(RegC405.RegistroC485) then
   begin
      for intFor := 0 to RegC405.RegistroC485.Count - 1 do
      begin
        if Assigned(FOnBeforeWriteRegistroC485) then
        begin
           FOnBeforeWriteRegistroC485(RegC405.RegistroC485.Items[intFor],
                                      NullVL_BC_COFINS,
                                      NullALIQ_COFINS,
                                      NullQUANT_BC_COFINS,
                                      NullALIQ_COFINS_QUANT,
                                      NullVL_COFINS);

          with RegC405.RegistroC485.Items[intFor] do
          begin
            Add( LFill('C485')                     +
                 LFill(CstCofinsToStr(CST_COFINS)) +
                 LFill(VL_ITEM,0,2)                +
                 DFill(VL_BC_COFINS,      2, NullVL_BC_COFINS) +
                 DFill(ALIQ_COFINS,       4, NullALIQ_COFINS) +
                 DFill(QUANT_BC_COFINS,   3, NullQUANT_BC_COFINS) +
                 DFill(ALIQ_COFINS_QUANT, 4, NullALIQ_COFINS_QUANT) +
                 LFill(VL_COFINS,0,       2, NullVL_COFINS) +
                 LFill(COD_ITEM)                   +
                 LFill(COD_CTA) ) ;
            //
            RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
          end;

        end
        else
        begin
          with RegC405.RegistroC485.Items[intFor] do
          begin
            Add( LFill('C485')                     +
                 LFill(CstCofinsToStr(CST_COFINS)) +
                 LFill(VL_ITEM, 0, 2)              +
                 VDFill(VL_BC_COFINS,      2) +
                 VDFill(ALIQ_COFINS,       4) +
                 VDFill(QUANT_BC_COFINS,   3) +
                 VDFill(ALIQ_COFINS_QUANT, 4) +
                 VDFill(VL_COFINS,       2)   +
                 LFill(COD_ITEM)              +
                 LFill(COD_CTA) ) ;
            //
            RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
          end;
        end;
     end;
     // Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC485Count := FRegistroC485Count + RegC405.RegistroC485.Count;
   end;
end;

procedure TBloco_C.WriteRegistroC489(RegC400: TRegistroC400);
var
  intFor      : integer;
begin
  if Assigned(RegC400.RegistroC489) then
  begin
    for intFor := 0 to RegC400.RegistroC489.Count - 1 do
    begin
      with RegC400.RegistroC489.Items[intFor] do
      begin
        Add( LFill('C489')   +
             LFill(NUM_PROC) +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC489Count := FRegistroC489Count + RegC400.RegistroC489.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC490(RegC010: TRegistroC010);
  var
    intFor: integer;
begin
  if Assigned(RegC010.RegistroC490) then
  begin
    for intFor := 0 to RegC010.RegistroC490.Count - 1 do
    begin
      with RegC010.RegistroC490.Items[intFor] do
      begin
        Add( LFill('C490') +
             LFill(DT_DOC_INI,'ddmmyyyy') +
             LFill(DT_DOC_FIN,'ddmmyyyy') +
             LFill(COD_MOD) ) ;

        /// Registros FILHOS
        WriteRegistroC491( RegC010.RegistroC490.Items[intFor] ) ;
        WriteRegistroC495( RegC010.RegistroC490.Items[intFor] ) ;
        WriteRegistroC499( RegC010.RegistroC490.Items[intFor] ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC490Count := FRegistroC490Count + RegC010.RegistroC490.Count;
    //
    RegC010.RegistroC490.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC491(RegC490: TRegistroC490);
var
  intFor: integer;
  NullVL_BC_PIS: Boolean;
  NullALIQ_PIS: Boolean;
  NullQUANT_BC_PIS: Boolean;
  NullALIQ_PIS_QUANT: Boolean;
  NullVL_PIS: Boolean;
begin
  NullVL_BC_PIS := False;
  NullALIQ_PIS  := False;
  NullQUANT_BC_PIS := False;
  NullALIQ_PIS_QUANT := False;
  NullVL_PIS := False;

  if Assigned(RegC490.RegistroC491) then
  begin
    for intFor := 0 to RegC490.RegistroC491.Count - 1 do
    begin
      if Assigned(FOnBeforeWriteRegistroC491) then
      begin
         FOnBeforeWriteRegistroC491(RegC490.RegistroC491.Items[intFor],
                                    NullVL_BC_PIS,
                                    NullALIQ_PIS,
                                    NullQUANT_BC_PIS,
                                    NullALIQ_PIS_QUANT,
                                    NullVL_PIS);

        with RegC490.RegistroC491.Items[intFor] do
        begin
          Add( LFill('C491')             +
               LFill(COD_ITEM)           +
               LFill(CstPisToStr(CST_PIS)) +
               LFill(CFOP,4)             +
               LFill(VL_ITEM,0,2)        +
               DFill(VL_BC_PIS,      2, NullVL_BC_PIS) +
               DFill(ALIQ_PIS,       4, NullALIQ_PIS) +
               DFill(QUANT_BC_PIS,   3, NullQUANT_BC_PIS) +
               DFill(ALIQ_PIS_QUANT, 4, NullALIQ_PIS_QUANT) +
               LFill(VL_PIS,0,       2, NullVL_PIS)         +
               LFill(COD_CTA) ) ;
          //
          RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
        end;

      end
      else
      begin
        with RegC490.RegistroC491.Items[intFor] do
        begin
          Add( LFill('C491')             +
               LFill(COD_ITEM)           +
               LFill(CstPisToStr(CST_PIS)) +
               LFill(CFOP,4)             +
               LFill(VL_ITEM,0,2)        +
               VDFill(VL_BC_PIS,      2) +
               VDFill(ALIQ_PIS,       4) +
               VDFill(QUANT_BC_PIS,   3) +
               VDFill(ALIQ_PIS_QUANT, 4) +
               VDFill(VL_PIS,         2) +
               LFill(COD_CTA) ) ;
          //
          RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
        end;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC491Count := FRegistroC491Count + RegC490.RegistroC491.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC495(RegC490: TRegistroC490);
var
  intFor: integer;
  NullVL_BC_COFINS: Boolean;
  NullALIQ_COFINS: Boolean;
  NullQUANT_BC_COFINS: Boolean;
  NullALIQ_COFINS_QUANT: Boolean;
  NullVL_COFINS: Boolean;
begin
  NullVL_BC_COFINS := False;
  NullALIQ_COFINS  := False;
  NullQUANT_BC_COFINS := False;
  NullALIQ_COFINS_QUANT := False;
  NullVL_COFINS := False;
  //--
  if Assigned(RegC490.RegistroC495) then
  begin
    for intFor := 0 to RegC490.RegistroC495.Count - 1 do
    begin
      if Assigned(FOnBeforeWriteRegistroC495) then
      begin
         FOnBeforeWriteRegistroC495(RegC490.RegistroC495.Items[intFor],
                                    NullVL_BC_COFINS,
                                    NullALIQ_COFINS,
                                    NullQUANT_BC_COFINS,
                                    NullALIQ_COFINS_QUANT,
                                    NullVL_COFINS);
        with RegC490.RegistroC495.Items[intFor] do
        begin
          Add( LFill('C495')                +
               LFill(COD_ITEM)              +
               LFill(CstCofinsToStr(CST_COFINS) ) +
               LFill(CFOP,4)                +
               LFill(VL_ITEM,0,2)           +
               DFill(VL_BC_COFINS,      2, NullVL_BC_COFINS) +
               DFill(ALIQ_COFINS,       4, NullALIQ_COFINS) +
               DFill(QUANT_BC_COFINS,   3, NullQUANT_BC_COFINS) +
               DFill(ALIQ_COFINS_QUANT, 4, NullALIQ_COFINS_QUANT) +
               LFill(VL_COFINS,0,       2, NullVL_COFINS) +
               LFill(COD_CTA) ) ;
          //
          RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
        end;

      end
      else
      begin
        with RegC490.RegistroC495.Items[intFor] do
        begin
          Add( LFill('C495')                +
               LFill(COD_ITEM)              +
               LFill(CstCofinsToStr(CST_COFINS) ) +
               LFill(CFOP,4)                +
               LFill(VL_ITEM,0,2)           +
               VDFill(VL_BC_COFINS,      2) +
               VDFill(ALIQ_COFINS,       4) +
               VDFill(QUANT_BC_COFINS,   3) +
               VDFill(ALIQ_COFINS_QUANT, 4) +
               VDFill(VL_COFINS,         2) +
               LFill(COD_CTA) ) ;
          //
          RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
        end;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC495Count := FRegistroC495Count + RegC490.RegistroC495.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC499(RegC490: TRegistroC490);
var
  intFor      : integer;
begin
  if Assigned(RegC490.RegistroC499) then
  begin
    for intFor := 0 to RegC490.RegistroC499.Count - 1 do
    begin
      with RegC490.RegistroC499.Items[intFor] do
      begin

        Add( LFill('C499')   +
             LFill(NUM_PROC) +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC499Count := FRegistroC499Count + RegC490.RegistroC499.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC500(RegC010: TRegistroC010);
  var
    intFor              : integer;
begin
  if Assigned( RegC010.RegistroC500 ) then
  begin
     for intFor := 0 to RegC010.RegistroC500.Count - 1 do
     begin
        with RegC010.RegistroC500.Items[intFor] do
        begin
          if Bloco_0.Registro0000.COD_VER >= vlVersao320 then
          begin
            Add( LFill('C500')               +
                 LFill( COD_PART )           +
                 LFill( COD_MOD,2 )          +
                 LFill( CodSitToStr(COD_SIT) )  +
                 LFill( SER )                +
                 LFill( SUB, 3 )                +
                 LFill( NUM_DOC,9 )          +
                 LFill( DT_DOC, 'ddmmyyyy' ) +
                 LFill( DT_ENT, 'ddmmyyyy' ) +
                 LFill( VL_DOC,0,2 )         +
                 LFill( VL_ICMS,0,2 )        +
                 LFill( COD_INF )            +
                 LFill( VL_PIS,0,2 )         +
                 LFill( VL_COFINS,0,2 )      +
                 LFill( CHV_DOCe )
               );

          end
          else
          begin
           Add( LFill('C500')               +
                LFill( COD_PART )           +
                LFill( COD_MOD,2 )          +
                LFill( CodSitToStr(COD_SIT) )  +
                LFill( SER )                +
                LFill( SUB, 3 )                +
                LFill( NUM_DOC,9 )          +
                LFill( DT_DOC, 'ddmmyyyy' ) +
                LFill( DT_ENT, 'ddmmyyyy' ) +
                LFill( VL_DOC,0,2 )         +
                LFill( VL_ICMS,0,2 )        +
                LFill( COD_INF )            +
                LFill( VL_PIS,0,2 )         +
                LFill( VL_COFINS,0,2 ) );
          end;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC501( RegC010.RegistroC500.Items[intFor] ) ;
        WriteRegistroC505( RegC010.RegistroC500.Items[intFor] ) ;
        WriteRegistroC509( RegC010.RegistroC500.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC500Count := FRegistroC500Count + RegC010.RegistroC500.Count;

     RegC010.RegistroC500.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC501(RegC500: TRegistroC500);
  var
    intFor         : integer;
    strNAT_BC_CRED : String;

begin
  if Assigned(RegC500.RegistroC501) then
  begin
    for intFor := 0 to RegC500.RegistroC501.Count - 1 do
    begin
      with RegC500.RegistroC501.Items[intFor] do
      begin
        case NAT_BC_CRED of
          bccAqBensRevenda                 : strNAT_BC_CRED := '01';
          bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
          bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
          bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
          bccAluguelPredios                : strNAT_BC_CRED := '05';
          bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
          bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
          bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
          bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
          bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
          bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
          bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
          bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
          bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
          bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
          bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
          bccAtPresServ                    : strNAT_BC_CRED := '17';
          bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
        end;

        Add( LFill('C501')        +
             LFill(CstPisToStr(CST_PIS) ) +
             LFill(VL_ITEM,0,2)   +
             LFill(strNAT_BC_CRED)+
             LFill(VL_BC_PIS,0,2) +
             DFill(ALIQ_PIS, 4)   +
             LFill(VL_PIS,0,2)    +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC501Count := FRegistroC501Count + RegC500.RegistroC501.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC505(RegC500: TRegistroC500);
  var
    intFor         : integer;
    strNAT_BC_CRED : String;
begin
  if Assigned(RegC500.RegistroC505) then
  begin
    for intFor := 0 to RegC500.RegistroC505.Count - 1 do
    begin
      with RegC500.RegistroC505.Items[intFor] do
      begin
        case NAT_BC_CRED of
          bccAqBensRevenda                 : strNAT_BC_CRED := '01';
          bccAqBensUtiComoInsumo           : strNAT_BC_CRED := '02';
          bccAqServUtiComoInsumo           : strNAT_BC_CRED := '03';
          bccEnergiaEletricaTermica        : strNAT_BC_CRED := '04';
          bccAluguelPredios                : strNAT_BC_CRED := '05';
          bccAluguelMaqEquipamentos        : strNAT_BC_CRED := '06';
          bccArmazenagemMercadoria         : strNAT_BC_CRED := '07';
          bccConArrendamentoMercantil      : strNAT_BC_CRED := '08';
          bccMaqCredDepreciacao            : strNAT_BC_CRED := '09';
          bccMaqCredAquisicao              : strNAT_BC_CRED := '10';
          bccAmortizacaoDepreciacaoImoveis : strNAT_BC_CRED := '11';
          bccDevolucaoSujeita              : strNAT_BC_CRED := '12';
          bccOutrasOpeComDirCredito        : strNAT_BC_CRED := '13';
          bccAtTransporteSubcontratacao    : strNAT_BC_CRED := '14';
          bccAtImobCustoIncorrido          : strNAT_BC_CRED := '15';
          bccAtImobCustoOrcado             : strNAT_BC_CRED := '16';
          bccAtPresServ                    : strNAT_BC_CRED := '17';
          bccEstoqueAberturaBens           : strNAT_BC_CRED := '18';
        end;

        Add( LFill('C505')           +
             LFill(CstCofinsToStr(CST_COFINS) ) +
             LFill(VL_ITEM,0,2)      +
             LFill(strNAT_BC_CRED)   +
             LFill(VL_BC_COFINS,0,2) +
             DFill(ALIQ_COFINS, 4)   +
             LFill(VL_COFINS,0,2)    +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC505Count := FRegistroC505Count + RegC500.RegistroC505.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC509(RegC500: TRegistroC500);
var
  intFor      : integer;
begin
  if Assigned(RegC500.RegistroC509) then
  begin
    for intFor := 0 to RegC500.RegistroC509.Count - 1 do
    begin
      with RegC500.RegistroC509.Items[intFor] do
      begin
        Add( LFill('C509')   +
             LFill(NUM_PROC) +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC509Count := FRegistroC509Count + RegC500.RegistroC509.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC600(RegC010: TRegistroC010);
var
  intFor: integer;
begin
  if Assigned( RegC010.RegistroC600 ) then
  begin
     for intFor := 0 to RegC010.RegistroC600.Count - 1 do
     begin
        with RegC010.RegistroC600.Items[intFor] do
        begin
          Add( LFill('C600')              +
               LFill( COD_MOD,2 )         +
               LFill( COD_MUN ,7 )        +
               LFill( SER,4 )             +
               LFill( SUB,3 )             +
               LFill( COD_CONS,2 )        +
               LFill( QTD_CONS,0 )        +
               LFill( QTD_CANC,0 )        +
               LFill( DT_DOC )            +
               LFill( VL_DOC,0,2 )        +
               LFill( VL_DESC,0,2 )       +
               LFill( CONS,0 )            +
               LFill( VL_FORN,0,2 )       +
               LFill( VL_SERV_NT,0,2 )    +
               LFill( VL_TERC,0,2 )       +
               LFill( VL_DA,0,2 )         +
               LFill( VL_BC_ICMS,0,2 )    +
               LFill( VL_ICMS,0,2 )       +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 )    +
               LFill( VL_PIS,0,2 )        +
               LFill( VL_COFINS,0,2 ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC601( RegC010.RegistroC600.Items[intFor] ) ;
        WriteRegistroC605( RegC010.RegistroC600.Items[intFor] ) ;
        WriteRegistroC609( RegC010.RegistroC600.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC600Count := FRegistroC600Count + RegC010.RegistroC600.Count;

     RegC010.RegistroC600.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC601(RegC600: TRegistroC600);
var
  intFor: integer;
begin
  if Assigned(RegC600.RegistroC601) then
  begin
    for intFor := 0 to RegC600.RegistroC601.Count - 1 do
    begin
      with RegC600.RegistroC601.Items[intFor] do
      begin
        Add( LFill('C601')        +
             LFill(CstPisToStr(CST_PIS)) +
             LFill(VL_ITEM,0,2)   +
             LFill(VL_BC_PIS,0,2) +
             DFill(ALIQ_PIS, 4)   +
             LFill(VL_PIS,0,2)    +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC601Count := FRegistroC601Count + RegC600.RegistroC601.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC605(RegC600: TRegistroC600);
  var
    intFor: integer;
begin
  if Assigned(RegC600.RegistroC605) then
  begin
    for intFor := 0 to RegC600.RegistroC605.Count - 1 do
    begin
      with RegC600.RegistroC605.Items[intFor] do
      begin
        Add( LFill('C605')           +
             LFill(CstCofinsToStr(CST_COFINS)) +
             LFill(VL_ITEM,0,2)      +
             LFill(VL_BC_COFINS,0,2) +
             DFill(ALIQ_COFINS, 4)   +
             LFill(VL_COFINS,0,2)    +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC605Count := FRegistroC605Count + RegC600.RegistroC605.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC609(RegC600: TRegistroC600);
  var
    intFor      : integer;
begin
  if Assigned(RegC600.RegistroC609) then
  begin
    for intFor := 0 to RegC600.RegistroC609.Count - 1 do
    begin
      with RegC600.RegistroC609.Items[intFor] do
      begin
        Add( LFill('C609')   +
             LFill(NUM_PROC) +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC609Count := FRegistroC609Count + RegC600.RegistroC609.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC860(RegC010: TRegistroC010);
  var
    intFor: integer;
  begin
    if Assigned( RegC010.RegistroC860 ) then
    begin
       for intFor := 0 to RegC010.RegistroC860.Count - 1 do
       begin
          with RegC010.RegistroC860.Items[intFor] do
          begin
            Add( LFill('C860')       +
                 LFill( COD_MOD, 2 ) +
                 LFill( NR_SAT, 9 )     +
                 LFill( DT_DOC)      +
                 LFill( DOC_INI, 6 )    +
                 LFill( DOC_FIM, 6 )    ) ;
          end;
          /// Registros FILHOS
          WriteRegistroC870( RegC010.RegistroC860.Items[intFor] ) ;
          WriteRegistroC880( RegC010.RegistroC860.Items[intFor] ) ;
          WriteRegistroC890( RegC010.RegistroC860.Items[intFor] ) ;

          RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
       end;
       /// Variavél para armazenar a quantidade de registro do tipo.
       Inc( FRegistroC860Count , RegC010.RegistroC860.Count);

       RegC010.RegistroC860.Clear;
    end;
end;

procedure TBloco_C.WriteRegistroC870(RegC860: TRegistroC860);
var
  intFor: integer;
begin
  if Assigned(RegC860.RegistroC870) then
  begin
    for intFor := 0 to RegC860.RegistroC870.Count - 1 do
    begin
      with RegC860.RegistroC870.Items[intFor] do
      begin
        Add( LFill('C870')        +
             LFill(COD_ITEM)      +
             LFill(CFOP, 4)       +
             LFill(VL_ITEM,0,2)   +
             LFill(VL_DESC,0,2)   +
             LFill(CstPisToStr(CST_PIS))       +
             LFill(VL_BC_PIS,0,2)              +
             DFill(ALIQ_PIS, 4 , (CST_PIS in [stpisValorAliquotaPorST]) )                +
             LFill(VL_PIS,0,2)                 +
             LFill(CstCofinsToStr(CST_COFINS)) +
             LFill(VL_BC_COFINS,0,2)           +
             DFill(ALIQ_COFINS, 4 , (CST_COFINS in [stcofinsValorAliquotaPorST]) )       +
             LFill(VL_COFINS,0,2)              +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    Inc( FRegistroC870Count , RegC860.RegistroC870.Count);
  end;
end;

procedure TBloco_C.WriteRegistroC880(RegC860: TRegistroC860);
var
  intFor: integer;
begin
  if Assigned(RegC860.RegistroC880) then
  begin
    for intFor := 0 to RegC860.RegistroC880.Count - 1 do
    begin
      with RegC860.RegistroC880.Items[intFor] do
      begin
        Add( LFill('C880')                     +
             LFill(COD_ITEM)                   +
             LFill(CFOP, 4)                    +
             LFill(VL_ITEM, 0, 2)              +
             LFill(CstPisToStr(CST_PIS))       +
             LFill(QUANT_BC_PIS,0,3)           +
             LFill(ALIQ_PIS_QUANT,0, 4)        +
             LFill(VL_PIS,0,2)                 +
             LFill(CstCofinsToStr(CST_COFINS)) +
             LFill(QUANT_BC_COFINS,0,3)        +
             LFill(ALIQ_COFINS_QUANT,0, 4)       +
             LFill(VL_COFINS,0,2)              +
             LFill(COD_CTA) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    Inc(FRegistroC880Count , RegC860.RegistroC880.Count);
  end;
end;

procedure TBloco_C.WriteRegistroC890(RegC860: TRegistroC860);
var
  intFor: integer;
begin
  if Assigned(RegC860.RegistroC890) then
  begin
    for intFor := 0 to RegC860.RegistroC890.Count - 1 do
    begin
      with RegC860.RegistroC890.Items[intFor] do
      begin
        Add( LFill('C890')        +
             LFill(NUM_PROC)       +
             LFill(ACBrOrigemProcessoStr[ Integer(IND_PROC)]) ) ;
        //
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    // Variavél para armazenar a quantidade de registro do tipo.
    Inc(FRegistroC890Count, RegC860.RegistroC890.Count);
  end;
end;

procedure TBloco_C.WriteRegistroC990 ;
begin
  if Assigned(RegistroC990) then
  begin
     with RegistroC990 do
     begin
       QTD_LIN_C := QTD_LIN_C + 1;
       ///
       Add( LFill('C990') +
            LFill(QTD_LIN_C,0) );
     end;
  end;
end;

end.
