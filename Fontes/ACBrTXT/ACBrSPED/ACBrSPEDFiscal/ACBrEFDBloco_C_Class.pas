{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrEFDBloco_C_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_C,
     ACBrEFDBloco_0_Class, ACBrEFDBlocos;

type
  /// TBLOCO_C -
  TBloco_C = class(TACBrSPED)
  private
    FOwner: TComponent;

//    FOnBeforeWriteRegistroC111: TWriteRegistroEvent;
//    FOnBeforeWriteRegistroC120: TWriteRegistroEvent;
//    FOnBeforeWriteRegistroC170: TWriteRegistroEvent;
//    FOnBeforeWriteRegistroC470: TWriteRegistroEvent;
//    FOnBeforeWriteRegistroC510: TWriteRegistroEvent;

//    FOnWriteRegistroC111: TWriteRegistroEvent;
//    FOnWriteRegistroC120: TWriteRegistroEvent;
//    FOnWriteRegistroC170: TWriteRegistroEvent;
//    FOnWriteRegistroC460: TWriteRegistroEvent;
//    FOnWriteRegistroC470: TWriteRegistroEvent;
//    FOnWriteRegistroC510: TWriteRegistroEvent;

//    FOnAfterWriteRegistroC111: TWriteRegistroEvent;
//    FOnAfterWriteRegistroC120: TWriteRegistroEvent;
//    FOnAfterWriteRegistroC170: TWriteRegistroEvent;
//    FOnAfterWriteRegistroC470: TWriteRegistroEvent;
//    FOnAfterWriteRegistroC510: TWriteRegistroEvent;

    FBloco_0: TBloco_0;

    FRegistroC001: TRegistroC001;      /// BLOCO C - RegistroC001
    FRegistroC990: TRegistroC990;      /// BLOCO C - RegistroC990

    FRegistroC100Count: Integer;
    FRegistroC101Count: Integer; 
    FRegistroC110Count: Integer;
    FRegistroC105Count: Integer;
    FRegistroC112Count: Integer;
    FRegistroC113Count: Integer;
    FRegistroC114Count: Integer;
    FRegistroC115Count: Integer;
    FRegistroC116Count: Integer;
    FRegistroC111Count: Integer;
    FRegistroC120Count: Integer;
    FRegistroC130Count: Integer;
    FRegistroC140Count: Integer;
    FRegistroC141Count: Integer;
    FRegistroC160Count: Integer;
    FRegistroC165Count: Integer;
    FRegistroC170Count: Integer;
    FRegistroC171Count: Integer;
    FRegistroC172Count: Integer;
    FRegistroC173Count: Integer;
    FRegistroC174Count: Integer;
    FRegistroC175Count: Integer;
    FRegistroC176Count: Integer;
    FRegistroC177Count: Integer;
    FRegistroC178Count: Integer;
    FRegistroC179Count: Integer;
    FRegistroC180Count: Integer;
    FRegistroC181Count: Integer;
    FRegistroC185Count: Integer;
    FRegistroC186Count: Integer;
    FRegistroC190Count: Integer;
    FRegistroC191Count: Integer;
    FRegistroC195Count: Integer;
    FRegistroC197Count: Integer;
    FRegistroC300Count: Integer;
    FRegistroC310Count: Integer;
    FRegistroC320Count: Integer;
    FRegistroC321Count: Integer;
    FRegistroC330Count: Integer;
    FRegistroC350Count: Integer;
    FRegistroC370Count: Integer;
    FRegistroC380Count: Integer;
    FRegistroC390Count: Integer;
    FRegistroC400Count: Integer;
    FRegistroC405Count: Integer;
    FRegistroC410Count: Integer;
    FRegistroC420Count: Integer;
    FRegistroC425Count: Integer;
    FRegistroC430Count: Integer;
    FRegistroC460Count: Integer;
    FRegistroC465Count: Integer;
    FRegistroC470Count: Integer;
    FRegistroC480Count: Integer;
    FRegistroC490Count: Integer;
    FRegistroC495Count: Integer;
    FRegistroC500Count: Integer;
    FRegistroC510Count: Integer;
    FRegistroC590Count: Integer;
    FRegistroC591Count: Integer;
    FRegistroC595Count: Integer;
    FRegistroC597Count: Integer;
    FRegistroC600Count: Integer;
    FRegistroC601Count: Integer;
    FRegistroC610Count: Integer;
    FRegistroC690Count: Integer;
    FRegistroC700Count: Integer;
    FRegistroC790Count: Integer;
    FRegistroC791Count: Integer;
    FRegistroC800Count: Integer;
    FRegistroC810Count: Integer;
    FRegistroC850Count: Integer;
    FRegistroC855Count: Integer;
    FRegistroC857Count: Integer;
    FRegistroC860Count: Integer;
    FRegistroC870Count: Integer;
    FRegistroC890Count: Integer;
    FRegistroC895Count: Integer;
    FRegistroC897Count: Integer;

    procedure WriteRegistroC100(RegC001: TRegistroC001);
    procedure WriteRegistroC101(RegC100: TRegistroC100);
    procedure WriteRegistroC105(RegC100: TRegistroC100);
    procedure WriteRegistroC110(RegC100: TRegistroC100);
    procedure WriteRegistroC111(RegC110: TRegistroC110);
    procedure WriteRegistroC112(RegC110: TRegistroC110);
    procedure WriteRegistroC113(RegC110: TRegistroC110);
    procedure WriteRegistroC114(RegC110: TRegistroC110);
    procedure WriteRegistroC115(RegC110: TRegistroC110);
    procedure WriteRegistroC116(RegC110: TRegistroC110);    
    procedure WriteRegistroC120(RegC100: TRegistroC100);
    procedure WriteRegistroC130(RegC100: TRegistroC100);
    procedure WriteRegistroC140(RegC100: TRegistroC100);
    procedure WriteRegistroC141(RegC140: TRegistroC140);
    procedure WriteRegistroC160(RegC100: TRegistroC100);
    procedure WriteRegistroC165(RegC100: TRegistroC100);
    procedure WriteRegistroC170(RegC100: TRegistroC100);
    procedure WriteRegistroC171(RegC170: TRegistroC170);
    procedure WriteRegistroC172(RegC170: TRegistroC170);
    procedure WriteRegistroC173(RegC170: TRegistroC170);
    procedure WriteRegistroC174(RegC170: TRegistroC170);
    procedure WriteRegistroC175(RegC170: TRegistroC170);
    procedure WriteRegistroC176(RegC170: TRegistroC170);
    procedure WriteRegistroC177(RegC170: TRegistroC170);
    procedure WriteRegistroC178(RegC170: TRegistroC170);
    procedure WriteRegistroC179(RegC170: TRegistroC170);
    procedure WriteRegistroC180(RegC100: TRegistroC100; RegC170: TRegistroC170);
    procedure WriteRegistroC181(RegC100: TRegistroC100; RegC170: TRegistroC170);
    procedure WriteRegistroC185(RegC100: TRegistroC100);
    procedure WriteRegistroC186(RegC100: TRegistroC100);
    procedure WriteRegistroC190(RegC100: TRegistroC100);
    procedure WriteRegistroC191(RegC190: TRegistroC190);
    procedure WriteRegistroC195(RegC100: TRegistroC100);
    procedure WriteRegistroC197(RegC195: TRegistroC195);
    procedure WriteRegistroC300(RegC001: TRegistroC001);
    procedure WriteRegistroC310(RegC300: TRegistroC300);
    procedure WriteRegistroC320(RegC300: TRegistroC300);
    procedure WriteRegistroC321(RegC320: TRegistroC320);
    procedure WriteRegistroC330(RegC321: TRegistroC321);
    procedure WriteRegistroC350(RegC001: TRegistroC001);
    procedure WriteRegistroC370(RegC350: TRegistroC350);
    procedure WriteRegistroC380(RegC370: TRegistroC370);
    procedure WriteRegistroC390(RegC350: TRegistroC350);
    procedure WriteRegistroC400(RegC001: TRegistroC001);
    procedure WriteRegistroC405(RegC400: TRegistroC400);
    procedure WriteRegistroC410(RegC405: TRegistroC405);
    procedure WriteRegistroC420(RegC405: TRegistroC405);
    procedure WriteRegistroC425(RegC420: TRegistroC420);
    procedure WriteRegistroC430(RegC425: TRegistroC425);
    procedure WriteRegistroC460(RegC405: TRegistroC405);
    procedure WriteRegistroC465(RegC460: TRegistroC460);
    procedure WriteRegistroC470(RegC460: TRegistroC460);
    procedure WriteRegistroC480(RegC470: TRegistroC470);
    procedure WriteRegistroC490(RegC405: TRegistroC405);
    procedure WriteRegistroC495(RegC001: TRegistroC001);
    procedure WriteRegistroC500(RegC001: TRegistroC001);
    procedure WriteRegistroC510(RegC500: TRegistroC500);
    procedure WriteRegistroC590(RegC500: TRegistroC500);
    procedure WriteRegistroC591(RegC590: TRegistroC590);
    procedure WriteRegistroC595(RegC500: TRegistroC500);
    procedure WriteRegistroC597(RegC595: TRegistroC595);
    procedure WriteRegistroC600(RegC001: TRegistroC001);
    procedure WriteRegistroC601(RegC600: TRegistroC600);
    procedure WriteRegistroC610(RegC600: TRegistroC600);
    procedure WriteRegistroC690(RegC600: TRegistroC600);
    procedure WriteRegistroC700(RegC001: TRegistroC001);
    procedure WriteRegistroC790(RegC700: TRegistroC700);
    procedure WriteRegistroC791(RegC790: TRegistroC790);
    procedure WriteRegistroC800(RegC001: TRegistroC001);
    procedure WriteRegistroC810(RegC800: TRegistroC800);
    procedure WriteRegistroC815(RegC810: TRegistroC810);
    procedure WriteRegistroC850(RegC800: TRegistroC800);
    procedure WriteRegistroC855(RegC800: TRegistroC800);
    procedure WriteRegistroC857(RegC855: TRegistroC855);
    procedure WriteRegistroC860(RegC001: TRegistroC001);
    procedure WriteRegistroC870(RegC860: TRegistroC860);
    procedure WriteRegistroC880(RegC870: TRegistroC870);
    procedure WriteRegistroC890(RegC860: TRegistroC860);
    procedure WriteRegistroC895(RegC860: TRegistroC860);
    procedure WriteRegistroC897(RegC895: TRegistroC895);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create(AOwner: TComponent); overload;
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function RegistroC001New: TRegistroC001;
    function RegistroC100New: TRegistroC100;

    function RegistroC101New: TRegistroC101; 
	
    function RegistroC110New: TRegistroC110;

    function RegistroC105New: TRegistroC105;
    function RegistroC111New: TRegistroC111;
    function RegistroC112New: TRegistroC112;
    function RegistroC113New: TRegistroC113;
    function RegistroC114New: TRegistroC114;
    function RegistroC115New: TRegistroC115;
    function RegistroC116New: TRegistroC116;    
    function RegistroC120New: TRegistroC120;
    function RegistroC130New: TRegistroC130;
    function RegistroC140New: TRegistroC140;
    function RegistroC141New: TRegistroC141;
    function RegistroC160New: TRegistroC160;
    function RegistroC165New: TRegistroC165;
    function RegistroC170New: TRegistroC170;
    function RegistroC171New: TRegistroC171;
    function RegistroC172New: TRegistroC172;
    function RegistroC173New: TRegistroC173;
    function RegistroC174New: TRegistroC174;
    function RegistroC175New: TRegistroC175;
    function RegistroC176New: TRegistroC176;
    function RegistroC177New: TRegistroC177;
    function RegistroC178New: TRegistroC178;
    function RegistroC179New: TRegistroC179;
    function RegistroC180New: TRegistroC180;
    function RegistroC181New: TRegistroC181;
    function RegistroC185New: TRegistroC185;
    function RegistroC186New: TRegistroC186;
    function RegistroC190New: TRegistroC190;
    function RegistroC191New: TRegistroC191;
    function RegistroC195New: TRegistroC195;
    function RegistroC197New: TRegistroC197;
    function RegistroC300New: TRegistroC300;
    function RegistroC310New: TRegistroC310;
    function RegistroC320New: TRegistroC320;
    function RegistroC321New: TRegistroC321;
    function RegistroC330New: TRegistroC330;
    function RegistroC350New: TRegistroC350;
    function RegistroC370New: TRegistroC370;
    function RegistroC380New: TRegistroC380;
    function RegistroC390New: TRegistroC390;
    function RegistroC400New: TRegistroC400;
    function RegistroC405New: TRegistroC405;
    function RegistroC410New: TRegistroC410;
    function RegistroC420New: TRegistroC420;
    function RegistroC425New: TRegistroC425;
    function RegistroC430New: TRegistroC430;
    function RegistroC460New: TRegistroC460;
    function RegistroC465New: TRegistroC465;
    function registroC470New: TRegistroC470;
    function registroC480New: TRegistroC480;
    function RegistroC490New: TRegistroC490;
    function RegistroC495New: TRegistroC495;
    function RegistroC500New: TRegistroC500;
    function RegistroC510New: TRegistroC510;
    function RegistroC590New: TRegistroC590;
    function RegistroC591New: TRegistroC591;
    function RegistroC595New: TRegistroC595;
    function RegistroC597New: TRegistroC597;
    function RegistroC600New: TRegistroC600;
    function RegistroC601New: TRegistroC601;
    function RegistroC610New: TRegistroC610;
    function RegistroC690New: TRegistroC690;
    function RegistroC700New: TRegistroC700;
    function RegistroC790New: TRegistroC790;
    function registroC791New: TRegistroC791;
    function RegistroC800New: TRegistroC800;
    function RegistroC810New: TRegistroC810;
    function RegistroC850New: TRegistroC850;
    function RegistroC855New: TRegistroC855;
    function RegistroC857New: TRegistroC857;
    function RegistroC860New: TRegistroC860;
    function RegistroC890New: TRegistroC890;
    function RegistroC895New: TRegistroC895;
    function RegistroC897New: TRegistroC897;

    procedure WriteRegistroC001;
    procedure WriteRegistroC990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroC001: TRegistroC001 read FRegistroC001 write FRegistroC001;
    property RegistroC990: TRegistroC990 read FRegistroC990 write FRegistroC990;

    property RegistroC100Count: Integer read FRegistroC100Count write FRegistroC100Count;

    property RegistroC101Count: Integer read FRegistroC101Count write FRegistroC101Count;
	
    property RegistroC105Count: Integer read FRegistroC105Count write FRegistroC105Count;
    property RegistroC110Count: Integer read FRegistroC110Count write FRegistroC110Count;
    property RegistroC112Count: Integer read FRegistroC112Count write FRegistroC112Count;
    property RegistroC113Count: Integer read FRegistroC113Count write FRegistroC113Count;
    property RegistroC114Count: Integer read FRegistroC114Count write FRegistroC114Count;
    property RegistroC115Count: Integer read FRegistroC115Count write FRegistroC115Count;
    property RegistroC116Count: Integer read FRegistroC116Count write FRegistroC116Count;    
    property RegistroC111Count: Integer read FRegistroC111Count write FRegistroC111Count;
    property RegistroC120Count: Integer read FRegistroC120Count write FRegistroC120Count;
    property RegistroC130Count: Integer read FRegistroC130Count write FRegistroC130Count;
    property RegistroC140Count: Integer read FRegistroC140Count write FRegistroC140Count;
    property RegistroC141Count: Integer read FRegistroC141Count write FRegistroC141Count;
    property RegistroC160Count: Integer read FRegistroC160Count write FRegistroC160Count;
    property RegistroC165Count: Integer read FRegistroC165Count write FRegistroC165Count;
    property RegistroC170Count: Integer read FRegistroC170Count write FRegistroC170Count;
    property RegistroC171Count: Integer read FRegistroC171Count write FRegistroC171Count;
    property RegistroC172Count: Integer read FRegistroC172Count write FRegistroC172Count;
    property RegistroC173Count: Integer read FRegistroC173Count write FRegistroC173Count;
    property RegistroC174Count: Integer read FRegistroC174Count write FRegistroC174Count;
    property RegistroC175Count: Integer read FRegistroC175Count write FRegistroC175Count;
    property RegistroC176Count: Integer read FRegistroC176Count write FRegistroC176Count;
    property RegistroC177Count: Integer read FRegistroC177Count write FRegistroC177Count;
    property RegistroC178Count: Integer read FRegistroC178Count write FRegistroC178Count;
    property RegistroC179Count: Integer read FRegistroC179Count write FRegistroC179Count;
    property RegistroC180Count: Integer read FRegistroC180Count write FRegistroC180Count;
    property RegistroC181Count: Integer read FRegistroC181Count write FRegistroC181Count;
    property RegistroC185Count: Integer read FRegistroC185Count write FRegistroC185Count;
    property RegistroC186Count: Integer read FRegistroC186Count write FRegistroC186Count;
    property RegistroC190Count: Integer read FRegistroC190Count write FRegistroC190Count;
    property RegistroC191Count: Integer read FRegistroC191Count write FRegistroC191Count;
    property RegistroC195Count: Integer read FRegistroC195Count write FRegistroC195Count;
    property RegistroC197Count: Integer read FRegistroC197Count write FRegistroC197Count;
    property RegistroC300Count: Integer read FRegistroC300Count write FRegistroC300Count;
    property RegistroC310Count: Integer read FRegistroC310Count write FRegistroC310Count;
    property RegistroC320Count: Integer read FRegistroC320Count write FRegistroC320Count;
    property RegistroC321Count: Integer read FRegistroC321Count write FRegistroC321Count;
    property RegistroC330Count: Integer read FRegistroC330Count write FRegistroC330Count;
    property RegistroC350Count: Integer read FRegistroC350Count write FRegistroC350Count;
    property RegistroC370Count: Integer read FRegistroC370Count write FRegistroC370Count;
    property RegistroC380Count: Integer read FRegistroC380Count write FRegistroC380Count;
    property RegistroC390Count: Integer read FRegistroC390Count write FRegistroC390Count;
    property RegistroC400Count: Integer read FRegistroC400Count write FRegistroC400Count;
    property RegistroC405Count: Integer read FRegistroC405Count write FRegistroC405Count;
    property RegistroC410Count: Integer read FRegistroC410Count write FRegistroC410Count;
    property RegistroC420Count: Integer read FRegistroC420Count write FRegistroC420Count;
    property RegistroC425Count: Integer read FRegistroC425Count write FRegistroC425Count;
    property RegistroC430Count: Integer read FRegistroC430Count write FRegistroC430Count;
    property RegistroC460Count: Integer read FRegistroC460Count write FRegistroC460Count;
    property RegistroC465Count: Integer read FRegistroC465Count write FRegistroC465Count;
    property RegistroC470Count: Integer read FRegistroC470Count write FRegistroC470Count;
    property RegistroC480Count: Integer read FRegistroC480Count write FRegistroC480Count;
    property RegistroC490Count: Integer read FRegistroC490Count write FRegistroC490Count;
    property RegistroC495Count: Integer read FRegistroC495Count write FRegistroC495Count;
    property RegistroC500Count: Integer read FRegistroC500Count write FRegistroC500Count;
    property RegistroC510Count: Integer read FRegistroC510Count write FRegistroC510Count;
    property RegistroC590Count: Integer read FRegistroC590Count write FRegistroC590Count;
    property RegistroC591Count: Integer read FRegistroC591Count write FRegistroC591Count;
    property RegistroC595Count: Integer read FRegistroC595Count write FRegistroC595Count;
    property RegistroC597Count: Integer read FRegistroC597Count write FRegistroC597Count;
    property RegistroC600Count: Integer read FRegistroC600Count write FRegistroC600Count;
    property RegistroC601Count: Integer read FRegistroC601Count write FRegistroC601Count;
    property RegistroC610Count: Integer read FRegistroC610Count write FRegistroC610Count;
    property RegistroC690Count: Integer read FRegistroC690Count write FRegistroC690Count;
    property RegistroC700Count: Integer read FRegistroC700Count write FRegistroC700Count;
    property RegistroC790Count: Integer read FRegistroC790Count write FRegistroC790Count;
    property RegistroC791Count: Integer read FRegistroC791Count write FRegistroC791Count;
    property RegistroC800Count: Integer read FRegistroC800Count write FRegistroC800Count;
    property RegistroC810Count: Integer read FRegistroC810Count write FRegistroC810Count;
    property RegistroC850Count: Integer read FRegistroC850Count write FRegistroC850Count;
    property RegistroC855Count: Integer read FRegistroC855Count write FRegistroC855Count;
    property RegistroC857Count: Integer read FRegistroC857Count write FRegistroC857Count;
    property RegistroC860Count: Integer read FRegistroC860Count write FRegistroC860Count;
    property RegistroC870Count: Integer read FRegistroC870Count write FRegistroC870Count;
    property RegistroC890Count: Integer read FRegistroC890Count write FRegistroC890Count;
    property RegistroC895Count: Integer read FRegistroC895Count write FRegistroC895Count;
    property RegistroC897Count: Integer read FRegistroC897Count write FRegistroC897Count;

//    property OnBeforeWriteRegistroC111: TWriteRegistroEvent read FOnBeforeWriteRegistroC111 write FOnBeforeWriteRegistroC111;
//    property OnBeforeWriteRegistroC120: TWriteRegistroEvent read FOnBeforeWriteRegistroC120 write FOnBeforeWriteRegistroC120;
//    property OnBeforeWriteRegistroC170: TWriteRegistroEvent read FOnBeforeWriteRegistroC170 write FOnBeforeWriteRegistroC170;
//    property OnBeforeWriteRegistroC470: TWriteRegistroEvent read FOnBeforeWriteRegistroC470 write FOnBeforeWriteRegistroC470;
//    property OnBeforeWriteRegistroC510: TWriteRegistroEvent read FOnBeforeWriteRegistroC510 write FOnBeforeWriteRegistroC510;

//    property OnWriteRegistroC111: TWriteRegistroEvent read FOnWriteRegistroC111 write FOnWriteRegistroC111;
//    property OnWriteRegistroC120: TWriteRegistroEvent read FOnWriteRegistroC120 write FOnWriteRegistroC120;
//    property OnWriteRegistroC170: TWriteRegistroEvent read FOnWriteRegistroC170 write FOnWriteRegistroC170;
//    property OnWriteRegistroC460: TWriteRegistroEvent read FOnWriteRegistroC460 write FOnWriteRegistroC460;
//    property OnWriteRegistroC470: TWriteRegistroEvent read FOnWriteRegistroC470 write FOnWriteRegistroC470;
//    property OnWriteRegistroC510: TWriteRegistroEvent read FOnWriteRegistroC510 write FOnWriteRegistroC510;

//    property OnAfterWriteRegistroC111: TWriteRegistroEvent read FOnAfterWriteRegistroC111 write FOnAfterWriteRegistroC111;
//    property OnAfterWriteRegistroC120: TWriteRegistroEvent read FOnAfterWriteRegistroC120 write FOnAfterWriteRegistroC120;
//    property OnAfterWriteRegistroC170: TWriteRegistroEvent read FOnAfterWriteRegistroC170 write FOnAfterWriteRegistroC170;
//    property OnAfterWriteRegistroC470: TWriteRegistroEvent read FOnAfterWriteRegistroC470 write FOnAfterWriteRegistroC470;
//    property OnAfterWriteRegistroC510: TWriteRegistroEvent read FOnAfterWriteRegistroC510 write FOnAfterWriteRegistroC510;
  end;

implementation

uses ACBrSpedFiscal, ACBrTXTUtils, ACBrUtil.Strings, strutils, Variants;

{ TBloco_C }

constructor TBloco_C.Create(AOwner: TComponent);
begin
   inherited Create;
   FOwner := AOwner;
   CriaRegistros;
end;

destructor TBloco_C.Destroy;
begin
  FOwner := nil;
  LiberaRegistros;
  inherited;
end;

procedure TBloco_C.CriaRegistros;
begin
  FRegistroC001 := TRegistroC001.Create;
  FRegistroC990 := TRegistroC990.Create;

  FRegistroC100Count := 0;
  FRegistroC101Count := 0;
  FRegistroC105Count := 0;
  FRegistroC110Count := 0;
  FRegistroC112Count := 0;
  FRegistroC113Count := 0;
  FRegistroC114Count := 0;
  FRegistroC115Count := 0;
  FRegistroC116Count := 0;
  FRegistroC111Count := 0;
  FRegistroC120Count := 0;
  FRegistroC130Count := 0;
  FRegistroC140Count := 0;
  FRegistroC141Count := 0;
  FRegistroC160Count := 0;
  FRegistroC165Count := 0;
  FRegistroC170Count := 0;
  FRegistroC171Count := 0;
  FRegistroC172Count := 0;
  FRegistroC173Count := 0;
  FRegistroC174Count := 0;
  FRegistroC175Count := 0;
  FRegistroC176Count := 0;
  FRegistroC177Count := 0;
  FRegistroC178Count := 0;
  FRegistroC179Count := 0;
  FRegistroC180Count := 0;
  FRegistroC181Count := 0;
  FRegistroC185Count := 0;
  FRegistroC186Count := 0;
  FRegistroC190Count := 0;
  FRegistroC191Count := 0;
  FRegistroC195Count := 0;
  FRegistroC197Count := 0;
  FRegistroC300Count := 0;
  FRegistroC310Count := 0;
  FRegistroC320Count := 0;
  FRegistroC321Count := 0;
  FRegistroC330Count := 0;
  FRegistroC350Count := 0;
  FRegistroC370Count := 0;
  FRegistroC380Count := 0;
  FRegistroC390Count := 0;
  FRegistroC400Count := 0;
  FRegistroC405Count := 0;
  FRegistroC410Count := 0;
  FRegistroC420Count := 0;
  FRegistroC425Count := 0;
  FRegistroC430Count := 0;
  FRegistroC460Count := 0;
  FRegistroC465Count := 0;
  FRegistroC470Count := 0;
  FRegistroC480Count := 0;
  FRegistroC490Count := 0;
  FRegistroC495Count := 0;
  FRegistroC500Count := 0;
  FRegistroC510Count := 0;
  FRegistroC590Count := 0;
  FRegistroC591Count := 0;
  FRegistroC595Count := 0;
  FRegistroC597Count := 0;
  FRegistroC600Count := 0;
  FRegistroC601Count := 0;
  FRegistroC610Count := 0;
  FRegistroC690Count := 0;
  FRegistroC700Count := 0;
  FRegistroC790Count := 0;
  FRegistroC791Count := 0;
  FRegistroC800Count := 0;
  FRegistroC810Count := 0;
  FRegistroC850Count := 0;
  FRegistroC855Count := 0;
  FRegistroC857Count := 0;
  FRegistroC860Count := 0;
  FRegistroC870Count := 0;
  FRegistroC890Count := 0;
  FRegistroC895Count := 0;
  FRegistroC897Count := 0;

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

function TBloco_C.RegistroC100New: TRegistroC100;
begin
   Result := FRegistroC001.RegistroC100.New();
end;

function TBloco_C.RegistroC101New: TRegistroC101;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC101.New;
end;


function TBloco_C.RegistroC105New: TRegistroC105;
var
C100: TRegistroC100;
C100Count: Integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   if C100Count = -1 then
      raise EACBrSPEDFiscalException.Create('O registro C105 deve ser filho do registro C100, e não existe nenhum C100 pai!');

   C100   := FRegistroC001.RegistroC100.Items[C100Count];
   Result := C100.RegistroC105.New(C100);
end;

function TBloco_C.RegistroC110New: TRegistroC110;
var
C100: TRegistroC100;
C100Count: Integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   if C100Count = -1 then
      raise EACBrSPEDFiscalException.Create('O registro C105 deve ser filho do registro C100, e não existe nenhum C100 pai!');

   C100   := FRegistroC001.RegistroC100.Items[C100Count];
   Result := C100.RegistroC110.New();
end;

function TBloco_C.RegistroC111New: TRegistroC111;
var
C100Count: integer;
C110Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C110Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Items[C110Count].RegistroC111.New;
end;

function TBloco_C.RegistroC112New: TRegistroC112;
var
C100Count: integer;
C110Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C110Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Items[C110Count].RegistroC112.New;
end;

function TBloco_C.RegistroC113New: TRegistroC113;
var
C100Count: integer;
C110Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C110Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Items[C110Count].RegistroC113.New;
end;

function TBloco_C.RegistroC114New: TRegistroC114;
var
C100Count: integer;
C110Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C110Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Items[C110Count].RegistroC114.New;
end;

function TBloco_C.RegistroC115New: TRegistroC115;
var
C100Count: integer;
C110Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C110Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Items[C110Count].RegistroC115.New;
end;

function TBloco_C.RegistroC116New: TRegistroC116;
var
C100Count: integer;
C110Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C110Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC110.Items[C110Count].RegistroC116.New;
end;

function TBloco_C.RegistroC120New: TRegistroC120;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC120.New;
end;

function TBloco_C.RegistroC130New: TRegistroC130;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC130.New;
end;

function TBloco_C.RegistroC140New: TRegistroC140;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC140.New;
end;

function TBloco_C.RegistroC141New: TRegistroC141;
var
C100Count: integer;
C140Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C140Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC140.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC140.Items[C140Count].RegistroC141.New;
end;

function TBloco_C.RegistroC160New: TRegistroC160;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC160.New;
end;

function TBloco_C.RegistroC165New: TRegistroC165;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC165.New;
end;

function TBloco_C.RegistroC170New: TRegistroC170;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC170.New;
end;

function TBloco_C.RegistroC171New: TRegistroC171;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC171.New;
end;

function TBloco_C.RegistroC172New: TRegistroC172;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC172.New;
end;

function TBloco_C.RegistroC173New: TRegistroC173;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC173.New;
end;

function TBloco_C.RegistroC174New: TRegistroC174;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC174.New;
end;

function TBloco_C.RegistroC175New: TRegistroC175;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC175.New;
end;

function TBloco_C.RegistroC176New: TRegistroC176;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC176.New;
end;

function TBloco_C.RegistroC177New: TRegistroC177;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC177.New;
end;

function TBloco_C.RegistroC178New: TRegistroC178;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC178.New;
end;

function TBloco_C.RegistroC179New: TRegistroC179;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC179.New;
end;

function TBloco_C.RegistroC180New: TRegistroC180;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC180.New;
end;

function TBloco_C.RegistroC181New: TRegistroC181;
var
C100Count: integer;
C170Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C170Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC170.Items[C170Count].RegistroC181.New;
end;

function TBloco_C.RegistroC185New: TRegistroC185;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC185.New;
end;

function TBloco_C.RegistroC186New: TRegistroC186;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC186.New;
end;

function TBloco_C.RegistroC190New: TRegistroC190;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC190.New;
end;

function TBloco_C.RegistroC191New: TRegistroC191;
var
C100Count: integer;
C190Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C190Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC190.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC190.Items[C190Count].RegistroC191.New;
end;

function TBloco_C.RegistroC195New: TRegistroC195;
begin
   Result := FRegistroC001.RegistroC100.Items[FRegistroC001.RegistroC100.Count -1].RegistroC195.New;
end;

function TBloco_C.RegistroC197New: TRegistroC197;
var
C100Count: integer;
C195Count: integer;
begin
   C100Count := FRegistroC001.RegistroC100.Count -1;
   C195Count := FRegistroC001.RegistroC100.Items[C100Count].RegistroC195.Count -1;
   //
   Result := FRegistroC001.RegistroC100.Items[C100Count].RegistroC195.Items[C195Count].RegistroC197.New;
end;

function TBloco_C.RegistroC300New: TRegistroC300;
begin
   Result := FRegistroC001.RegistroC300.New;
end;

function TBloco_C.RegistroC310New: TRegistroC310;
var
   C300Count: integer;
begin
   C300Count := FRegistroC001.RegistroC300.Count -1;
   if C300Count = -1 then
      raise Exception.Create('O registro C310 deve ser filho do registro C300, e não existe nenhum C300 pai!');
	  
   Result := FRegistroC001.RegistroC300.Items[C300Count].RegistroC310.New;
end;

function TBloco_C.RegistroC320New: TRegistroC320;
var
   C300Count: integer;
begin
  C300Count := FRegistroC001.RegistroC300.Count -1;
  if C300Count = -1 then
    raise Exception.Create('O registro C320 deve ser filho do registro C300, e não existe nenhum C300 pai!');

  Result := FRegistroC001.RegistroC300.Items[C300Count].RegistroC320.New;
end;

function TBloco_C.RegistroC321New: TRegistroC321;
var
C300Count: integer;
C320Count: integer;
begin
  C300Count := FRegistroC001.RegistroC300.Count -1;
  if C300Count = -1 then
    raise Exception.Create('O registro C321 deve ser filho de um registro C320 filho de um registro C300, e não existe nenhum C300 pai!');

  C320Count := FRegistroC001.RegistroC300.Items[C300Count].RegistroC320.Count -1;
  if C320Count = -1 then
    raise Exception.Create('O registro C321 deve ser filho do registro C320, e não existe nenhum C320 pai!');

   //
  Result := FRegistroC001.RegistroC300.Items[C300Count].RegistroC320.Items[C320Count].RegistroC321.New;
end;

function TBloco_C.RegistroC330New: TRegistroC330;
var
  C300Count: integer;
  C320Count: integer;
  C321Count: integer;
begin
  C300Count := FRegistroC001.RegistroC300.Count -1;
  if C300Count = -1 then
    raise Exception.Create('O registro C330 deve ser filho de um registro C321 filho de um registro C320 filho de um registro C300, e não existe nenhum C300 pai!');

  C320Count := FRegistroC001.RegistroC300.Items[C300Count].RegistroC320.Count -1;
  if C320Count = -1 then
    raise Exception.Create('O registro C330 deve ser filho do registro C321 filho de um registro C320, e não existe nenhum C320 pai!');

  C321Count := FRegistroC001.RegistroC300.Items[C300Count].RegistroC320.Items[C320Count].RegistroC321.Count -1;
  if C321Count = -1 then
    raise Exception.Create('O registro C330 deve ser filho do registro C321, e não existe nenhum C321 pai!');
  //
  Result := FRegistroC001.RegistroC300.Items[C300Count].RegistroC320.Items[C320Count].RegistroC321.Items[C321Count].RegistroC330.New;
end;

function TBloco_C.RegistroC350New: TRegistroC350;
begin
   Result := FRegistroC001.RegistroC350.New;
end;

function TBloco_C.RegistroC370New: TRegistroC370;
begin
   Result := FRegistroC001.RegistroC350.Items[FRegistroC001.RegistroC350.Count -1].RegistroC370.New;
end;

function TBloco_C.RegistroC380New: TRegistroC380;
var
  C350Count: integer;
  C370Count: integer;
begin
  C350Count := FRegistroC001.RegistroC350.Count -1;
  C370Count := FRegistroC001.RegistroC350.Items[C350Count].RegistroC370.Count -1;
  //
  Result := FRegistroC001.RegistroC350.Items[C350Count].RegistroC370.Items[C370Count].RegistroC380.New;
end;


function TBloco_C.RegistroC390New: TRegistroC390;
begin
   Result := FRegistroC001.RegistroC350.Items[FRegistroC001.RegistroC350.Count -1].RegistroC390.New;
end;

function TBloco_C.RegistroC400New: TRegistroC400;
begin
   Result := FRegistroC001.RegistroC400.New;
end;

function TBloco_C.RegistroC405New: TRegistroC405;
begin
   Result := FRegistroC001.RegistroC400.Items[FRegistroC001.RegistroC400.Count -1].RegistroC405.New;
end;

function TBloco_C.RegistroC420New: TRegistroC420;
var
C400Count: integer;
C405Count: integer;
begin
   C400Count := FRegistroC001.RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
   //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC420.New;
end;

function TBloco_C.RegistroC425New: TRegistroC425;
var
C400Count: integer;
C405Count: integer;
C420Count: integer;
begin
   C400Count := FRegistroC001.RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
   C420Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC420.Count -1;
   //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC420.Items[C420Count].RegistroC425.New;
end;

function TBloco_C.RegistroC430New: TRegistroC430;
var
  C400Count: integer;
  C405Count: integer;
  C420Count: integer;
  C425Count: integer;
begin
  C400Count := FRegistroC001.RegistroC400.Count -1;
  C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
  C420Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC420.Count -1;
  C425Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC420.Items[C420Count].RegistroC425.Count -1;
  //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC420.Items[C420Count].RegistroC425.Items[C425Count].RegistroC430.New;
end;

function TBloco_C.RegistroC460New: TRegistroC460;
var
C400Count: integer;
C405Count: integer;
begin
   C400Count := FRegistroC001.RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
   //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.New;
end;

function TBloco_C.RegistroC465New: TRegistroC465;
var
C400Count: integer;
C405Count: integer;
C460Count: integer;
begin
   C400Count := FRegistroC001.RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
   C460Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.Count -1;
   //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.Items[C460Count].RegistroC465.New;
end;

function TBloco_C.registroC470New: TRegistroC470;
var
C400Count: integer;
C405Count: integer;
C460Count: integer;
begin
   C400Count := FRegistroC001.RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
   C460Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.Count -1;
   //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.Items[C460Count].RegistroC470.New;
end;

function TBloco_C.registroC480New: TRegistroC480;
var
  C400Count: integer;
  C405Count: integer;
  C460Count: integer;
  C470Count: integer;
begin
  C400Count := FRegistroC001.RegistroC400.Count -1;
  C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
  C460Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.Count -1;
  C470Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.Items[C460Count].RegistroC470.Count -1;
  //
  Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC460.Items[C460Count].RegistroC470.Items[C470Count].RegistroC480.New;
end;

function TBloco_C.RegistroC490New: TRegistroC490;
var
C400Count: integer;
C405Count: integer;
begin
   C400Count := FRegistroC001.RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
   //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC490.New;
end;

function TBloco_C.RegistroC495New: TRegistroC495;
begin
   Result := FRegistroC001.RegistroC495.New;
end;

function TBloco_C.RegistroC500New: TRegistroC500;
begin
   Result := FRegistroC001.RegistroC500.New;
end;

function TBloco_C.RegistroC510New: TRegistroC510;
begin
   Result := FRegistroC001.RegistroC500.Items[FRegistroC001.RegistroC500.Count -1].RegistroC510.New;
end;

function TBloco_C.RegistroC590New: TRegistroC590;
begin
   Result := FRegistroC001.RegistroC500.Items[FRegistroC001.RegistroC500.Count -1].RegistroC590.New;
end;

function TBloco_C.RegistroC591New: TRegistroC591;
var
C500Count: integer;
C590Count: integer;
begin
   C500Count := FRegistroC001.RegistroC500.Count -1;
   C590Count := FRegistroC001.RegistroC500.Items[C500Count].RegistroC590.Count -1;
   //
   Result := FRegistroC001.RegistroC500.Items[C500Count].RegistroC590.Items[C590Count].RegistroC591.New;
end;

function TBloco_C.RegistroC595New: TRegistroC595;
begin
   Result := FRegistroC001.RegistroC500.Items[FRegistroC001.RegistroC500.Count -1].RegistroC595.New;
end;

function TBloco_C.RegistroC597New: TRegistroC597;
var
C500Count: integer;
C595Count: integer;
begin
   C500Count := FRegistroC001.RegistroC500.Count -1;
   C595Count := FRegistroC001.RegistroC500.Items[C500Count].RegistroC595.Count -1;
   //
   Result := FRegistroC001.RegistroC500.Items[C500Count].RegistroC595.Items[C595Count].RegistroC597.New;
end;

function TBloco_C.RegistroC600New: TRegistroC600;
begin
   Result := FRegistroC001.RegistroC600.New;
end;

function TBloco_C.RegistroC601New: TRegistroC601;
begin
   Result := FRegistroC001.RegistroC600.Items[FRegistroC001.RegistroC600.Count -1].RegistroC601.New;
end;

function TBloco_C.RegistroC610New: TRegistroC610;
begin
   Result := FRegistroC001.RegistroC600.Items[FRegistroC001.RegistroC600.Count -1].RegistroC610.New;
end;

function TBloco_C.RegistroC690New: TRegistroC690;
begin
   Result := FRegistroC001.RegistroC600.Items[FRegistroC001.RegistroC600.Count -1].RegistroC690.New;
end;

function TBloco_C.RegistroC700New: TRegistroC700;
begin
   Result := FRegistroC001.RegistroC700.New;
end;

function TBloco_C.RegistroC790New: TRegistroC790;
begin
   Result := FRegistroC001.RegistroC700.Items[FRegistroC001.RegistroC700.Count -1].RegistroC790.New;
end;

function TBloco_C.registroC791New: TRegistroC791;
var
C700Count: integer;
C790Count: integer;
begin
   C700Count := FRegistroC001.RegistroC700.Count -1;
   C790Count := FRegistroC001.RegistroC700.Items[C700Count].RegistroC790.Count -1;
   //
   Result := FRegistroC001.RegistroC700.Items[C700Count].RegistroC790.Items[C790Count].RegistroC791.New;
end;

function TBloco_C.RegistroC800New: TRegistroC800;
begin
   Result := FRegistroC001.RegistroC800.New;
end;

function TBloco_C.RegistroC810New: TRegistroC810;
begin
   Result := FRegistroC001.RegistroC800.Items[FRegistroC001.RegistroC800.Count -1].RegistroC810.New;
end;

function TBloco_C.RegistroC850New: TRegistroC850;
begin
   Result := FRegistroC001.RegistroC800.Items[FRegistroC001.RegistroC800.Count -1].RegistroC850.New;
end;

function TBloco_C.RegistroC855New: TRegistroC855;
begin
  Result := FRegistroC001.RegistroC800.Items[FRegistroC001.RegistroC800.Count -1].RegistroC855.New;
end;

function TBloco_C.RegistroC857New: TRegistroC857;
var
  wUltimoC800, wUltimoC855: Integer;
begin
  wUltimoC800 := (FRegistroC001.RegistroC800.Count -1);
  wUltimoC855 := (FRegistroC001.RegistroC800.Items[wUltimoC800].RegistroC855.Count - 1);
  Result := FRegistroC001.RegistroC800.Items[wUltimoC800].RegistroC855.Items[wUltimoC855].RegistroC857.New;
end;

function TBloco_C.RegistroC860New: TRegistroC860;
begin
   Result := FRegistroC001.RegistroC860.New;
end;

function TBloco_C.RegistroC890New: TRegistroC890;
begin
   Result := FRegistroC001.RegistroC860.Items[FRegistroC001.RegistroC860.Count -1].RegistroC890.New;
end;

function TBloco_C.RegistroC895New: TRegistroC895;
begin
  Result := FRegistroC001.RegistroC860.Items[FRegistroC001.RegistroC860.Count -1].RegistroC895.New;
end;

function TBloco_C.RegistroC897New: TRegistroC897;
var
  wUltimoC860, wUltimoC895: Integer;
begin
  wUltimoC860 := FRegistroC001.RegistroC860.Count - 1;
  wUltimoC895 := FRegistroC001.RegistroC860.Items[wUltimoC860].RegistroC895.Count - 1;
  Result := FRegistroC001.RegistroC860.Items[wUltimoC860].RegistroC895.Items[wUltimoC895].RegistroC897.New;
end;

function TBloco_C.RegistroC410New: TRegistroC410;
var
C400Count: integer;
C405Count: integer;
begin
   C400Count := FRegistroC001.RegistroC400.Count -1;
   C405Count := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Count -1;
   //
   Result := FRegistroC001.RegistroC400.Items[C400Count].RegistroC405.Items[C405Count].RegistroC410.New;
end;

procedure TBloco_C.WriteRegistroC001;
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
       WriteRegistroC100( FRegistroC001 ) ;
       WriteRegistroC300( FRegistroC001 ) ;
       WriteRegistroC350( FRegistroC001 ) ;
       WriteRegistroC400( FRegistroC001 ) ;
       WriteRegistroC495( FRegistroC001 ) ;
       WriteRegistroC500( FRegistroC001 ) ;
       WriteRegistroC600( FRegistroC001 ) ;
       WriteRegistroC700( FRegistroC001 ) ;
       WriteRegistroC800( FRegistroC001 ) ;
       WriteRegistroC860( FRegistroC001 ) ;
     end;
  end;
end;

procedure TBloco_C.WriteRegistroC100(RegC001: TRegistroC001) ;
var
  intFor: integer;
  strIND_FRT: String;
  strIND_PGTO: String;
  strCOD_SIT: String;
  booNFCancelada: Boolean; /// Variavél p/ tratamento de NFs canceladas, denegadas ou inutilizada
  booAborta: Boolean;
begin
  booAborta := False;

  if Assigned( RegC001.RegistroC100 ) then
  begin
    for intFor := 0 to RegC001.RegistroC100.Count - 1 do
    begin
      if Assigned(TACBrSPEDFiscal(FOwner).ChecksBloco_C.OnCheckRegistroC100) then
         TACBrSPEDFiscal(FOwner).ChecksBloco_C.OnCheckRegistroC100(RegC001.RegistroC100.Items[intFor], booAborta);

      if not booAborta then
      begin
        with RegC001.RegistroC100.Items[intFor] do
        begin
          Check(not((COD_MOD = '55') and (COD_SIT <> sdDoctoNumInutilizada) and (Trim(CHV_NFE) = '')),
                                                               '(C-C100) Nota: %s' +sLineBreak+
                                                               '         Série: %s'+sLineBreak+
                                                               '         Emitida no dia: %s'+sLineBreak+
                                                               '         Modelo: %s'        +sLineBreak+
                                                               '         ChaveNF: %s'+sLineBreak, [NUM_DOC, SER, FormatDateTime('dd/mm/yyyy', DT_DOC), COD_MOD, CHV_NFE]);

          case COD_SIT of
            sdRegular:               strCOD_SIT := '00';
            sdExtempRegular:         strCOD_SIT := '01';
            sdCancelado:             strCOD_SIT := '02';
            sdCanceladoExtemp:       strCOD_SIT := '03';
            sdDoctoDenegado:         strCOD_SIT := '04';
            sdDoctoNumInutilizada:   strCOD_SIT := '05';
            sdFiscalCompl:           strCOD_SIT := '06';
            sdExtempCompl:           strCOD_SIT := '07';
            sdRegimeEspecNEsp:       strCOD_SIT := '08';
          end;

          /// Tratamento NFs canceladas 02/03, denegada 04 ou inutilizada 05
          /// Invertido a posição do teste condicional pois o ACBr por padrão adiciona IND_FRT=2
          if Pos(strCOD_SIT,'02, 03, 04, 05') > 0 then
          begin
            DT_DOC   := 0;
            DT_E_S   := 0;
            IND_FRT  := tfNenhum;
            IND_PGTO := tpNenhum;
            booNFCancelada := true
          end
          else
            booNFCancelada := false;

          if DT_INI < EncodeDate(2012,01,01) then
          begin
            case IND_FRT of
              tfPorContaTerceiros:                                   strIND_FRT := '0';
              tfPorContaEmitente, tfProprioPorContaRemetente:        strIND_FRT := '1';
              tfPorContaDestinatario, tfProprioPorContaDestinatario: strIND_FRT := '2';
              tfSemCobrancaFrete:                                    strIND_FRT := '9';
              tfNenhum:                                              strIND_FRT := '';
            end
          end
          else if DT_INI < EncodeDate(2018,01,01) then
          begin
            case IND_FRT of
              tfPorContaEmitente, tfProprioPorContaRemetente:        strIND_FRT := '0';
              tfPorContaDestinatario, tfProprioPorContaDestinatario: strIND_FRT := '1';
              tfPorContaTerceiros:                                   strIND_FRT := '2';
              tfSemCobrancaFrete:                                    strIND_FRT := '9';
              tfNenhum:                                              strIND_FRT := '';
            end;
          end
          else
          begin
            case IND_FRT of
              tfPorContaEmitente:            strIND_FRT := '0';
              tfPorContaDestinatario:        strIND_FRT := '1';
              tfPorContaTerceiros:           strIND_FRT := '2';
              tfProprioPorContaRemetente:    strIND_FRT := '3';
              tfProprioPorContaDestinatario: strIND_FRT := '4';
              tfSemCobrancaFrete:            strIND_FRT := '9';
              tfNenhum:                      strIND_FRT := '';
            end;
          end;

          if DT_INI >= EncodeDate(2012,07,01) then
          begin
            case IND_PGTO of
              tpVista:        strIND_PGTO := '0';
              tpPrazo:        strIND_PGTO := '1';
              tpOutros:       strIND_PGTO := '2';
              tpNenhum:       strIND_PGTO := '';
            end
          end
          else
          begin
            case IND_PGTO of
              tpVista:        strIND_PGTO := '0';
              tpPrazo:        strIND_PGTO := '1';
              tpSemPagamento: strIND_PGTO := '9';
              tpNenhum:       strIND_PGTO := '';
            end;
          end;

          /// Para nota fiscal de consumidor final (COD_MOD = '65') não devem ser
          /// informafo os campo:
          /// COD_PAR, VL_BC_ICMS_ST, VL_ICMS_ST, VL_IPI, VL_PIS, VL_COFINS, VL_PIS_ST, VL_COFINS_ST.
          if COD_MOD = '65' then
          begin
            Add( LFill('C100') +
                 LFill( Integer(IND_OPER), 0 ) +
                 LFill( Integer(IND_EMIT), 0 ) +
                 LFill( COD_PART ) +
                 LFill( COD_MOD  ) +
                 LFill( strCOD_SIT  ) +
                 LFill( SER  ) +
                 LFill( NUM_DOC  ) +
                 LFill( CHV_NFE  ) +
                 LFill( DT_DOC, 'ddmmyyyy' ) +
                 LFill( DT_E_S, 'ddmmyyyy' ) +
                 LFill( VL_DOC , 0 , 2 , booNFCancelada ) +
                 LFill( strIND_PGTO ) +
                 LFill( VL_DESC,0,2, booNFCancelada ) +
                 LFill( VL_ABAT_NT,0,2, booNFCancelada ) +
                 LFill( VL_MERC,0,2, booNFCancelada ) +
                 LFill( strIND_FRT ) +
                 LFill( VL_FRT,0,2, booNFCancelada ) +
                 LFill( VL_SEG,0,2, booNFCancelada ) +
                 LFill( VL_OUT_DA,0,2, booNFCancelada ) +
                 LFill( VL_BC_ICMS,0,2, booNFCancelada ) +
                 LFill( VL_ICMS,0,2, booNFCancelada ) +
                 LFill( VL_BC_ICMS_ST,0,2, true ) +
                 LFill( VL_ICMS_ST,0,2, true ) +
                 LFill( VL_IPI,0,2, true ) +
                 LFill( VL_PIS,0,2, true ) +
                 LFill( VL_COFINS,0,2, true ) +
                 LFill( VL_PIS_ST,0,2, true ) +
                 LFill( VL_COFINS_ST,0,2, true ) ) ;
          end
          else
          begin
            Add( LFill('C100') +
                 LFill( Integer(IND_OPER), 0 ) +
                 LFill( Integer(IND_EMIT), 0 ) +
                 LFill( COD_PART ) +
                 LFill( COD_MOD  ) +
                 LFill( strCOD_SIT  ) +
                 LFill( SER  ) +
                 LFill( NUM_DOC  ) +
                 LFill( CHV_NFE  ) +
                 LFill( DT_DOC, 'ddmmyyyy' ) +
                 LFill( DT_E_S, 'ddmmyyyy' ) +
                 LFill( VL_DOC , 0 , 2 , booNFCancelada ) +
                 LFill( strIND_PGTO ) +
                 LFill( VL_DESC,0,2, booNFCancelada ) +
                 LFill( VL_ABAT_NT,0,2, booNFCancelada ) +
                 LFill( VL_MERC,0,2, booNFCancelada ) +
                 LFill( strIND_FRT ) +
                 LFill( VL_FRT,0,2, booNFCancelada ) +
                 LFill( VL_SEG,0,2, booNFCancelada ) +
                 LFill( VL_OUT_DA,0,2, booNFCancelada ) +
                 LFill( VL_BC_ICMS,0,2, booNFCancelada ) +
                 LFill( VL_ICMS,0,2, booNFCancelada ) +
                 LFill( VL_BC_ICMS_ST,0,2, booNFCancelada ) +
                 LFill( VL_ICMS_ST,0,2, booNFCancelada ) +
                 LFill( VL_IPI,0,2, booNFCancelada ) +
                 LFill( VL_PIS,0,2,True ) +
                 LFill( VL_COFINS,0,2,True) +
                 LFill( VL_PIS_ST,0,2,True ) +
                 LFill( VL_COFINS_ST,0,2, True) ) ;
          end;
        end;
        /// Registros FILHOS

        WriteRegistroC101( RegC001.RegistroC100.Items[intFor] ) ; // EC 87/2015 - Marcio Meneguzzi 06jan2016
        WriteRegistroC105( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC110( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC120( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC130( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC140( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC160( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC165( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC170( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC185( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC186( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC190( RegC001.RegistroC100.Items[intFor] ) ;
        WriteRegistroC195( RegC001.RegistroC100.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
      end;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC100Count := FRegistroC100Count + RegC001.RegistroC100.Count;

    RegC001.RegistroC100.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC101(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC101 ) then
  begin
     for intFor := 0 to RegC100.RegistroC101.Count - 1 do
     begin
       with RegC100.RegistroC101.Items[intFor] do
       begin
         ///
         Add( LFill('C101') +
              LFill( VL_FCP_UF_DEST,0,2 ) +
              LFill( VL_ICMS_UF_DEST,0,2 ) +
              LFill( VL_ICMS_UF_REM,0,2 ) );
       end;
       RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variável para armazenar a quantidade de registro do tipo.
     FRegistroC101Count := FRegistroC101Count + RegC100.RegistroC101.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC105(RegC100: TRegistroC100);
var
  intFor: integer;
  strOPER: String;
  vRegC105: TRegistroC105;
begin
  if Assigned( RegC100.RegistroC105 ) then
  begin
    for intFor := 0 to RegC100.RegistroC105.Count - 1 do
    begin
      vRegC105 := RegC100.RegistroC105.Items[intFor];

      Check(funChecaUF(vRegC105.UF), '(C-C105) UF DESTINO: A UF "%s" informada é inválida!', [vRegC105.UF]);

      strOPER := IndTipoOperacaoStToStr(vRegC105.OPER);
      ///
      Add( LFill('C105') +
           LFill( strOPER ) +
           LFill( vRegC105.UF ) ) ;
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;
    /// Variável para armazenar a quantidade de registro do tipo.
    FRegistroC105Count := FRegistroC105Count + RegC100.RegistroC105.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC110(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC110 ) then
  begin
     for intFor := 0 to RegC100.RegistroC110.Count - 1 do
     begin
        with RegC100.RegistroC110.Items[intFor] do
        begin
          Add( LFill('C110') +
               LFill( COD_INF ) +
               LFill( TXT_COMPL) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC111( RegC100.RegistroC110.Items[intFor] ) ;
        WriteRegistroC112( RegC100.RegistroC110.Items[intFor] ) ;
        WriteRegistroC113( RegC100.RegistroC110.Items[intFor] ) ;
        WriteRegistroC114( RegC100.RegistroC110.Items[intFor] ) ;
        WriteRegistroC115( RegC100.RegistroC110.Items[intFor] ) ;
        WriteRegistroC116( RegC100.RegistroC110.Items[intFor] ) ;        

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC110Count := FRegistroC110Count + RegC100.RegistroC110.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC111(RegC110: TRegistroC110);
var
  intFor: integer;
  intIND_PROC: integer;
  strLinha: String;
begin
  if Assigned( RegC110.RegistroC111 ) then
  begin
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC111) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC111(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;

     for intFor := 0 to RegC110.RegistroC111.Count - 1 do
     begin
        strLinha := '';
        with RegC110.RegistroC111.Items[intFor] do
        begin
          case IND_PROC of
           opSefaz:           intIND_PROC := 0;
           opJusticaFederal:  intIND_PROC := 1;
           opJusticaEstadual: intIND_PROC := 2;
           opSecexRFB:        intIND_PROC := 9;
           opOutros:          intIND_PROC := 9;
           else               intIND_PROC := 9;
          end;

          strLinha := LFill('C111') +
                      LFill( NUM_PROC ) +
                      LFill( intIND_PROC, 0 );
          //-- Write
          if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC111) then
             TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC111(strLinha);

          Add(strLinha);
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC111) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC111(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;

     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC111Count := FRegistroC111Count + RegC110.RegistroC111.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC112(RegC110: TRegistroC110) ;
var
  intFor: integer;
begin
  if Assigned( RegC110.RegistroC112 ) then
  begin
     for intFor := 0 to RegC110.RegistroC112.Count - 1 do
     begin
        with RegC110.RegistroC112.Items[intFor] do
        begin
          Add( LFill('C112') +
               LFill( Integer(COD_DA), 0 ) +
               LFill( UF ) +
               LFill( NUM_DA ) +
               LFill( COD_AUT ) +
               LFill( VL_DA,0,2 ) +
               LFill( DT_VCTO ) +
               LFill( DT_PGTO ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC112Count := FRegistroC112Count + RegC110.RegistroC112.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC113(RegC110: TRegistroC110);
var
  intFor: integer;
begin
  if Assigned( RegC110.RegistroC113 ) then
  begin
     for intFor := 0 to RegC110.RegistroC113.Count - 1 do
     begin
        with RegC110.RegistroC113.Items[intFor] do
        begin
          Add( LFill('C113') +
               LFill( Integer(IND_OPER), 0 ) +
               LFill( Integer(IND_EMIT), 0 ) +
               LFill( COD_PART ) +
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               IfThen( FBloco_0.Registro0000.COD_VER >= vlVersao110, LFill( CHV_DOCe ), EmptyStr));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC113Count := FRegistroC113Count + RegC110.RegistroC113.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC114(RegC110: TRegistroC110) ;
var
  intFor: integer;
begin
  if Assigned( RegC110.RegistroC114 ) then
  begin
     for intFor := 0 to RegC110.RegistroC114.Count - 1 do
     begin
        with RegC110.RegistroC114.Items[intFor] do
        begin
          Add( LFill('C114') +
               LFill( COD_MOD ) +
               LFill( ECF_FAB ) +
               LFill( ECF_CX ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC114Count := FRegistroC114Count + RegC110.RegistroC114.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC115(RegC110: TRegistroC110);
var
  intFor: integer;
  intIND_CARGA: integer;
begin
  if Assigned( RegC110.RegistroC115 ) then
  begin
     for intFor := 0 to RegC110.RegistroC115.Count - 1 do
     begin
        with RegC110.RegistroC115.Items[intFor] do
        begin
          case IND_CARGA of
           ttRodoviario:      intIND_CARGA := 0;
           ttFerroviario:     intIND_CARGA := 1;
           ttRodoFerroviario: intIND_CARGA := 2;
           ttAquaviario:      intIND_CARGA := 3;
           ttDutoviario:      intIND_CARGA := 4;
           ttAereo:           intIND_CARGA := 5;
           ttOutros:          intIND_CARGA := 9;
           else               intIND_CARGA := 9;
          end;

          Add( LFill('C115') +
               LFill( intIND_CARGA, 0 ) +
               LFill( CNPJ_COL ) +
               LFill( IE_COL ) +
               LFill( CPF_COL ) +
               LFill( COD_MUN_COL ) +
               LFill( CNPJ_ENTG ) +
               LFill( IE_ENTG ) +
               LFill( CPF_ENTG ) +
               LFill( COD_MUN_ENTG ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC115Count := FRegistroC115Count + RegC110.RegistroC115.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC116(RegC110: TRegistroC110);
var
  intFor: integer;
begin
  if Assigned( RegC110.RegistroC116 ) then
  begin
     for intFor := 0 to RegC110.RegistroC116.Count - 1 do
     begin
        with RegC110.RegistroC116.Items[intFor] do
        begin
          Add( LFill('C116') +
               LFill( COD_MOD ) +
               LFill( NR_SAT ) +
               LFill( CHV_CFE ) +
               LFill( NUM_CFE ) +
               LFill( DT_DOC ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC116Count := FRegistroC116Count + RegC110.RegistroC116.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC120(RegC100: TRegistroC100);
var
  intFor: integer;
  strLinha: String;
begin
  if Assigned( RegC100.RegistroC120 ) then
  begin
     if RegC100.RegistroC120.Count > 0 then
     begin
        if (RegC100.IND_OPER in [tpSaidaPrestacao]) then
           Check(False, 'O RegistroC120, não deve ser gerado em movimentações de saída, no %s e no %s, conforme ATO COTEPE 09/08', ['PerfilA','PerfilB']);
     end;
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC120) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC120(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to RegC100.RegistroC120.Count - 1 do
     begin
        with RegC100.RegistroC120.Items[intFor] do
        begin
          strLinha := LFill('C120') +
                      LFill( Integer(COD_DOC_IMP), 0 ) +
                      LFill( NUM_DOC__IMP ) +
                      LFill( PIS_IMP,0,2 ) +
                      LFill( COFINS_IMP,0,2 ) +
                      LFill( NUM_ACDRAW );
          //-- Write
          if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC120) then
             TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC120(strLinha);

          Add(strLinha);
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC120) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC120(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC120Count := FRegistroC120Count + RegC100.RegistroC120.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC130(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC130 ) then
  begin
     if RegC100.RegistroC130.Count > 0 then
     begin
        if (RegC100.IND_OPER in [tpEntradaAquisicao]) then
           Check(False, 'O RegistroC130, não deve ser gerado em movimentações de entrada, no %s e no %s, conforme ATO COTEPE 09/08', ['PerfilA','PerfilB']);
     end;
     for intFor := 0 to RegC100.RegistroC130.Count - 1 do
     begin
        with RegC100.RegistroC130.Items[intFor] do
        begin
          Add( LFill('C130') +
               LFill( VL_SERV_NT,2,0 ) +
               LFill( VL_BC_ISSQN,2,0 ) +
               LFill( VL_ISSQN,2,0 ) +
               LFill( VL_BC_IRRF,2,0 ) +
               LFill( VL_IRRF,2,0 ) +
               LFill( VL_BC_PREV,2,0 ) +
               LFill( VL_PREV,2,0 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC130Count := FRegistroC130Count + RegC100.RegistroC130.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC140(RegC100: TRegistroC100);
var
  intFor: integer;
  strIND_TIT: String;
begin
  if Assigned( RegC100.RegistroC140 ) then
  begin
     for intFor := 0 to RegC100.RegistroC140.Count - 1 do
     begin
        with RegC100.RegistroC140.Items[intFor] do
        begin
          case IND_TIT of
           tcDuplicata:   strIND_TIT := '00';
           tcCheque:      strIND_TIT := '01';
           tcPromissoria: strIND_TIT := '02';
           tcRecibo:      strIND_TIT := '03';
           tcOutros:      strIND_TIT := '99';
          end;

          Add( LFill('C140') +
               LFill( Integer(IND_EMIT), 0 ) +
               LFill( strIND_TIT  ) +
               LFill( DESC_TIT ) +
               LFill( NUM_TIT ) +
               LFill( QTD_PARC,2 ) +
               LFill( VL_TIT,0,2 ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC141( RegC100.RegistroC140.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC140Count := FRegistroC140Count + RegC100.RegistroC140.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC141(RegC140:TRegistroC140) ;
var
  intFor: integer;
begin
  if Assigned( RegC140.RegistroC141 ) then
  begin
     for intFor := 0 to RegC140.RegistroC141.Count - 1 do
     begin
        with RegC140.RegistroC141.Items[intFor] do
        begin
          Add( LFill('C141') +
               LFill( NUM_PARC ) +
               LFill( DT_VCTO ) +
               LFill( VL_PARC,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC141Count := FRegistroC141Count + RegC140.RegistroC141.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC160(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC160 ) then
  begin
     if RegC100.RegistroC160.Count > 0 then
     begin
        if (RegC100.IND_OPER in [tpEntradaAquisicao]) then
           Check(False, 'O RegistroC160, não deve ser gerado em movimentações de entrada, no %s e no %s, conforme ATO COTEPE 09/08', ['PerfilA','PerfilB']);
     end;
     for intFor := 0 to RegC100.RegistroC160.Count - 1 do
     begin
        with RegC100.RegistroC160.Items[intFor] do
        begin
          Add( LFill('C160') +
               LFill( COD_PART ) +
               LFill( VEIC_ID ) +
               LFill( QTD_VOL,0 ) +
               LFill( PESO_BRT,0,2 ) +
               LFill( PESO_LIQ,0,2 ) +
               LFill( UF_ID ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC160Count := FRegistroC160Count + RegC100.RegistroC160.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC165(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC165 ) then
  begin
     if RegC100.RegistroC165.Count > 0 then
     begin
        if (RegC100.IND_OPER in [tpEntradaAquisicao]) then
           Check(False, 'O RegistroC165, não deve ser gerado em movimentações de entrada, no %s e no %s, conforme ATO COTEPE 09/08', ['PerfilA','PerfilB']);
     end;
     for intFor := 0 to RegC100.RegistroC165.Count - 1 do
     begin
        with RegC100.RegistroC165.Items[intFor] do
        begin
          Add( LFill('C165') +
               LFill( COD_PART ) +
               LFill( VEIC_ID ) +
               LFill( COD_AUT ) +
               LFill( NR_PASSE ) +
               LFill( HORA ) +
               LFill( TEMPER ) +
               LFill( QTD_VOL,0 ) +
               LFill( PESO_BRT,0,2 ) +
               LFill( PESO_LIQ,0,2 ) +
               LFill( NOM_MOT ) +
               LFill( CPF ) +
               LFill( UF_ID ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC165Count := FRegistroC165Count + RegC100.RegistroC165.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC170(RegC100: TRegistroC100);
var
  intFor: integer;
  strIND_APUR : String;
  intDecimaisPercent: integer;
  strMascaraParcent : string;
  strLinha: String;
begin
  if Assigned( RegC100.RegistroC170 ) then
  begin
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC170) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC170(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     if (DT_INI >= EncodeDate(2012,01,01)) then
     begin
       intDecimaisPercent := 4;
       strMascaraParcent := '#0.0000';
     end
     else
     begin
       intDecimaisPercent := 2;
       strMascaraParcent := '#0.00';
     end;

     for intFor := 0 to RegC100.RegistroC170.Count - 1 do
     begin
        with RegC100.RegistroC170.Items[intFor] do
        begin
          case IND_APUR of
            iaMensal    : strIND_APUR := '0';
            iaDecendial : strIND_APUR := '1';
            iaNenhum    : strIND_APUR := ' ';
          end;

          strLinha :=  LFill('C170') +
                       LFill( NUM_ITEM ) +
                       LFill( COD_ITEM ) +
                       LFill( DESCR_COMPL ) +
                       LFILL( QTD, 0, 4, False, '0', '#0.00000') +
                       LFill( UNID ) +
                       LFill( VL_ITEM, 0, 2 ) +
                       LFill( VL_DESC, 0, 2 ) +
                       LFill( Integer(IND_MOV), 0 ) +
                       LFill( CST_ICMS, 3 ) +
                       LFill( CFOP, 4 ) +
                       LFill( COD_NAT ) +
                       LFill( VL_BC_ICMS, 0, 2 ) +
                       LFill( ALIQ_ICMS,  0, 2 ) +
                       LFill( VL_ICMS, 0, 2 ) +
                       LFill( VL_BC_ICMS_ST, 0, 2 ) +
                       LFill( ALIQ_ST, 0, 2 ) +
                       LFill( VL_ICMS_ST, 0, 2 ) +
                       LFill( strIND_APUR ) +
                       LFill( CST_IPI, 2, True ) +
                       LFill( COD_ENQ ) +
                       LFill( VL_BC_IPI, 0, 2 ) +
                       LFill( ALIQ_IPI,  0, 2 ) +
                       LFill( VL_IPI, 0, 2 ) +
                       LFill( CST_PIS, 2, True ) +
                       LFill( VL_BC_PIS, 0, 2,True ) +
                       LFill( ALIQ_PIS_PERC, 0, intDecimaisPercent, True, '0', strMascaraParcent ) +
                       LFill( QUANT_BC_PIS,  0, 3, True, '0', '#0.000' ) +
                       LFill( ALIQ_PIS_R, 0, 4, True, '0', '#0.0000' ) +
                       LFill( VL_PIS, 0, 2,True ) +
                       LFill( CST_COFINS, 2, True ) +
                       LFill( VL_BC_COFINS, 0, 2,True ) +
                       LFill( ALIQ_COFINS_PERC, 0, intDecimaisPercent, True, '0', strMascaraParcent ) +
                       LFill( QUANT_BC_COFINS,  0, 3, True, '0', '#0.000' ) +
                       LFill( ALIQ_COFINS_R, 0, 4, True, '0', '#0.0000' ) +
                       LFill( VL_COFINS, 0, 2,True ) +
                       LFill( COD_CTA )+
                       ifthen( FBloco_0.Registro0000.COD_VER > vlVersao111 ,LFill( VL_ABAT_NT, 0, 2,True ),'');
          //-- Write
          if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC170) then
             TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC170(strLinha);

          Add(strLinha);
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC171( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC172( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC173( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC174( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC175( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC176( RegC100.RegistroC170.Items[intFor] ) ;
		    if FBloco_0.Registro0000.UF = 'PE' then
    		  WriteRegistroC177( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC178( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC179( RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC180( RegC100, RegC100.RegistroC170.Items[intFor] ) ;
        WriteRegistroC181( RegC100, RegC100.RegistroC170.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC170) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC170(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC170Count := FRegistroC170Count + RegC100.RegistroC170.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC171(RegC170: TRegistroC170) ;
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC171 ) then
  begin
     for intFor := 0 to RegC170.RegistroC171.Count - 1 do
     begin
        with RegC170.RegistroC171.Items[intFor] do
        begin
          Add( LFill('C171') +
               LFill( NUM_TANQUE,3 ) +
               DFill( QTDE, 3 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC171Count := FRegistroC171Count + RegC170.RegistroC171.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC172(RegC170: TRegistroC170) ;
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC172 ) then
  begin
     for intFor := 0 to RegC170.RegistroC172.Count - 1 do
     begin
        with RegC170.RegistroC172.Items[intFor] do
        begin
          Add( LFill('C172') +
               LFill( VL_BC_ISSQN,0,2 ) +
               LFill( ALIQ_ISSQN,0,2 ) +
               LFill( VL_ISSQN,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC172Count := FRegistroC172Count + RegC170.RegistroC172.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC173(RegC170: TRegistroC170) ;
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC173 ) then
  begin
     for intFor := 0 to RegC170.RegistroC173.Count - 1 do
     begin
        with RegC170.RegistroC173.Items[intFor] do
        begin
          Add( LFill('C173') +
               LFill( LOTE_MED ) +
               DFill( QTD_ITEM, 3 ) +
               LFill( DT_FAB ) +
               LFill( DT_VAL ) +
               LFill( Integer(IND_MED), 0 ) +
               LFill( Integer(TP_PROD), 0 ) +
               LFill( VL_TAB_MAX,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC173Count := FRegistroC173Count + RegC170.RegistroC173.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC174(RegC170: TRegistroC170);
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC174 ) then
  begin
     for intFor := 0 to RegC170.RegistroC174.Count - 1 do
     begin
        with RegC170.RegistroC174.Items[intFor] do
        begin
          Add( LFill('C174') +
               LFill( Integer(IND_ARM), 0 ) +
               LFill( NUM_ARM ) +
               LFill( DESCR_COMPL ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC174Count := FRegistroC174Count + RegC170.RegistroC174.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC175(RegC170: TRegistroC170);
var
  intFor: integer;
  intIND_VEIC_OPER: integer;
begin
  if Assigned( RegC170.RegistroC175 ) then
  begin
     for intFor := 0 to RegC170.RegistroC175.Count - 1 do
     begin
        with RegC170.RegistroC175.Items[intFor] do
        begin
          case IND_VEIC_OPER of
           tovVendaPConcess:  intIND_VEIC_OPER := 0;
           tovFaturaDireta:   intIND_VEIC_OPER := 1;
           tovVendaDireta:    intIND_VEIC_OPER := 2;
           tovVendaDConcess:  intIND_VEIC_OPER := 3;
           tovVendaOutros:    intIND_VEIC_OPER := 9;
           else               intIND_VEIC_OPER := 9;
          end;

          Add( LFill('C175') +
               LFill( intIND_VEIC_OPER, 0 ) +
               LFill( CNPJ ) +
               LFill( UF ) +
               LFill( CHASSI_VEIC ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC175Count := FRegistroC175Count + RegC170.RegistroC175.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC176(RegC170: TRegistroC170);
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC176 ) then
  begin
     for intFor := 0 to RegC170.RegistroC176.Count - 1 do
     begin
        with RegC170.RegistroC176.Items[intFor] do
        begin
          Add( LFill('C176') +
               LFill( COD_MOD_ULT_E ) +
               LFill( NUM_DOC_ULT_E ) +
               LFill( SER_ULT_E ) +
               LFill( DT_ULT_E ) +
               LFill( COD_PART_ULT_E ) +
               DFill( QUANT_ULT_E,3 ) +
               DFill( VL_UNIT_ULT_E,3 ) +
               DFill( VL_UNIT_BC_ST,3 ) +
               ifthen(DT_INI >= EncodeDate(2017,01,01),
               LFill( CHAVE_NFE_ULT_E ) +
               LFill( NUM_ITEM_ULT_E ) +
               DFill( VL_UNIT_BC_ICMS_ULT_E,2, True ) +
               DFill( ALIQ_ICMS_ULT_E,2, True ) +
               DFill( VL_UNIT_LIMITE_BC_ICMS_ULT_E,2, True ) +
               DFill( VL_UNIT_ICMS_ULT_E,3, True ) +
               DFill( ALIQ_ST_ULT_E,2, True ) +
               DFill( VL_UNIT_RES,3, True ) +
               LFill( COD_RESP_RET ) +
               LFill( MotivoRessarcimentoToStr(COD_MOT_RES) ) +
               LFill( CHAVE_NFE_RET ) +
               LFill( COD_PART_NFE_RET ) +
               LFill( SER_NFE_RET ) +
               LFill( NUM_NFE_RET ) +
               LFill( ITEM_NFE_RET ) +
               LFill( COD_DA ) +
               LFill( NUM_DA )+
               ifthen(FBloco_0.Registro0000.COD_VER > vlVersao111, LFill( VL_UNIT_RES_FCP_ST,0,3 ),''), '')) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC176Count := FRegistroC176Count + RegC170.RegistroC176.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC177(RegC170: TRegistroC170);
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC177 ) then
  begin
     for intFor := 0 to RegC170.RegistroC177.Count - 1 do
     begin
        with RegC170.RegistroC177.Items[intFor] do
        begin
          if FBloco_0.Registro0000.COD_VER < vlVersao112 then
            Add( LFill('C177') +
                 LFill( COD_SELO_IPI ) +
                 LFill( QT_SELO_IPI,0,0 ) )
          else
            Add( LFill('C177') +
                 LFill( COD_INF_ITEM ) )
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC177Count := FRegistroC177Count + RegC170.RegistroC177.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC178(RegC170: TRegistroC170) ;
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC178 ) then
  begin
     for intFor := 0 to RegC170.RegistroC178.Count - 1 do
     begin
        with RegC170.RegistroC178.Items[intFor] do
        begin
          Add( LFill('C178') +
               LFill( CL_ENQ ) +
               LFill( VL_UNID,0,2 ) +
               DFill( QUANT_PAD, 3 ) );
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC178Count := FRegistroC178Count + RegC170.RegistroC178.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC179(RegC170: TRegistroC170);
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC179 ) then
  begin
     for intFor := 0 to RegC170.RegistroC179.Count - 1 do
     begin
        with RegC170.RegistroC179.Items[intFor] do
        begin
          Add( LFill('C179') +
               LFill( BC_ST_ORIG_DEST,0,2 ) +
               LFill( ICMS_ST_REP,0,2 ) +
               LFill( ICMS_ST_COMPL,0,2 ) +
               LFill( BC_RET,0,2 ) +
               LFill( ICMS_RET,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC179Count := FRegistroC179Count + RegC170.RegistroC179.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC180(RegC100: TRegistroC100; RegC170: TRegistroC170);
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC180 ) then
  begin
     if RegC170.RegistroC180.Count > 0 then
     begin
        if (RegC100.IND_OPER = tpSaidaPrestacao) or (FBloco_0.Registro0000.IND_PERFIL in [pfPerfilC]) then
           Check(False, 'O RegistroC180, não deve ser gerado em movimentações de saída ou %s, conforme ATO COTEPE 09/08', ['PerfilC']);
     end;
     for intFor := 0 to RegC170.RegistroC180.Count - 1 do
     begin
        with RegC170.RegistroC180.Items[intFor] do
        begin
          Add( LFill('C180') +
               LFill( COD_RESP_RET ) +
               DFill( QUANT_CONV,6 ) +
               LFill( UNID) +
               DFill( VL_UNIT_CONV,6 ) +
               DFill( VL_UNIT_ICMS_OP_CONV,6 ) +
               DFill( VL_UNIT_BC_ICMS_ST_CONV,6 ) +
               DFill( VL_UNIT_ICMS_ST_CONV,6 ) +
               DFill( VL_UNIT_FCP_ST_CONV,6 ) +
               LFill( COD_DA ) +
               LFill( NUM_DA ));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC180Count := FRegistroC180Count + RegC170.RegistroC180.Count;

  end;
end;

procedure TBloco_C.WriteRegistroC181(RegC100: TRegistroC100; RegC170: TRegistroC170);
var
  intFor: integer;
begin
  if Assigned( RegC170.RegistroC181 ) then
  begin
     if RegC170.RegistroC181.Count > 0 then
     begin
        if (RegC100.IND_OPER = tpSaidaPrestacao) or (FBloco_0.Registro0000.IND_PERFIL in [pfPerfilC]) then
           Check(False, 'O RegistroC181, não deve ser gerado em movimentações de saída ou %s, conforme ATO COTEPE 09/08', ['PerfilC']);
     end;
     for intFor := 0 to RegC170.RegistroC181.Count - 1 do
     begin
        with RegC170.RegistroC181.Items[intFor] do
        begin
          Add( LFill('C181') +
               LFill( COD_MOT_REST_COMPL ) +
               LFill( QUANT_CONV,0,6 ) +
               LFill( UNID) +
               LFill( COD_MOD_SAIDA) +
               LFill( SERIE_SAIDA) +
               LFill( ECF_FAB_SAIDA) +
               LFill( NUM_DOC_SAIDA) +
               LFill( CHV_DFE_SAIDA) +
               LFill( DT_DOC_SAIDA, 'ddmmyyyy' ) +
               LFill( NUM_ITEM_SAIDA) +
               DFill( VL_UNIT_CONV_SAIDA,6,True ) +
               VDFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV_SAIDA, 6) +
               VDFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV_SAIDA, 6) +
               VDFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV_SAIDA, 6) +
               VDFill( VL_UNIT_ICMS_NA_OPERACAO_CONV_SAIDA, 6) +
               VDFill( VL_UNIT_ICMS_OP_CONV_SAIDA, 6) +
               VDFill( VL_UNIT_ICMS_ST_CONV_REST, 6) +
               VDFill( VL_UNIT_FCP_ST_CONV_REST, 6) +
               VDFill( VL_UNIT_ICMS_ST_CONV_COMPL, 6) +
               VDFill( VL_UNIT_FCP_ST_CONV_COMPL, 6));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC181Count := FRegistroC181Count + RegC170.RegistroC181.Count;

  end;
end;


procedure TBloco_C.WriteRegistroC185(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC185 ) then
  begin
     for intFor := 0 to RegC100.RegistroC185.Count - 1 do
     begin
        with RegC100.RegistroC185.Items[intFor] do
        begin
          Add( LFill('C185') +
               LFill( NUM_ITEM )+
               LFill( COD_ITEM )+
               LFill( CST_ICMS )+
               LFill( CFOP )+
               LFill( COD_MOT_REST_COMPL) +
               DFill( QUANT_CONV, 6, False ) +
               LFill( UNID ) +
               DFill( VL_UNIT_CONV,6, False ) +
               VDFill(VL_UNIT_ICMS_NA_OPERACAO_CONV, 6) +
               VDFill(VL_UNIT_ICMS_OP_CONV,6) +
               VDFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV, 6) +
               VDFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV, 6) +
               VDFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV, 6) +
               VDFill(VL_UNIT_ICMS_ST_CONV_REST, 6) +
               VDFill(VL_UNIT_FCP_ST_CONV_REST, 6) +
               VDFill(VL_UNIT_ICMS_ST_CONV_COMPL, 6) +
               VDFill(VL_UNIT_FCP_ST_CONV_COMPL, 6));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC185Count := FRegistroC185Count + RegC100.RegistroC185.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC186(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC186 ) then
  begin
     for intFor := 0 to RegC100.RegistroC186.Count - 1 do
     begin
        with RegC100.RegistroC186.Items[intFor] do
        begin
          Add( LFill('C186') +
               LFill( NUM_ITEM )+
               LFill( COD_ITEM )+
               LFill( CST_ICMS )+
               LFill( CFOP )+
               LFill( COD_MOT_REST_COMPL) +
               DFill( QUANT_CONV, 6 ) +
               LFill( UNID ) +
               LFill( COD_MOD_ENTRADA ) +
               LFill( SERIE_ENTRADA ) +
               LFill( NUM_DOC_ENTRADA ) +
               LFill( CHV_DFE_ENTRADA ) +
               LFill( DT_DOC_ENTRADA, 'ddmmyyyy' ) +
               LFill( NUM_ITEM_ENTRADA ) +
               DFill( VL_UNIT_CONV_ENTRADA,6 ) +
               DFill( VL_UNIT_ICMS_OP_CONV_ENTRADA,6 ) +
               DFill( VL_UNIT_BC_ICMS_ST_CONV_ENTRADA,6 ) +
               DFill( VL_UNIT_ICMS_ST_CONV_ENTRADA,6 ) +
               DFill( VL_UNIT_FCP_ST_CONV_ENTRADA,6 ));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC186Count := FRegistroC186Count + RegC100.RegistroC186.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC190(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC190 ) then
  begin
     for intFor := 0 to RegC100.RegistroC190.Count - 1 do
     begin
        with RegC100.RegistroC190.Items[intFor] do
        begin
          Add( LFill('C190') +
               LFill( CST_ICMS, 3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( VL_IPI,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
        if FBloco_0.Registro0000.COD_VER > vlVersao111 then
          WriteRegistroC191( RegC100.RegistroC190.Items[intFor]) ;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC190Count := FRegistroC190Count + RegC100.RegistroC190.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC191(RegC190: TRegistroC190);
var
  intFor: integer;
begin
  if Assigned( RegC190.RegistroC191 ) then
  begin
     for intFor := 0 to RegC190.RegistroC191.Count - 1 do
     begin
        with RegC190.RegistroC191.Items[intFor] do
        begin
          Add( LFill('C191') +
               LFill( VL_FCP_OP, 0,2 ) +
               LFill( VL_FCP_ST, 0,2 ) +
               LFill( VL_FCP_RET,0,2 )) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC191Count := FRegistroC191Count + RegC190.RegistroC191.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC195(RegC100: TRegistroC100);
var
  intFor: integer;
begin
  if Assigned( RegC100.RegistroC195 ) then
  begin
     for intFor := 0 to RegC100.RegistroC195.Count - 1 do
     begin
        with RegC100.RegistroC195.Items[intFor] do
        begin
          Add( LFill('C195') +
               LFill( COD_OBS ) +
               LFill( TXT_COMPL ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC197( RegC100.RegistroC195.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC195Count := FRegistroC195Count + RegC100.RegistroC195.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC197(RegC195: TRegistroC195);
var
  intFor: integer;
begin
  if Assigned(RegC195.RegistroC197 ) then
  begin
     for intFor := 0 to RegC195.RegistroC197.Count - 1 do
     begin
        with RegC195.RegistroC197.Items[intFor] do
        begin
          Add( LFill('C197') +
               LFill( COD_AJ ) +
               LFill( DESCR_COMPL_AJ ) +
               LFill( COD_ITEM ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_OUTROS,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC197Count := FRegistroC197Count + RegC195.RegistroC197.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC300(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned( RegC001.RegistroC300 ) then
  begin
     if RegC001.RegistroC300.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC300, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC001.RegistroC300.Count - 1 do
     begin
        with RegC001.RegistroC300.Items[intFor] do
        begin
          Add( LFill('C300') +
               LFill( COD_MOD,2 ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( NUM_DOC_INI ) +
               LFill( NUM_DOC_FIN ) +
               LFill( DT_DOC ) +
               LFill( VL_DOC,0,2 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( COD_CTA ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC310( RegC001.RegistroC300.Items[intFor] ) ;
        WriteRegistroC320( RegC001.RegistroC300.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC300Count := FRegistroC300Count + RegC001.RegistroC300.Count;

     RegC001.RegistroC300.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC310(RegC300: TRegistroC300);
var
  intFor: integer;
begin
  if Assigned( RegC300.RegistroC310 ) then
  begin
     if RegC300.RegistroC310.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC310, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC300.RegistroC310.Count - 1 do
     begin
        with RegC300.RegistroC310.Items[intFor] do
        begin
          Add( LFill('C310') +
               LFill( NUM_DOC_CANC ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC310Count := FRegistroC310Count + RegC300.RegistroC310.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC320(RegC300: TRegistroC300);
var
  intFor: integer;
begin
  if Assigned( RegC300.RegistroC320 ) then
  begin
     if RegC300.RegistroC320.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC320, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC300.RegistroC320.Count - 1 do
     begin
        with RegC300.RegistroC320.Items[intFor] do
        begin
          Add( LFill('C320') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC321( RegC300.RegistroC320.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC320Count := FRegistroC320Count + RegC300.RegistroC320.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC321(RegC320: TRegistroC320);
var
  intFor: integer;
begin
  if Assigned( RegC320.RegistroC321 ) then
  begin
     if RegC320.RegistroC321.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC321, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC320.RegistroC321.Count - 1 do
     begin
        with RegC320.RegistroC321.Items[intFor] do
        begin
          Add( LFill('C321') +
               LFill( COD_ITEM ) +
               DFill( QTD, 3 ) +
               LFill( UNID ) +
               LFill( VL_ITEM,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC330( RegC320.RegistroC321.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC321Count := FRegistroC321Count + RegC320.RegistroC321.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC330(RegC321: TRegistroC321);
var
  intFor: integer;
begin
  if Assigned( RegC321.RegistroC330 ) then
  begin
     for intFor := 0 to RegC321.RegistroC330.Count - 1 do
     begin
        with RegC321.RegistroC330.Items[intFor] do
        begin
          Add( LFill('C330') +
               LFill( COD_MOT_REST_COMPL ) +
               LFill( QUANT_CONV,0,6 ) +
               LFill( UNID ) +
               LFill( VL_UNIT_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_NA_OPERACAO_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_OP_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV,0,6 ) +
               LFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV,0,6 ) +
               VDFill(VL_UNIT_ICMS_ST_CONV_REST, 6) +
               VDFill(VL_UNIT_FCP_ST_CONV_REST, 6) +
               VDFill(VL_UNIT_ICMS_ST_CONV_COMPL, 6) +
               VDFill(VL_UNIT_FCP_ST_CONV_COMPL, 6));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC330Count := FRegistroC330Count + RegC321.RegistroC330.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC350(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned( RegC001.RegistroC350 ) then
  begin
     if RegC001.RegistroC350.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilB] then
           Check(False, 'O RegistroC350, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilB']);
     end;
     for intFor := 0 to RegC001.RegistroC350.Count - 1 do
     begin
        with RegC001.RegistroC350.Items[intFor] do
        begin
          Add( LFill('C350') +
               LFill( SER ) +
               LFill( SUB_SER ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( CNPJ_CPF ) +
               LFill( VL_MERC,0,2 ) +
               LFill( VL_DOC,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( COD_CTA ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC370( RegC001.RegistroC350.Items[intFor] ) ;
        WriteRegistroC390( RegC001.RegistroC350.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC350Count := FRegistroC350Count + RegC001.RegistroC350.Count;

     RegC001.RegistroC350.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC370(RegC350: TRegistroC350);
var
  intFor: integer;
begin
  if Assigned( RegC350.RegistroC370 ) then
  begin
     if RegC350.RegistroC370.Count> 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilB] then
           Check(False, 'O RegistroC370, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilB']);
     end;
     for intFor := 0 to RegC350.RegistroC370.Count - 1 do
     begin
        with RegC350.RegistroC370.Items[intFor] do
        begin
          Add( LFill('C370') +
               LFill( NUM_ITEM,3 ) +
               LFill( COD_ITEM ) +
               DFill( QTD, 3 ) +
               LFill( UNID ) +
               LFill( VL_ITEM,0,3 ) +
               LFill( VL_DESC,0,3 ) ) ;
        end;
        WriteRegistroC380( RegC350.RegistroC370.Items[intFor] ) ;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC370Count := FRegistroC370Count + RegC350.RegistroC370.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC380(RegC370: TRegistroC370);
var
  intFor: integer;
begin
  if Assigned( RegC370.RegistroC380 ) then
  begin
     for intFor := 0 to RegC370.RegistroC380.Count - 1 do
     begin
        with RegC370.RegistroC380.Items[intFor] do
        begin
          Add( LFill('C380') +
               LFill( COD_MOT_REST_COMPL ) +
               LFill( QUANT_CONV,0,6 ) +
               LFill( UNID ) +
               LFill( VL_UNIT_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_NA_OPERACAO_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_OP_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV,0,6 ) +
               LFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV,0,6 ) +
               LFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV,0,6 ) +
               VDFill(VL_UNIT_ICMS_ST_CONV_REST, 6) +
               VDFill(VL_UNIT_FCP_ST_CONV_REST, 6) +
               VDFill(VL_UNIT_ICMS_ST_CONV_COMPL, 6) +
               VDFill(VL_UNIT_FCP_ST_CONV_COMPL, 6) +
               LFill( CST_ICMS ) +
               LFill( CFOP ));
        end;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC380Count := FRegistroC380Count + RegC370.RegistroC380.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC390(RegC350: TRegistroC350) ;
var
  intFor: integer;
begin
  if Assigned( RegC350.RegistroC390 ) then
  begin
     if RegC350.RegistroC390.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilB] then
           Check(False, 'O RegistroC390, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilB']);
     end;
     for intFor := 0 to RegC350.RegistroC390.Count - 1 do
     begin
        with RegC350.RegistroC390.Items[intFor] do
        begin
          Add( LFill('C390') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,0,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC390Count := FRegistroC390Count + RegC350.RegistroC390.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC400(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned( RegC001.RegistroC400 ) then
  begin
     for intFor := 0 to RegC001.RegistroC400.Count - 1 do
     begin
        with RegC001.RegistroC400.Items[intFor] do
        begin
          Add( LFill('C400') +
               LFill(COD_MOD ) +
               LFill(ECF_MOD) +
               LFill(ECF_FAB) +
               LFill(ECF_CX ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC405( RegC001.RegistroC400.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC400Count := FRegistroC400Count + RegC001.RegistroC400.Count;

     RegC001.RegistroC400.Clear;
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
          Add( LFill('C405') +
               LFill( DT_DOC ) +
               LFill( CRO,3 ) +
               LFill( CRZ,6 ) +
               IfThen( DT_INI >= EncodeDate(2013,10,01), LFill( NUM_COO_FIN, 9) , LFill( NUM_COO_FIN, 6) ) +
               LFill( GT_FIN,0,2  ) +
               LFill( VL_BRT,0,2  ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC410( RegC400.RegistroC405.Items[intFor] ) ;
        WriteRegistroC420( RegC400.RegistroC405.Items[intFor] ) ;
        WriteRegistroC460( RegC400.RegistroC405.Items[intFor] ) ;
        WriteRegistroC490( RegC400.RegistroC405.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC405Count := FRegistroC405Count + RegC400.RegistroC405.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC410(RegC405: TRegistroC405);
var
  intFor: integer;
begin
  if Assigned( RegC405.RegistroC410 ) then
  begin
     for intFor := 0 to RegC405.RegistroC410.Count - 1 do
     begin
        with RegC405.RegistroC410.Items[intFor] do
        begin
          Add( LFill('C410') +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC410Count := FRegistroC410Count + RegC405.RegistroC410.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC420(RegC405: TRegistroC405) ;
var
  intFor: integer;
begin
  if Assigned( RegC405.RegistroC420 ) then
  begin
     for intFor := 0 to RegC405.RegistroC420.Count - 1 do
     begin
        with RegC405.RegistroC420.Items[intFor] do
        begin
          Add( LFill('C420') +
               LFill( COD_TOT_PAR ) +
               LFill( VLR_ACUM_TOT,0,2) +
               LFill( NR_TOT, 2, true) +
               LFill( DESCR_NR_TOT ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC425( RegC405.RegistroC420.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC420Count := FRegistroC420Count + RegC405.RegistroC420.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC425(RegC420: TRegistroC420);
var
  intFor: integer;
begin
  if Assigned( RegC420.RegistroC425 ) then
  begin
     if RegC420.RegistroC425.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC425, não é obrigatório no %s.', ['PerfilA']);
     end;
     for intFor := 0 to RegC420.RegistroC425.Count - 1 do
     begin
        with RegC420.RegistroC425.Items[intFor] do
        begin
          Check(FBloco_0.Registro0001.Registro0200.LocalizaRegistro(COD_ITEM), '(C-C425) ITENS: O código do item "%s" não existe no registro 0200!', [COD_ITEM]);

          Add( LFill('C425') +
               LFill( COD_ITEM ) +
               DFill( QTD,3 ) +
               LFill( UNID ) +
               LFill( VL_ITEM,0,2 ) +
               LFill( VL_PIS ,0,2 ) +
               LFill( VL_COFINS ,0,2 ) ) ;
        end;
        WriteRegistroC430( RegC420.RegistroC425.Items[intFor] ) ;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC425Count := FRegistroC425Count + RegC420.RegistroC425.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC430(RegC425: TRegistroC425);
var
  intFor: integer;
begin
  if Assigned( RegC425.RegistroC430 ) then
  begin
     for intFor := 0 to RegC425.RegistroC430.Count - 1 do
     begin
        with RegC425.RegistroC430.Items[intFor] do
        begin
          Add( LFill('C430') +
            LFill( COD_MOT_REST_COMPL ) +
            LFill( QUANT_CONV,0,6 ) +
            LFill( UNID ) +
            LFill( VL_UNIT_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_NA_OPERACAO_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_OP_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV,0,6 ) +
            LFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV,0,6 ) +
            VDFill(VL_UNIT_ICMS_ST_CONV_REST, 6) +
            VDFill(VL_UNIT_FCP_ST_CONV_REST, 6) +
            VDFill(VL_UNIT_ICMS_ST_CONV_COMPL, 6) +
            VDFill(VL_UNIT_FCP_ST_CONV_COMPL, 6) +
            LFill( CST_ICMS ) +
            LFill( CFOP ));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC430Count := FRegistroC430Count + RegC425.RegistroC430.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC460(RegC405: TRegistroC405);
var
  intFor: integer;
  strCOD_SIT: String;
  strLinha: String;
begin
  if Assigned( RegC405.RegistroC460 ) then
  begin
     if RegC405.RegistroC460.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilB] then
           Check(False, 'O RegistroC460, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilB']);
     end;
     for intFor := 0 to RegC405.RegistroC460.Count - 1 do
     begin
        with RegC405.RegistroC460.Items[intFor] do
        begin
          case COD_SIT of
           sdRegular:               strCOD_SIT := '00';
           sdExtempRegular:         strCOD_SIT := '01';
           sdCancelado:             strCOD_SIT := '02';
           sdCanceladoExtemp:       strCOD_SIT := '03';
           sdDoctoDenegado:         strCOD_SIT := '04';
           sdDoctoNumInutilizada:   strCOD_SIT := '05';
           sdFiscalCompl:           strCOD_SIT := '06';
           sdExtempCompl:           strCOD_SIT := '07';
           sdRegimeEspecNEsp:       strCOD_SIT := '08';
          end;

          strLinha := LFill('C460') +
                      LFill( COD_MOD ) +
                      LFill( strCOD_SIT ) +
                      IfThen( DT_INI >= EncodeDate(2013,10,01), LFill( NUM_DOC, 9) , LFill( NUM_DOC, 6) ) +
                      LFill( DT_DOC, 'ddmmyyyy' ) +
                      LFill( VL_DOC,0,2, true ) +
                      LFill( VL_PIS,0,2, true ) +
                      LFill( VL_COFINS,0,2, true ) +
                      LFill( CPF_CNPJ ) +
                      LFill( NOM_ADQ )  ;

          //-- Write
          if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC460) then
             TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC460(strLinha);

          Add(strLinha);
        end;
        /// Registros FILHOS
        WriteRegistroC470( RegC405.RegistroC460.Items[intFor] );
        WriteRegistroC465( RegC405.RegistroC460.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC460Count := FRegistroC460Count + RegC405.RegistroC460.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC465(RegC460: TRegistroC460);
var
  intFor: integer;
  strLinha: String;
begin
  if Assigned( RegC460.RegistroC465) then
  begin
     if RegC460.RegistroC465.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilB] then
           Check(False, 'O RegistroC465, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilB']);
     end;
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC465) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC465(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to RegC460.RegistroC465.Count - 1 do
     begin
        with RegC460.RegistroC465.Items[intFor] do
        begin
          strLinha := LFill('C465') +
               LFill( CHV_CFE ) +
               LFill( NUM_CCF ) ;
          //-- Write
          if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC465) then
             TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC465(strLinha);

          Add(strLinha);
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC465) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC465(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC465Count := FRegistroC465Count + RegC460.RegistroC465.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC470(RegC460: TRegistroC460);
var
  intFor: integer;
  strLinha: String;
begin
  if Assigned( RegC460.RegistroC470 ) then
  begin
     if RegC460.RegistroC470.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilB] then
           Check(False, 'O RegistroC470, não deve ser gerado em movimentações de saída, no %s, conforme ATO COTEPE 09/08', ['PerfilB']);
     end;
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC470) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC470(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to RegC460.RegistroC470.Count - 1 do
     begin
        with RegC460.RegistroC470.Items[intFor] do
        begin
          strLinha := LFill('C470') +
                      LFill( COD_ITEM ) +
                      DFill( QTD,3 ) +
                      DFill( QTD_CANC,3 ) +
                      LFill( UNID ) +
                      LFill( VL_ITEM,0,2 ) +
                      LFill( CST_ICMS,3 ) +
                      LFill( CFOP,4 ) +
                      LFill( ALIQ_ICMS,6,2 ) +
                      LFill( VL_PIS,0,2 ) +
                      LFill( VL_COFINS,0,2 );
          //-- Write
          if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC470) then
             TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC470(strLinha);

          Add(strLinha);
        end;
        WriteRegistroC480( RegC460.RegistroC470.Items[intFor] );
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC470) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC470(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC470Count := FRegistroC470Count + RegC460.RegistroC470.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC480(RegC470: TRegistroC470);
var
  intFor: integer;
begin
  if Assigned( RegC470.RegistroC480 ) then
  begin
     for intFor := 0 to RegC470.RegistroC480.Count - 1 do
     begin
        with RegC470.RegistroC480.Items[intFor] do
        begin
          Add( LFill('C480') +
            LFill( COD_MOT_REST_COMPL ) +
            LFill( QUANT_CONV,0,6 ) +
            LFill( UNID ) +
            LFill( VL_UNIT_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_NA_OPERACAO_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_OP_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV,0,6 ) +
            LFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV,0,6 ) +
            LFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV,0,6 ) +
            VDFill(VL_UNIT_ICMS_ST_CONV_REST, 6) +
            VDFill(VL_UNIT_FCP_ST_CONV_REST, 6) +
            VDFill(VL_UNIT_ICMS_ST_CONV_COMPL, 6) +
            VDFill(VL_UNIT_FCP_ST_CONV_COMPL, 6) +
            LFill( CST_ICMS ) +
            LFill( CFOP ));
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC480Count := FRegistroC480Count + RegC470.RegistroC480.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC490(RegC405: TRegistroC405);
var
  intFor: integer;
begin
  if Assigned( RegC405.RegistroC490 ) then
  begin
     for intFor := 0 to RegC405.RegistroC490.Count - 1 do
     begin
        with RegC405.RegistroC490.Items[intFor] do
        begin
          Add( LFill('C490') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC490Count := FRegistroC490Count + RegC405.RegistroC490.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC495(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned( RegC001.RegistroC495 ) then
  begin
     if
      (RegC001.RegistroC495.Count > 0) and
      (DT_INI >= EncodeDate(2014,01,01)) and
      (FBloco_0.Registro0000.UF = 'BA')
     then
       Check(False, 'A partir de 01/01/2014, os contribuintes situados na Bahia obrigados a este registro devem apresentar o registro C425.');
                        ;
     for intFor := 0 to RegC001.RegistroC495.Count - 1 do
     begin
        with RegC001.RegistroC495.Items[intFor] do
        begin
          Add( LFill('C495') +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( COD_ITEM ) +
               DFill( QTD,3 ) +
               DFill( QTD_CANC,3 ) +
               LFill( UNID ) +
               LFill( VL_ITEM,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_CANC,0,2 ) +
               LFill( VL_ACMO,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_ISEN,0,2 ) +
               LFill( VL_NT,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC495Count := FRegistroC495Count + RegC001.RegistroC495.Count;

     RegC001.RegistroC495.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC500(RegC001: TRegistroC001);
var
  intFor: Integer;
  intTP_LIGACAO: Integer;
  strCOD_SIT: string;
  strCOD_GRUPO_TENSAO: String;
  vFin_DOCe: string;
  intIND_DEST: Integer;
  UmRegC500: TRegistroC500;
begin
  if Assigned( RegC001.RegistroC500 ) then
  begin
    for intFor := 0 to RegC001.RegistroC500.Count - 1 do
    begin
      UmRegC500 := RegC001.RegistroC500.Items[intFor];
      // COD_MOD
      Check(MatchText(UmRegC500.COD_MOD, ['06', '28', '29', '66']), 'Registro C500 : O código do modelo "%s" não está na lista de valores válidos "%s" !',
                                                   [UmRegC500.COD_MOD, '[06, 28, 29, 66]']);
      // COD_CONS
      Check(funChecaCOD_CONS(UmRegC500.COD_MOD, UmRegC500.COD_CONS), 'Registro C500 : Se o modelo for 06 (energia elétrica) ou 28 (gás canalizado), ' +
                                                'os valores válidos são "%s". Se o modelo for 29 (água canalizada), o valor deve ' +
                                                'constar da Tabela 4.4.2 do Ato COTEPE/ICMS nº 09, de 18 de abril de 2008. !',
                                                ['[01, 02, 03, 04, 05, 06, 07, 08]']);

      strCOD_SIT := CodSitToStr(UmRegC500.COD_SIT);

      strCOD_GRUPO_TENSAO := GrupoTensaoToStr(UmRegC500.COD_GRUPO_TENSAO);

      intTP_LIGACAO := TipoLigacaoToInt(UmRegC500.TP_LIGACAO);

      if UmRegC500.COD_MOD = '66' then
      begin
       vFin_DOCe := FinalidadeEmissaoDocEletToStr(UmRegC500.FIN_DOCe);
      end
      else
      begin
       vFin_DOCe := '';
      end;

      if UmRegC500.IND_OPER = tpEntradaAquisicao then
        intIND_DEST := 0
      else
        intIND_DEST := IndicadorDestinatarioAcessanteToInt(UmRegC500.IND_DEST);

      if FBloco_0.Registro0000.COD_VER >= vlVersao115 then
      begin
        // A partir de 01/01/2022
        Add( LFill('C500') +
            LFill( Integer(UmRegC500.IND_OPER), 0 ) +
            LFill( Integer(UmRegC500.IND_EMIT), 0 ) +
            LFill( UmRegC500.COD_PART ) +
            LFill( UmRegC500.COD_MOD, 2 ) +
            LFill( strCOD_SIT ) +
            LFill( UmRegC500.SER ) +
            LFill( UmRegC500.SUB ) +
            LFill( UmRegC500.COD_CONS ) +
            LFill( UmRegC500.NUM_DOC, 9 ) +
            LFill( UmRegC500.DT_DOC ) +
            LFill( UmRegC500.DT_E_S ) +
            LFill( UmRegC500.VL_DOC,0,2 ) +
            LFill( UmRegC500.VL_DESC,0,2 ) +
            LFill( UmRegC500.VL_FORN,0,2 ) +
            LFill( UmRegC500.VL_SERV_NT,0,2 ) +
            LFill( UmRegC500.VL_TERC,0,2 ) +
            LFill( UmRegC500.VL_DA,0,2 ) +
            LFill( UmRegC500.VL_BC_ICMS,0,2 ) +
            LFill( UmRegC500.VL_ICMS,0,2 ) +
            LFill( UmRegC500.VL_BC_ICMS_ST,0,2 ) +
            LFill( UmRegC500.VL_ICMS_ST,0,2 ) +
            LFill( UmRegC500.COD_INF ) +
            LFill( UmRegC500.VL_PIS,0,2, True ) +
            LFill( UmRegC500.VL_COFINS,0,2, True ) +
            LFill( intTP_LIGACAO, 0, True ) +
            LFill( strCOD_GRUPO_TENSAO ) +
            LFill( UmRegC500.CHV_DOCe ) +
            LFill( vFin_DOCe ) +
            LFill( UmRegC500.CHV_DOCe_REF ) +
            LFill( intIND_DEST, 0, True  ) +
            LFill( UmRegC500.COD_MUN_DEST ) +
            LFill( UmRegC500.COD_CTA ) +
            LFill( UmRegC500.COD_MOD_DOC_REF ) +
            LFill( UmRegC500.HASH_DOC_REF ) +
            LFill( UmRegC500.SER_DOC_REF ) +
            LFill( UmRegC500.NUM_DOC_REF ) +
            LFill( UmRegC500.MES_DOC_REF ) +
            VLFill( UmRegC500.ENER_INJET, 0) +
            VLFill( UmRegC500.OUTRAS_DED, 0)
           ) ;
      end
      else if FBloco_0.Registro0000.COD_VER >= vlVersao113 then
      begin
        // A partir de 01/01/2020
        Add( LFill('C500') +
             LFill( Integer(UmRegC500.IND_OPER), 0 ) +
             LFill( Integer(UmRegC500.IND_EMIT), 0 ) +
             LFill( UmRegC500.COD_PART ) +
             LFill( UmRegC500.COD_MOD,2 ) +
             LFill( strCOD_SIT ) +
             LFill( UmRegC500.SER ) +
             LFill( UmRegC500.SUB ) +
             LFill( UmRegC500.COD_CONS ) +
             LFill( UmRegC500.NUM_DOC, 9 ) +
             LFill( UmRegC500.DT_DOC ) +
             LFill( UmRegC500.DT_E_S ) +
             LFill( UmRegC500.VL_DOC,0,2 ) +
             LFill( UmRegC500.VL_DESC,0,2 ) +
             LFill( UmRegC500.VL_FORN,0,2 ) +
             LFill( UmRegC500.VL_SERV_NT,0,2 ) +
             LFill( UmRegC500.VL_TERC,0,2 ) +
             LFill( UmRegC500.VL_DA,0,2 ) +
             LFill( UmRegC500.VL_BC_ICMS,0,2 ) +
             LFill( UmRegC500.VL_ICMS,0,2 ) +
             LFill( UmRegC500.VL_BC_ICMS_ST,0,2 ) +
             LFill( UmRegC500.VL_ICMS_ST,0,2 ) +
             LFill( UmRegC500.COD_INF ) +
             LFill( UmRegC500.VL_PIS,0,2, True ) +
             LFill( UmRegC500.VL_COFINS,0,2, True ) +
             LFill( intTP_LIGACAO, 0, True ) +
             LFill( strCOD_GRUPO_TENSAO ) +
             LFill( UmRegC500.CHV_DOCe ) +
             LFill( vFin_DOCe ) +
             LFill( UmRegC500.CHV_DOCe_REF ) +
             LFill( intIND_DEST, 0, True  ) +
             LFill( UmRegC500.COD_MUN_DEST ) +
             LFill( UmRegC500.COD_CTA )
            ) ;
      end
      else
      begin
        // Até 31/12/2019
        Add( LFill('C500') +
             LFill( Integer(UmRegC500.IND_OPER), 0 ) +
             LFill( Integer(UmRegC500.IND_EMIT), 0 ) +
             LFill( UmRegC500.COD_PART ) +
             LFill( UmRegC500.COD_MOD,2 ) +
             LFill( strCOD_SIT ) +
             LFill( UmRegC500.SER ) +
             LFill( UmRegC500.SUB ) +
             LFill( UmRegC500.COD_CONS ) +
             LFill( UmRegC500.NUM_DOC, 9 ) +
             LFill( UmRegC500.DT_DOC ) +
             LFill( UmRegC500.DT_E_S ) +
             LFill( UmRegC500.VL_DOC,0,2 ) +
             LFill( UmRegC500.VL_DESC,0,2 ) +
             LFill( UmRegC500.VL_FORN,0,2 ) +
             LFill( UmRegC500.VL_SERV_NT,0,2 ) +
             LFill( UmRegC500.VL_TERC,0,2 ) +
             LFill( UmRegC500.VL_DA,0,2 ) +
             LFill( UmRegC500.VL_BC_ICMS,0,2 ) +
             LFill( UmRegC500.VL_ICMS,0,2 ) +
             LFill( UmRegC500.VL_BC_ICMS_ST,0,2 ) +
             LFill( UmRegC500.VL_ICMS_ST,0,2 ) +
             LFill( UmRegC500.COD_INF ) +
             LFill( UmRegC500.VL_PIS,0,2, True ) +
             LFill( UmRegC500.VL_COFINS,0,2, True ) +
             LFill( intTP_LIGACAO, 0, True ) +
             LFill( strCOD_GRUPO_TENSAO )
           ) ;
      end;

      /// Registro FILHOS do FILHO
      WriteRegistroC510(UmRegC500);
      WriteRegistroC590(UmRegC500);
      WriteRegistroC595(UmRegC500);

      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC500Count := FRegistroC500Count + RegC001.RegistroC500.Count;

    RegC001.RegistroC500.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC510(RegC500:TRegistroC500);
var
  intFor: integer;
  strLinha: String;
begin
  if Assigned( RegC500.RegistroC510 ) then
  begin
     if RegC500.RegistroC510.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilB] then
           Check(False, 'O RegistroC510, não deve ser gerado em movimentações de entrada nem saída, no %s , conforme ATO COTEPE 09/08', ['PerfilB']);
     end;
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC510) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnBeforeWriteRegistroC510(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to RegC500.RegistroC510.Count - 1 do
     begin
        with RegC500.RegistroC510.Items[intFor] do
        begin
          strLinha := LFill('C510') +
                      LFill( NUM_ITEM,3 ) +
                      LFill( COD_ITEM ) +
                      LFill( COD_CLASS,4 ) +
                      DFill( QTD,3 ) +
                      LFill( UNID ) +
                      LFill( VL_ITEM,0,2 ) +
                      LFill( VL_DESC,0,2 ) +
                      LFill( CST_ICMS,3 ) +
                      LFill( CFOP,4 ) +
                      LFill( VL_BC_ICMS,0,2 ) +
                      LFill( ALIQ_ICMS,0,2 ) +
                      LFill( VL_ICMS,0,2 ) +
                      LFill( VL_BC_ICMS_ST,0,2 ) +
                      LFill( ALIQ_ST,0,2 ) +
                      LFill( VL_ICMS_ST,0,2 ) +
                      LFill( Integer(IND_REC), 0 ) +
                      LFill( COD_PART  ) +
                      LFill( VL_PIS,0,2 ) +
                      LFill( VL_COFINS,0,2 ) +
                      LFill( COD_CTA );
          //-- Write
          if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC510) then
             TACBrSPEDFiscal(FOwner).EventsBloco_C.OnWriteRegistroC510(strLinha);

          Add(strLinha);
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     //-- Before
     strLinha := '';
     if Assigned(TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC510) then
     begin
        TACBrSPEDFiscal(FOwner).EventsBloco_C.OnAfterWriteRegistroC510(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC510Count := FRegistroC510Count + RegC500.RegistroC510.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC590(RegC500:TRegistroC500) ;
var
  intFor: integer;
begin
  if Assigned( RegC500.RegistroC590 ) then
  begin
     for intFor := 0 to RegC500.RegistroC590.Count - 1 do
     begin
        with RegC500.RegistroC590.Items[intFor] do
        begin
          Add( LFill('C590') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS  ) ) ;
        end;
        WriteRegistroC591( RegC500.RegistroC590.Items[intFor] ) ;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC590Count := FRegistroC590Count + RegC500.RegistroC590.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC591(RegC590:TRegistroC590) ;
var
  intFor: integer;
begin
  if Assigned( RegC590.RegistroC591 ) then
  begin
     for intFor := 0 to RegC590.RegistroC591.Count - 1 do
     begin
        with RegC590.RegistroC591.Items[intFor] do
        begin
          Add( LFill('C591') +
               LFill( VL_FCP_OP,0,2 ) +
               LFill( VL_FCP_ST,0,2 )) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC591Count := FRegistroC591Count + RegC590.RegistroC591.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC595(RegC500:TRegistroC500) ;
var
  intFor: integer;
begin
  if Assigned( RegC500.RegistroC595 ) then
  begin
     for intFor := 0 to RegC500.RegistroC595.Count - 1 do
     begin
        with RegC500.RegistroC595.Items[intFor] do
        begin
          Add( LFill('C595') +
               LFill( COD_OBS ) +
               LFill( TXT_COMPL ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroC597( RegC500.RegistroC595.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC595Count := FRegistroC595Count + RegC500.RegistroC595.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC597(RegC595: TRegistroC595);
var
  intFor: integer;
begin
  if Assigned(RegC595.RegistroC597 ) then
  begin
     for intFor := 0 to RegC595.RegistroC597.Count - 1 do
     begin
        with RegC595.RegistroC597.Items[intFor] do
        begin
          Add( LFill('C597') +
               LFill( COD_AJ ) +
               LFill( DESCR_COMPL_AJ ) +
               LFill( COD_ITEM ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_OUTROS,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC597Count := FRegistroC597Count + RegC595.RegistroC597.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC600(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned( RegC001.RegistroC600 ) then
  begin
     if RegC001.RegistroC600.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC600, não deve ser gerado em movimentações de entrada nem saída, no %s , conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC001.RegistroC600.Count - 1 do
     begin
        with RegC001.RegistroC600.Items[intFor] do
        begin
          Add( LFill('C600') +
               LFill( COD_MOD,2 ) +
               LFill( COD_MUN ,7 ) +
               LFill( SER,4 ) +
               LFill( SUB,3 ) +
               LFill( COD_CONS,2 ) +
               LFill( QTD_CONS,0 ) +
               LFill( QTD_CANC,0 ) +
               LFill( DT_DOC ) +
               LFill( VL_DOC,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( CONS,0 ) +
               LFill( VL_FORN,0,2 ) +
               LFill( VL_SERV_NT,0,2 ) +
               LFill( VL_TERC,0,2 ) +
               LFill( VL_DA,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC601( RegC001.RegistroC600.Items[intFor] ) ;
        WriteRegistroC610( RegC001.RegistroC600.Items[intFor] ) ;
        WriteRegistroC690( RegC001.RegistroC600.Items[intFor] ) ;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC600Count := FRegistroC600Count + RegC001.RegistroC600.Count;

     RegC001.RegistroC600.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC601(RegC600: TRegistroC600);
var
  intFor: integer;
begin
  if Assigned( RegC600.RegistroC601 ) then
  begin
     if RegC600.RegistroC601.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC601, não deve ser gerado em movimentações de entrada nem saída, no %s , conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC600.RegistroC601.Count - 1 do
     begin
        with RegC600.RegistroC601.Items[intFor] do
        begin
          Add( LFill('C601') +
               LFill( NUM_DOC_CANC ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC601Count := FRegistroC601Count + RegC600.RegistroC601.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC610(RegC600: TRegistroC600) ;
var
  intFor: integer;
begin
  if Assigned( RegC600.RegistroC610 ) then
  begin
     if RegC600.RegistroC610.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC610, não deve ser gerado em movimentações de entrada nem saída, no %s , conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC600.RegistroC610.Count - 1 do
     begin
        with RegC600.RegistroC610.Items[intFor] do
        begin
          Add( LFill('C610') +
               LFill( COD_CLASS,4 ) +
               LFill( COD_ITEM ) +
               LFill( QTD,3 ) +
               LFill( UNID ) +
               LFill( VL_ITEM,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,0,3 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( COD_CTA ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC610Count := FRegistroC610Count + RegC600.RegistroC610.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC690(RegC600: TRegistroC600);
var
  intFor: integer;
begin
  if Assigned( RegC600.RegistroC690 ) then
  begin
     if RegC600.RegistroC690.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           Check(False, 'O RegistroC690, não deve ser gerado em movimentações de entrada nem saída, no %s , conforme ATO COTEPE 09/08', ['PerfilA']);
     end;
     for intFor := 0 to RegC600.RegistroC690.Count - 1 do
     begin
        with RegC600.RegistroC690.Items[intFor] do
        begin
          Add( LFill('C690') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC690Count := FRegistroC690Count + RegC600.RegistroC690.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC700(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned( RegC001.RegistroC700 ) then
  begin
     for intFor := 0 to RegC001.RegistroC700.Count - 1 do
     begin
        with RegC001.RegistroC700.Items[intFor] do
        begin
          Add( LFill('C700') +
               LFill( COD_MOD,2 ) +
               LFill( SER,4 ) +
               LFill( NRO_ORD_INI,9 ) +
               LFill( NRO_ORD_FIN,9 ) +
               LFill( DT_DOC_INI ) +
               LFill( DT_DOC_FIN ) +
               LFill( NOM_MEST ) +
               LFill( CHV_COD_DIG ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC790( RegC001.RegistroC700.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC700Count := FRegistroC700Count + RegC001.RegistroC700.Count;

     RegC001.RegistroC700.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC790(RegC700: TRegistroC700);
var
  intFor: integer;
begin
  if Assigned( RegC700.RegistroC790 ) then
  begin
     for intFor := 0 to RegC700.RegistroC790.Count - 1 do
     begin
        with RegC700.RegistroC790.Items[intFor] do
        begin
          Add( LFill('C790') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC791( RegC700.RegistroC790.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC790Count := FRegistroC790Count + RegC700.RegistroC790.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC791(RegC790: TRegistroC790);
var
  intFor: integer;
begin
  if Assigned( RegC790.RegistroC791 ) then
  begin
     for intFor := 0 to RegC790.RegistroC791.Count - 1 do
     begin
        with RegC790.RegistroC791.Items[intFor] do
        begin
          Add( LFill('C791') +
               LFill( UF ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) ) ;
        end;
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC791Count := FRegistroC791Count + RegC790.RegistroC791.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC800(RegC001: TRegistroC001);
var
  intFor: integer;
  strCOD_SIT: String;
  booNFCancelada: Boolean;
begin
  if Assigned( RegC001.RegistroC800 ) then
  begin
     for intFor := 0 to RegC001.RegistroC800.Count - 1 do
     begin
        with RegC001.RegistroC800.Items[intFor] do
        begin
          case COD_SIT of
           sdRegular:               strCOD_SIT := '00';
           sdExtempRegular:         strCOD_SIT := '01';
           sdCancelado:             strCOD_SIT := '02';
           sdCanceladoExtemp:       strCOD_SIT := '03';
           sdDoctoDenegado:         strCOD_SIT := '04';
           sdDoctoNumInutilizada:   strCOD_SIT := '05';
           sdFiscalCompl:           strCOD_SIT := '06';
           sdExtempCompl:           strCOD_SIT := '07';
           sdRegimeEspecNEsp:       strCOD_SIT := '08';
          end;

          /// Tratamento NFs canceladas 02/03, denegada 04 ou inutilizada 05 
          booNFCancelada := Pos(strCOD_SIT,'02, 03, 04, 05') > 0;
          if booNFCancelada then
          begin
            VL_PIS       := Null;
            VL_COFINS    := Null;
            VL_PIS_ST    := Null;
            VL_COFINS_ST := Null;
          end;

          Add( LFill('C800') +
               LFill( COD_MOD,2 ) +
               LFill( strCOD_SIT  ) +
               LFill( NUM_CFE,6 ) +
               LFill( DT_DOC, 'ddmmyyyy', booNFCancelada ) +
               LFill( VL_CFE,0,2, booNFCancelada ) +
               VLFill( VL_PIS,0,2 ) +
               VLFill( VL_COFINS,0,2) +
               LFill( CNPJ_CPF ) + // LFill( CNPJ_CPF, 14, True )  Não pode Formatar ( Versão 2.1.5 PVA )
               LFill( NR_SAT,9 ) +
               LFill( CHV_CFE ) +
               LFill( VL_DESC,0,2, booNFCancelada ) +
               LFill( VL_MERC,0,2, booNFCancelada ) +
               LFill( VL_OUT_DA,0,2, booNFCancelada ) +
               LFill( VL_ICMS,0,2, booNFCancelada ) +
               VLFill( VL_PIS_ST,0,2 ) +
               VLFill( VL_COFINS_ST,0,2) ) ;
        end;

        /// Registros FILHOS
        WriteRegistroC810( RegC001.RegistroC800.Items[intFor] );
        WriteRegistroC850( RegC001.RegistroC800.Items[intFor] );
        WriteRegistroC855( RegC001.RegistroC800.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC800Count := FRegistroC800Count + RegC001.RegistroC800.Count;

     RegC001.RegistroC800.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC810(RegC800: TRegistroC800);
var
  intFor: integer;
begin
  if Assigned( RegC800.RegistroC810 ) then
  begin
    for intFor := 0 to RegC800.RegistroC810.Count - 1 do
    begin
      with RegC800.RegistroC810.Items[intFor] do
      begin
        Add( LFill('C810') +
             LFill( NUM_ITEM ) +
             LFill( COD_ITEM ) +
             LFill( QTD, 0, 5 ) +
             LFill( UNID ) +
             LFill( VL_ITEM, 0, 2 ) +
             LFill( CST_ICMS ) +
             LFill( CFOP )
           ) ;
      end;
      WriteRegistroC815(RegC800.RegistroC810.Items[intFor]);
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC810Count := FRegistroC810Count + RegC800.RegistroC810.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC815(RegC810: TRegistroC810);
begin
  with RegC810.RegistroC815 do
  begin
    Add( LFill('C815') +
         LFill( COD_MOT_REST_COMPL ) +
         LFill( QUANT_CONV, 0, 6 ) +
         LFill( UNID ) +
         LFill( VL_UNIT_CONV, 0, 6 ) +
         VLFill( VL_UNIT_ICMS_NA_OPERACAO_CONV, 6 ) +
         VLFill( VL_UNIT_ICMS_OP_CONV, 6 ) +
         VLFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV, 6 ) +
         VLFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV, 6 ) +
         VLFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV, 6 ) +
         VDFill( VL_UNIT_ICMS_ST_CONV_REST, 6 ) +
         VDFill( VL_UNIT_FCP_ST_CONV_REST, 6 ) +
         VDFill( VL_UNIT_ICMS_ST_CONV_COMPL, 6 ) +
         VDFill( VL_UNIT_FCP_ST_CONV_COMPL, 6 )
       ) ;
  end;

  RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
end;

procedure TBloco_C.WriteRegistroC850(RegC800: TRegistroC800);
var
  intFor: integer;
begin
  if Assigned( RegC800.RegistroC850 ) then
  begin
     for intFor := 0 to RegC800.RegistroC850.Count - 1 do
     begin
        with RegC800.RegistroC850.Items[intFor] do
        begin
          Add( LFill('C850') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC850Count := FRegistroC850Count + RegC800.RegistroC850.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC855(RegC800: TRegistroC800);
var
  intFor: Integer;
begin
  if (FBloco_0.Registro0000.COD_VER < vlVersao116) then
    Exit;

  if Assigned(RegC800.RegistroC855) then
  begin
     for intFor := 0 to RegC800.RegistroC855.Count - 1 do
     begin
        with RegC800.RegistroC855.Items[intFor] do
          Add(LFill('C855') +
              LFill(COD_OBS) +
              LFill(TXT_COMPL));

        WriteRegistroC857(RegC800.RegistroC855.Items[intFor]);

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;

     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC855Count := FRegistroC855Count + RegC800.RegistroC855.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC857(RegC855: TRegistroC855);
var
  intFor: Integer;
begin
  if Assigned(RegC855.RegistroC857) then
  begin
     for intFor := 0 to RegC855.RegistroC857.Count - 1 do
     begin
        with RegC855.RegistroC857.Items[intFor] do
          Add(LFill('C857') +
              LFill(COD_AJ) +
              LFill(DESCR_COMPL_AJ) +
              LFill(COD_ITEM) +
              LFill(VL_BC_ICMS, 0, 2) +
              LFill(ALIQ_ICMS, 6, 2) +
              LFill(VL_ICMS, 0, 2) +
              LFill(VL_OUTROS, 0, 2));
        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;

     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC857Count := FRegistroC857Count + RegC855.RegistroC857.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC860(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned( RegC001.RegistroC860 ) then
  begin
     for intFor := 0 to RegC001.RegistroC860.Count - 1 do
     begin
        with RegC001.RegistroC860.Items[intFor] do
        begin
          Add( LFill('C860') +
               LFill( COD_MOD,2 ) +
               LFill( NR_SAT ) +
               LFill( DT_DOC ) +
               LFill( DOC_INI ) +
               LFill( DOC_FIN ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroC870( RegC001.RegistroC860.Items[intFor] );
        WriteRegistroC890( RegC001.RegistroC860.Items[intFor] );
        WriteRegistroC895( RegC001.RegistroC860.Items[intFor] );

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC860Count := FRegistroC860Count + RegC001.RegistroC860.Count;

     RegC001.RegistroC860.Clear;
  end;
end;

procedure TBloco_C.WriteRegistroC870(RegC860: TRegistroC860);
var
  intFor: integer;
begin
  if Assigned( RegC860.RegistroC870 ) then
  begin
    for intFor := 0 to RegC860.RegistroC870.Count - 1 do
    begin
      with RegC860.RegistroC870.Items[intFor] do
      begin
        Add( LFill('C870') +
             LFill( COD_ITEM ) +
             LFill( QTD, 0, 5) +
             LFill( UNID ) +
             LFill( CST_ICMS ) +
             LFill( CFOP )
            ) ;
      end;

      WriteRegistroC880(RegC860.RegistroC870.Items[intFor]);
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC870Count := FRegistroC870Count + RegC860.RegistroC870.Count;
  end;

end;

procedure TBloco_C.WriteRegistroC880(RegC870: TRegistroC870);
begin
  with RegC870.RegistroC880 do
  begin
    Add( LFill('C880') +
         LFill( COD_MOT_REST_COMPL ) +
         LFill( QUANT_CONV, 0, 6 ) +
         LFill( UNID ) +
         LFill( VL_UNIT_CONV, 0, 6 ) +
         VLFill( VL_UNIT_ICMS_NA_OPERACAO_CONV, 6 ) +
         VLFill( VL_UNIT_ICMS_OP_CONV, 6 ) +
         VLFill( VL_UNIT_ICMS_OP_ESTOQUE_CONV, 6 ) +
         VLFill( VL_UNIT_ICMS_ST_ESTOQUE_CONV, 6 ) +
         VLFill( VL_UNIT_FCP_ICMS_ST_ESTOQUE_CONV, 6 ) +
         VDFill( VL_UNIT_ICMS_ST_CONV_REST, 6 ) +
         VDFill( VL_UNIT_FCP_ST_CONV_REST, 6 ) +
         VDFill( VL_UNIT_ICMS_ST_CONV_COMPL, 6 ) +
         VDFill( VL_UNIT_FCP_ST_CONV_COMPL, 6 )
       ) ;
  end;

  RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
end;

procedure TBloco_C.WriteRegistroC890(RegC860: TRegistroC860);
var
  intFor: integer;
begin
  if Assigned( RegC860.RegistroC890 ) then
  begin
     for intFor := 0 to RegC860.RegistroC890.Count - 1 do
     begin
        with RegC860.RegistroC890.Items[intFor] do
        begin
          Add( LFill('C890') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;

        RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroC890Count := FRegistroC890Count + RegC860.RegistroC890.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC895(RegC860: TRegistroC860);
var
  intFor: Integer;
begin
  if (FBloco_0.Registro0000.COD_VER < vlVersao116) then
    Exit;

  if Assigned(RegC860.RegistroC895) then
  begin
    for intFor := 0 to RegC860.RegistroC895.Count - 1 do
    begin
      with RegC860.RegistroC895.Items[intFor] do
        Add(LFill('C895') +
            LFill(COD_OBS) +
            LFill(TXT_COMPL));

      WriteRegistroC897(RegC860.RegistroC895.Items[intFor]);
      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;

    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC895Count := FRegistroC895Count + RegC860.RegistroC895.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC897(RegC895: TRegistroC895);
var
  intFor: Integer;
begin
  if Assigned(RegC895.RegistroC897) then
  begin
    for intFor := 0 to RegC895.RegistroC897.Count - 1 do
    begin
      with RegC895.RegistroC897.Items[intFor] do
        Add(LFill('C897') +
            LFill(COD_AJ) +
            LFill(DESCR_COMPL_AJ) +
            LFill(COD_ITEM) +
            LFill(VL_BC_ICMS, 0, 2) +
            LFill(ALIQ_ICMS, 6, 2) +
            LFill(VL_ICMS, 0, 2) +
            LFill(VL_OUTROS, 0, 2));

      RegistroC990.QTD_LIN_C := RegistroC990.QTD_LIN_C + 1;
    end;

    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistroC897Count := FRegistroC897Count + RegC895.RegistroC897.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC990;
begin
  if Assigned(RegistroC990) then
  begin
     with RegistroC990 do
     begin
       QTD_LIN_C := QTD_LIN_C + 1;
       ///
       Add( LFill('C990') +
            LFill(QTD_LIN_C,0) ) ;
     end;
  end;
end;

end.
