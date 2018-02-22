{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEFDBloco_D_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_D,
     ACBrEFDBloco_0_Class, ACBrEFDBlocos;

type
  /// TBLOCO_D -
  TBloco_D = class(TACBrSPED)
  private
    FOnBeforeWriteRegistroD100: TWriteRegistroEvent;
    FOnBeforeWriteRegistroD110: TWriteRegistroEvent;
    FOnBeforeWriteRegistroD510: TWriteRegistroEvent;

    FOnWriteRegistroD100: TWriteRegistroEvent;
    FOnWriteRegistroD110: TWriteRegistroEvent;
    FOnWriteRegistroD510: TWriteRegistroEvent;

    FOnAfterWriteRegistroD100: TWriteRegistroEvent;
    FOnAfterWriteRegistroD110: TWriteRegistroEvent;
    FOnAfterWriteRegistroD510: TWriteRegistroEvent;

    FBloco_0: TBloco_0;

    FRegistroD001: TRegistroD001;      /// BLOCO D - RegistroD001
    FRegistroD990: TRegistroD990;      /// BLOCO D - RegistroD990

    FRegistroD100Count: integer;
    FRegistroD101Count: integer;
    FRegistroD110Count: integer;
    FRegistroD120Count: integer;
    FRegistroD130Count: integer;
    FRegistroD140Count: integer;
    FRegistroD150Count: integer;
    FRegistroD160Count: integer;
    FRegistroD161Count: integer;
    FRegistroD162Count: integer;
    FRegistroD170Count: integer;
    FRegistroD180Count: integer;
    FRegistroD190Count: integer;
    FRegistroD195Count: integer;
    FRegistroD197Count: integer;
    FRegistroD300Count: integer;
    FRegistroD301Count: integer;
    FRegistroD310Count: integer;
    FRegistroD350Count: integer;
    FRegistroD355Count: integer;
    FRegistroD360Count: integer;
    FRegistroD365Count: integer;
    FRegistroD370Count: integer;
    FRegistroD390Count: integer;
    FRegistroD400Count: integer;
    FRegistroD410Count: integer;
    FRegistroD411Count: integer;
    FRegistroD420Count: integer;
    FRegistroD500Count: integer;
    FRegistroD510Count: integer;
    FRegistroD530Count: integer;
    FRegistroD590Count: Integer;
    FRegistroD600Count: integer;
    FRegistroD610Count: integer;
    FRegistroD690Count: integer;
    FRegistroD695Count: integer;
    FRegistroD696Count: integer;
    FRegistroD697Count: integer;

    procedure WriteRegistroD100(RegD001: TRegistroD001) ;
    procedure WriteRegistroD101(RegD100: TRegistroD100) ; 
    procedure WriteRegistroD110(RegD100: TRegistroD100) ;
    procedure WriteRegistroD120(RegD110: TRegistroD110) ;
    procedure WriteRegistroD130(RegD100: TRegistroD100) ;
    procedure WriteRegistroD140(RegD100: TRegistroD100) ;
    procedure WriteRegistroD150(RegD100: TRegistroD100) ;
    procedure WriteRegistroD160(RegD100: TRegistroD100) ;
    procedure WriteRegistroD161(RegD160: TRegistroD160) ;
    procedure WriteRegistroD162(RegD160: TRegistroD160) ;
    procedure WriteRegistroD170(RegD100: TRegistroD100) ;
    procedure WriteRegistroD180(RegD100: TRegistroD100) ;
    procedure WriteRegistroD190(RegD100: TRegistroD100) ; 
    procedure WriteRegistroD195(RegD100: TRegistroD100) ;
    procedure WriteRegistroD197(RegD195: TRegistroD195) ;
    procedure WriteRegistroD300(RegD001: TRegistroD001) ;
    procedure WriteRegistroD301(RegD300: TRegistroD300) ;
    procedure WriteRegistroD310(RegD300: TRegistroD300) ;
    procedure WriteRegistroD350(RegD001: TRegistroD001) ;
    procedure WriteRegistroD355(RegD350: TRegistroD350) ;
    procedure WriteRegistroD360(RegD355: TRegistroD355) ;
    procedure WriteRegistroD365(RegD355: TRegistroD355) ;
    procedure WriteRegistroD370(RegD365: TRegistroD365) ;
    procedure WriteRegistroD390(RegD355: TRegistroD355) ;
    procedure WriteRegistroD400(RegD001: TRegistroD001) ;
    procedure WriteRegistroD410(RegD400: TRegistroD400) ;
    procedure WriteRegistroD411(RegD410: TRegistroD410) ;
    procedure WriteRegistroD420(RegD400: TRegistroD400) ;
    procedure WriteRegistroD500(RegD001: TRegistroD001) ;
    procedure WriteRegistroD510(RegD500: TRegistroD500) ;
    procedure WriteRegistroD530(RegD500: TRegistroD500) ;
    procedure WriteRegistroD590(RegD500: TRegistroD500) ; 
    procedure WriteRegistroD600(RegD001: TRegistroD001) ;
    procedure WriteRegistroD610(RegD600: TRegistroD600) ;
    procedure WriteRegistroD690(RegD600: TRegistroD600) ;
    procedure WriteRegistroD695(RegD001: TRegistroD001) ;
    procedure WriteRegistroD696(RegD695: TRegistroD695) ;
    procedure WriteRegistroD697(RegD696: TRegistroD696) ;

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function RegistroD001New: TRegistroD001;
    function RegistroD100New: TRegistroD100;
    function RegistroD101New: TRegistroD101;
    function RegistroD110New: TRegistroD110;
    function RegistroD120New: TRegistroD120;
    function RegistroD130New: TRegistroD130;
    function RegistroD140New: TRegistroD140;
    function RegistroD150New: TRegistroD150;
    function RegistroD160New: TRegistroD160;
    function RegistroD161New: TRegistroD161;
    function RegistroD162New: TRegistroD162;
    function RegistroD170New: TRegistroD170;
    function RegistroD180New: TRegistroD180;
    function RegistroD190New: TRegistroD190;
    function RegistroD195New: TRegistroD195;
    function RegistroD197New: TRegistroD197;
    function RegistroD300New: TRegistroD300;
    function RegistroD301New: TRegistroD301;
    function RegistroD310New: TRegistroD310;
    function RegistroD350New: TRegistroD350;
    function RegistroD355New: TRegistroD355;
    function RegistroD360New: TRegistroD360;
    function RegistroD365New: TRegistroD365;
    function RegistroD370New: TRegistroD370;
    function RegistroD390New: TRegistroD390;
    function RegistroD400New: TRegistroD400;
    function RegistroD410New: TRegistroD410;
    function RegistroD411New: TRegistroD411;
    function RegistroD420New: TRegistroD420;
    function RegistroD500New: TRegistroD500;
    function RegistroD510New: TRegistroD510;
    function RegistroD530New: TRegistroD530;
    function RegistroD590New: TRegistroD590;
    function RegistroD600New: TRegistroD600;
    function RegistroD610New: TRegistroD610;
    function RegistroD690New: TRegistroD690;
    function RegistroD695New: TRegistroD695;
    function RegistroD696New: TRegistroD696;
    function RegistroD697New: TRegistroD697;

    procedure WriteRegistroD001;
    procedure WriteRegistroD990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroD001: TRegistroD001 read FRegistroD001 write FRegistroD001;
    property RegistroD990: TRegistroD990 read FRegistroD990 write FRegistroD990;

    property RegistroD100Count: integer read FRegistroD100Count write FRegistroD100Count;
    property RegistroD101Count: integer read FRegistroD101Count write FRegistroD101Count;
    property RegistroD110Count: integer read FRegistroD110Count write FRegistroD110Count;
    property RegistroD120Count: integer read FRegistroD120Count write FRegistroD120Count;
    property RegistroD130Count: integer read FRegistroD130Count write FRegistroD130Count;
    property RegistroD140Count: integer read FRegistroD140Count write FRegistroD140Count;
    property RegistroD150Count: integer read FRegistroD150Count write FRegistroD150Count;
    property RegistroD160Count: integer read FRegistroD160Count write FRegistroD160Count;
    property RegistroD161Count: integer read FRegistroD161Count write FRegistroD161Count;
    property RegistroD162Count: integer read FRegistroD162Count write FRegistroD162Count;
    property RegistroD170Count: integer read FRegistroD170Count write FRegistroD170Count;
    property RegistroD180Count: integer read FRegistroD180Count write FRegistroD180Count;
    property RegistroD190Count: Integer read FRegistroD190Count write FRegistroD190Count; 
    property RegistroD195Count: Integer read FRegistroD195Count write FRegistroD195Count;
    property RegistroD197Count: Integer read FRegistroD197Count write FRegistroD197Count;
    property RegistroD300Count: integer read FRegistroD300Count write FRegistroD300Count;
    property RegistroD301Count: integer read FRegistroD301Count write FRegistroD301Count;
    property RegistroD310Count: integer read FRegistroD310Count write FRegistroD310Count;
    property RegistroD350Count: integer read FRegistroD350Count write FRegistroD350Count;
    property RegistroD355Count: integer read FRegistroD355Count write FRegistroD355Count;
    property RegistroD360Count: integer read FRegistroD360Count write FRegistroD360Count;
    property RegistroD365Count: integer read FRegistroD365Count write FRegistroD365Count;
    property RegistroD370Count: integer read FRegistroD370Count write FRegistroD370Count;
    property RegistroD390Count: integer read FRegistroD390Count write FRegistroD390Count;
    property RegistroD400Count: integer read FRegistroD400Count write FRegistroD400Count;
    property RegistroD410Count: integer read FRegistroD410Count write FRegistroD410Count;
    property RegistroD411Count: integer read FRegistroD411Count write FRegistroD411Count;
    property RegistroD420Count: integer read FRegistroD420Count write FRegistroD420Count;
    property RegistroD500Count: integer read FRegistroD500Count write FRegistroD500Count;
    property RegistroD510Count: integer read FRegistroD510Count write FRegistroD510Count;
    property RegistroD530Count: integer read FRegistroD530Count write FRegistroD530Count;
    property RegistroD590Count: Integer read FRegistroD590Count write FRegistroD590Count; 
    property RegistroD600Count: integer read FRegistroD600Count write FRegistroD600Count;
    property RegistroD610Count: integer read FRegistroD610Count write FRegistroD610Count;
    property RegistroD690Count: integer read FRegistroD690Count write FRegistroD690Count;
    property RegistroD695Count: integer read FRegistroD695Count write FRegistroD695Count;
    property RegistroD696Count: integer read FRegistroD696Count write FRegistroD696Count;
    property RegistroD697Count: integer read FRegistroD697Count write FRegistroD697Count;

    property OnBeforeWriteRegistroD100: TWriteRegistroEvent read FOnBeforeWriteRegistroD100 write FOnBeforeWriteRegistroD100;
    property OnBeforeWriteRegistroD110: TWriteRegistroEvent read FOnBeforeWriteRegistroD110 write FOnBeforeWriteRegistroD110;
    property OnBeforeWriteRegistroD510: TWriteRegistroEvent read FOnBeforeWriteRegistroD510 write FOnBeforeWriteRegistroD510;

    property OnWriteRegistroD100: TWriteRegistroEvent read FOnWriteRegistroD100 write FOnWriteRegistroD100;
    property OnWriteRegistroD110: TWriteRegistroEvent read FOnWriteRegistroD110 write FOnWriteRegistroD110;
    property OnWriteRegistroD510: TWriteRegistroEvent read FOnWriteRegistroD510 write FOnWriteRegistroD510;

    property OnAfterWriteRegistroD100: TWriteRegistroEvent read FOnAfterWriteRegistroD100 write FOnAfterWriteRegistroD100;
    property OnAfterWriteRegistroD110: TWriteRegistroEvent read FOnAfterWriteRegistroD110 write FOnAfterWriteRegistroD110;
    property OnAfterWriteRegistroD510: TWriteRegistroEvent read FOnAfterWriteRegistroD510 write FOnAfterWriteRegistroD510;
  end;

implementation

Uses ACBrUtil, StrUtils;

{ TBloco_D }

constructor TBloco_D.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_D.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_D.CriaRegistros;
begin
  FRegistroD001 := TRegistroD001.Create;
  FRegistroD990 := TRegistroD990.Create;

  FRegistroD100Count := 0;
  FRegistroD101Count := 0;
  FRegistroD110Count := 0;
  FRegistroD120Count := 0;
  FRegistroD130Count := 0;
  FRegistroD140Count := 0;
  FRegistroD150Count := 0;
  FRegistroD160Count := 0;
  FRegistroD161Count := 0;
  FRegistroD162Count := 0;
  FRegistroD170Count := 0;
  FRegistroD180Count := 0;
  FRegistroD190Count := 0; 
  FRegistroD195Count := 0;
  FRegistroD197Count := 0;
  FRegistroD300Count := 0;
  FRegistroD301Count := 0;
  FRegistroD310Count := 0;
  FRegistroD350Count := 0;
  FRegistroD355Count := 0;
  FRegistroD360Count := 0;
  FRegistroD365Count := 0;
  FRegistroD370Count := 0;
  FRegistroD390Count := 0;
  FRegistroD400Count := 0;
  FRegistroD410Count := 0;
  FRegistroD411Count := 0;
  FRegistroD420Count := 0;
  FRegistroD500Count := 0;
  FRegistroD510Count := 0;
  FRegistroD530Count := 0;
  FRegistroD590Count := 0; 
  FRegistroD600Count := 0;
  FRegistroD610Count := 0;
  FRegistroD690Count := 0;
  FRegistroD695Count := 0;
  FRegistroD696Count := 0;
  FRegistroD697Count := 0;

  FRegistroD990.QTD_LIN_D := 0;
end;

procedure TBloco_D.LiberaRegistros;
begin
  FRegistroD001.Free;
  FRegistroD990.Free;
end;

procedure TBloco_D.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_D.RegistroD001New: TRegistroD001;
begin
   Result := FRegistroD001;
end;

function TBloco_D.RegistroD100New: TRegistroD100;
begin
   Result := FRegistroD001.RegistroD100.New(FRegistroD001);
end;

// EC 87/2015 
function TBloco_D.RegistroD101New: TRegistroD101;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD101.New;
end;

function TBloco_D.RegistroD110New: TRegistroD110;
var
D100Count: integer;
D100: TRegistroD100;
begin
   D100Count := FRegistroD001.RegistroD100.Count -1;
   if D100Count = -1 then
      raise EACBrSPEDFiscalException.Create('O registro D110 deve ser filho do registro D100, e não existe nenhum D100 pai!');

   D100   := FRegistroD001.RegistroD100.Items[D100Count];
   Result := D100.RegistroD110.New(D100);
end;

function TBloco_D.RegistroD411New: TRegistroD411;
var
D400Count: integer;
D410Count: integer;
begin
   D400Count := FRegistroD001.RegistroD400.Count -1;
   D410Count := FRegistroD001.RegistroD400.Items[D400Count].RegistroD410.Count -1;
   //
   Result := FRegistroD001.RegistroD400.Items[D400Count].RegistroD410.Items[D410Count].RegistroD411.New;
end;

function TBloco_D.RegistroD120New: TRegistroD120;
var
D100Count: integer;
D110Count: integer;
begin
   D100Count := FRegistroD001.RegistroD100.Count -1;
   D110Count := FRegistroD001.RegistroD100.Items[D100Count].RegistroD110.Count -1;
   //
   Result := FRegistroD001.RegistroD100.Items[D100Count].RegistroD110.Items[D110Count].RegistroD120.New;
end;

function TBloco_D.RegistroD130New: TRegistroD130;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD130.New;
end;

function TBloco_D.RegistroD140New: TRegistroD140;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD140.New;
end;

function TBloco_D.RegistroD150New: TRegistroD150;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD150.New;
end;

function TBloco_D.RegistroD160New: TRegistroD160;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD160.New;
end;

function TBloco_D.RegistroD161New: TRegistroD161;
var
D100Count: integer;
D160Count: integer;
begin
   D100Count := FRegistroD001.RegistroD100.Count -1;
   D160Count := FRegistroD001.RegistroD100.Items[D100Count].RegistroD160.Count -1;
   //
   Result := FRegistroD001.RegistroD100.Items[D100Count].RegistroD160.Items[D160Count].RegistroD161.New;
end;

function TBloco_D.RegistroD162New: TRegistroD162;
var
D100Count: integer;
D160Count: integer;
begin
   D100Count := FRegistroD001.RegistroD100.Count -1;
   D160Count := FRegistroD001.RegistroD100.Items[D100Count].RegistroD160.Count -1;
   //
   Result := FRegistroD001.RegistroD100.Items[D100Count].RegistroD160.Items[D160Count].RegistroD162.New;
end;

function TBloco_D.RegistroD170New: TRegistroD170;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD170.New;
end;

function TBloco_D.RegistroD180New: TRegistroD180;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD180.New;
end;

function TBloco_D.RegistroD190New: TRegistroD190;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD190.New;
end;

function TBloco_D.RegistroD195New: TRegistroD195;
begin
   Result := FRegistroD001.RegistroD100.Items[FRegistroD001.RegistroD100.Count -1].RegistroD195.New;
end;

function TBloco_D.RegistroD197New: TRegistroD197;
var
C100Count: integer;
C195Count: integer;
begin
   C100Count := FRegistroD001.RegistroD100.Count -1;
   C195Count := FRegistroD001.RegistroD100.Items[C100Count].RegistroD195.Count -1;
   //
   Result := FRegistroD001.RegistroD100.Items[C100Count].RegistroD195.Items[C195Count].RegistroD197.New;
end;

function TBloco_D.RegistroD300New: TRegistroD300;
begin
   Result := FRegistroD001.RegistroD300.New;
end;

function TBloco_D.RegistroD301New: TRegistroD301;
begin
   Result := FRegistroD001.RegistroD300.Items[FRegistroD001.RegistroD300.Count -1].RegistroD301.New;
end;

function TBloco_D.RegistroD310New: TRegistroD310;
begin
   Result := FRegistroD001.RegistroD300.Items[FRegistroD001.RegistroD300.Count -1].RegistroD310.New;
end;

function TBloco_D.RegistroD350New: TRegistroD350;
begin
   Result := FRegistroD001.RegistroD350.New;
end;

function TBloco_D.RegistroD355New: TRegistroD355;
begin
   Result := FRegistroD001.RegistroD350.Items[FRegistroD001.RegistroD350.Count -1].RegistroD355.New;
end;

function TBloco_D.RegistroD360New: TRegistroD360;
var
D350Count: integer;
D355Count: integer;
begin
   D350Count := FRegistroD001.RegistroD350.Count -1;
   D355Count := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Count -1;
   //
   Result := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Items[D355Count].RegistroD360.New;
end;

function TBloco_D.RegistroD365New: TRegistroD365;
var
D350Count: integer;
D355Count: integer;
begin
   D350Count := FRegistroD001.RegistroD350.Count -1;
   D355Count := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Count -1;
   //
   Result := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Items[D355Count].RegistroD365.New;
end;

function TBloco_D.RegistroD370New: TRegistroD370;
var
D350Count: integer;
D355Count: integer;
D365Count: integer;
begin
   D350Count := FRegistroD001.RegistroD350.Count -1;
   D355Count := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Count -1;
   D365Count := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Items[D355Count].RegistroD365.Count -1;
   //
   Result := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Items[D355Count].RegistroD365.Items[D365Count].RegistroD370.New;
end;

function TBloco_D.RegistroD390New: TRegistroD390;
var
D350Count: integer;
D355Count: integer;
begin
   D350Count := FRegistroD001.RegistroD350.Count -1;
   D355Count := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Count -1;
   //
   Result := FRegistroD001.RegistroD350.Items[D350Count].RegistroD355.Items[D355Count].RegistroD390.New;
end;

function TBloco_D.RegistroD400New: TRegistroD400;
begin
   Result := FRegistroD001.RegistroD400.New;
end;

function TBloco_D.RegistroD410New: TRegistroD410;
begin
   Result := FRegistroD001.RegistroD400.Items[FRegistroD001.RegistroD400.Count -1].RegistroD410.New;
end;

function TBloco_D.RegistroD420New: TRegistroD420;
begin
   Result := FRegistroD001.RegistroD400.Items[FRegistroD001.RegistroD400.Count -1].RegistroD420.New;
end;

function TBloco_D.RegistroD500New: TRegistroD500;
begin
   Result := FRegistroD001.RegistroD500.New;
end;

function TBloco_D.RegistroD510New: TRegistroD510;
begin
   Result := FRegistroD001.RegistroD500.Items[FRegistroD001.RegistroD500.Count -1].RegistroD510.New;
end;

function TBloco_D.RegistroD530New: TRegistroD530;
begin
   Result := FRegistroD001.RegistroD500.Items[FRegistroD001.RegistroD500.Count -1].RegistroD530.New;
end;

function TBloco_D.RegistroD590New: TRegistroD590;
begin
   Result := FRegistroD001.RegistroD500.Items[FRegistroD001.RegistroD500.Count -1].RegistroD590.New;
end;

function TBloco_D.RegistroD600New: TRegistroD600;
begin
   Result := FRegistroD001.RegistroD600.New;
end;

function TBloco_D.RegistroD610New: TRegistroD610;
begin
   Result := FRegistroD001.RegistroD600.Items[FRegistroD001.RegistroD600.Count -1].RegistroD610.New;
end;

function TBloco_D.RegistroD690New: TRegistroD690;
begin
   Result := FRegistroD001.RegistroD600.Items[FRegistroD001.RegistroD600.Count -1].RegistroD690.New;
end;

function TBloco_D.RegistroD695New: TRegistroD695;
begin
   Result := FRegistroD001.RegistroD695.New;
end;

function TBloco_D.RegistroD696New: TRegistroD696;
begin
   Result := FRegistroD001.RegistroD695.Items[FRegistroD001.RegistroD695.Count -1].RegistroD696.New;
end;

function TBloco_D.RegistroD697New: TRegistroD697;
var
D695Count: integer;
D696Count: integer;
begin
   D695Count := FRegistroD001.RegistroD695.Count -1;
   D696Count := FRegistroD001.RegistroD695.Items[D695Count].RegistroD696.Count -1;
   //
   Result := FRegistroD001.RegistroD695.Items[D695Count].RegistroD696.Items[D696Count].RegistroD697.New;
end;

procedure TBloco_D.WriteRegistroD001 ;
begin
  if Assigned(FRegistroD001) then
  begin
     with FRegistroD001 do
     begin
       Add( LFill( 'D001' ) +
            LFill( Integer(IND_MOV), 0 ) ) ;

       if IND_MOV = imComDados then
       begin
         WriteRegistroD100 ( FRegistroD001 ) ;
         WriteRegistroD300 ( FRegistroD001 ) ;
         WriteRegistroD350 ( FRegistroD001 ) ;
         WriteRegistroD400 ( FRegistroD001 ) ;
         WriteRegistroD500 ( FRegistroD001 ) ;
         WriteRegistroD600 ( FRegistroD001 ) ;
         WriteRegistroD695 ( FRegistroD001 ) ;
       end;
     end;

     RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
  end;
end;

procedure TBloco_D.WriteRegistroD100(RegD001: TRegistroD001) ;
var
  intFor: integer;
  strIND_FRT: String;
  strCOD_SIT: String;
  booConsiderarComoValorNulo: Boolean;
  booConsiderarComoValorNuloParaInutilizado: Boolean;
  ChaveEletronicaCTe: string;
  strLinha: String;
begin
  if Assigned( RegD001.RegistroD100 ) then
  begin
     //-- Before
     strLinha := '';
     if Assigned(FOnBeforeWriteRegistroD100) then
     begin
        FOnBeforeWriteRegistroD100(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to RegD001.RegistroD100.Count - 1 do
     begin
        with RegD001.RegistroD100.Items[intFor] do
        begin
          if DT_INI < EncodeDate(2012,07,01) then
          begin
             case IND_FRT of
               tfPorContaTerceiros:                                   strIND_FRT := '0';
               tfPorContaEmitente, tfProprioPorContaRemetente:        strIND_FRT := '1';
               tfPorContaDestinatario, tfProprioPorContaDestinatario: strIND_FRT := '2';
               tfSemCobrancaFrete:                                    strIND_FRT := '9';
               tfNenhum:                                              strIND_FRT := '';
             end
          end
          else
          begin
             case IND_FRT of
               tfPorContaEmitente, tfProprioPorContaRemetente:        strIND_FRT := '0';
               tfPorContaDestinatario, tfProprioPorContaDestinatario: strIND_FRT := '1';
               tfPorContaTerceiros:                                   strIND_FRT := '2';
               tfSemCobrancaFrete:                                    strIND_FRT := '9';
               tfNenhum:                                              strIND_FRT := '';
             end;
          end;
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
          booConsiderarComoValorNulo := (strCOD_SIT = '02') or {Cancelado}
                                        (strCOD_SIT = '03') or {Cancelado extemporâneo}
                                        (strCOD_SIT = '04') or {Denegado}
                                        (strCOD_SIT = '05');   {Inutilizado}

          booConsiderarComoValorNuloParaInutilizado := (strCOD_SIT = '05');
          ChaveEletronicaCTe := IfThen(booConsiderarComoValorNuloParaInutilizado, '', CHV_CTE);

          strLinha := LFill('D100') +
                      LFill( Integer(IND_OPER), 0 ) +
                      LFill( Integer(IND_EMIT), 0 ) +
                      LFill( COD_PART ) +
                      LFill( COD_MOD ) +
                      LFill( strCOD_SIT ) +
                      LFill( SER ) +
                      LFill( SUB ) +
                      LFill( NUM_DOC ) +
                      LFill( ChaveEletronicaCTe ) +
                      LFill( DT_DOC ) +
                      LFill( DT_A_P ) +
                      LFill( TP_CT_e ) +
                      LFill( CHV_CTE_REF ) +
                      LFill( VL_DOC,0,2, booConsiderarComoValorNulo ) +
                      LFill( VL_DESC,0,2, booConsiderarComoValorNulo ) +
                      LFill( strIND_FRT ) +
                      LFill( VL_SERV,0,2, booConsiderarComoValorNulo ) +
                      LFill( VL_BC_ICMS,0,2, booConsiderarComoValorNulo ) +
                      LFill( VL_ICMS,0,2, booConsiderarComoValorNulo ) +
                      LFill( VL_NT,0,2, booConsiderarComoValorNulo ) +
                      LFill( COD_INF ) +
                      LFill( COD_CTA ) +
                      IfThen(DT_INI >= EncodeDate(2018,01,01),
                        LFill( COD_MUN_ORIG ) +
                        LFill( COD_MUN_DEST),EmptyStr);
          //-- Write
          if Assigned(FOnWriteRegistroD100) then
             FOnWriteRegistroD100(strLinha);

          Add(strLinha);
        end;
        /// Registros FILHOS
        WriteRegistroD110( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD101( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD130( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD140( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD150( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD160( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD170( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD180( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD190( RegD001.RegistroD100.Items[intFor] ) ;
        WriteRegistroD195( RegD001.RegistroD100.Items[intFor] ) ;

       RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(FOnAfterWriteRegistroD100) then
     begin
        FOnAfterWriteRegistroD100(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD100Count := FRegistroD100Count + RegD001.RegistroD100.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD101(RegD100: TRegistroD100) ;
var
  intFor: integer;
begin
  if Assigned( RegD100.RegistroD101 ) then
  begin
     for intFor := 0 to RegD100.RegistroD101.Count - 1 do
     begin
        with RegD100.RegistroD101.Items[intFor] do
        begin

          Add( LFill('D101') +
               LFill( VL_FCP_UF_DEST,0,2 ) +
               LFill( VL_ICMS_UF_DEST,0,2 ) +
               LFill( VL_ICMS_UF_REM,0,2 ) ) ;
        end;
       RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD101Count := FRegistroD101Count + RegD100.RegistroD101.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD110(RegD100: TRegistroD100) ;
var
  intFor: integer;
  strLinha: String;
begin
  if Assigned( RegD100.RegistroD110 ) then
  begin
     //-- Before
     strLinha := '';
     if Assigned(FOnBeforeWriteRegistroD110) then
     begin
        FOnBeforeWriteRegistroD110(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to RegD100.RegistroD110.Count - 1 do
     begin
        with RegD100.RegistroD110.Items[intFor] do
        begin
          strLinha := LFill('D110')        +
                      LFill(NUN_ITEM, 3)   +
                      LFill(COD_ITEM )     +
                      LFill(VL_SERV, 0, 2) +
                      LFill(VL_OUT, 0, 2);
          //-- Write
          if Assigned(FOnWriteRegistroD110) then
             FOnWriteRegistroD110(strLinha);

          Add(strLinha);
        end;
        /// Registros FILHOS
        WriteRegistroD120( RegD100.RegistroD110.Items[intFor] );

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(FOnAfterWriteRegistroD110) then
     begin
        FOnAfterWriteRegistroD110(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD110Count := FRegistroD110Count + RegD100.RegistroD110.Count;
  end;
end;


procedure TBloco_D.WriteRegistroD120(RegD110: TRegistroD110) ;
var
  intFor: integer;
begin
  if Assigned( RegD110.RegistroD120 ) then
  begin
     for intFor := 0 to RegD110.RegistroD120.Count - 1 do
     begin
        with RegD110.RegistroD120.Items[intFor] do
        begin
          Add( LFill('D120') +
               LFill( COD_MUN_ORIG ) +
               LFill( COD_MUN_DEST ) +
               LFill( VEIC_ID ) +
               LFill( UF_ID ) ) ;
        end;
       RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD120Count := FRegistroD120Count + RegD110.RegistroD120.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD130(RegD100: TRegistroD100) ;
var
  intFor: integer;
  strIND_FRT_RED: String;
begin
  if Assigned( RegD100.RegistroD130 ) then
  begin
     for intFor := 0 to RegD100.RegistroD130.Count - 1 do
     begin
        with RegD100.RegistroD130.Items[intFor] do
        begin
          case IND_FRT_RED of
            frSemRedespacho:        strIND_FRT_RED := '0';
            frPorContaEmitente:     strIND_FRT_RED := '1';
            frPorContaDestinatario: strIND_FRT_RED := '2';
            frOutros:               strIND_FRT_RED := '9';
            frNenhum:               strIND_FRT_RED := '';
          end;

          Add( LFill('D130') +
               LFill( COD_PART_CONSG ) +
               LFill( COD_PART_RED ) +
               LFill( strIND_FRT_RED ) +
               LFill( COD_MUN_ORIG ) +
               LFill( COD_MUN_DEST ) +
               LFill( VEIC_ID ) +
               LFill( VL_LIQ_FRT,0,2 ) +
               LFill( VL_SEC_CAT,0,2 ) +
               LFill( VL_DESP,0,2 ) +
               LFill( VL_PEDG,0,2 ) +
               LFill( VL_OUT,0,2 ) +
               LFill( VL_FRT,0,2 ) +
               LFill( UF_ID ) ) ;
        end;
       RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD130Count := FRegistroD130Count + RegD100.RegistroD130.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD140(RegD100: TRegistroD100) ;
var
  intFor: integer;
begin
  if Assigned( RegD100.RegistroD140 ) then
  begin
     for intFor := 0 to RegD100.RegistroD140.Count - 1 do
     begin
        with RegD100.RegistroD140.Items[intFor] do
        begin
          Add( LFill('D140') +
               LFill( COD_PART_CONSG ) +
               LFill( COD_MUN_ORIG ) +
               LFill( COD_MUN_DEST ) +
               LFill( IND_VEIC ) +
               LFill( Integer(VEIC_ID), 0 ) +
               LFill( Integer(IND_NAV), 0 ) +
               LFill( VIAGEM ) +
               LFill( VL_FRT_LIQ,0,2 ) +
               LFill( VL_DESP_PORT,0,2 ) +
               LFill( VL_DESP_CAR_DESC,0,2 ) +
               LFill( VL_OUT,0,2 ) +
               LFill( VL_FRT_BRT,0,2 ) +
               LFill( VL_FRT_MM,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD140Count := FRegistroD140Count + RegD100.RegistroD140.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD150(RegD100: TRegistroD100) ;
var
  intFor: integer;
  intIND_TFA: integer;
begin
  if Assigned( RegD100.RegistroD150 ) then
  begin
     for intFor := 0 to RegD100.RegistroD150.Count - 1 do
     begin
        with RegD100.RegistroD150.Items[intFor] do
        begin
          case IND_TFA of
            tipExp:   intIND_TFA := 0;
            tipEnc:   intIND_TFA := 1;
            tipCI:    intIND_TFA := 2;
            tipOutra: intIND_TFA := 9;
            else      intIND_TFA := 9;
          end;

          Add( LFill('D150') +
               LFill( COD_MUN_ORIG ) +
               LFill( COD_MUN_DEST ) +
               LFill( VEIC_ID ) +
               LFill( VIAGEM ) +
               LFill( intIND_TFA, 0 ) +
               LFill( VL_PESO_TX,0,2 ) +
               LFill( VL_TX_TERR,0,2 ) +
               LFill( VL_TX_RED,0,2 ) +
               LFill( VL_OUT,0,2 ) +
               LFill( VL_TX_ADV,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD150Count := FRegistroD150Count + RegD100.RegistroD150.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD160(RegD100: TRegistroD100) ;
var
  intFor: integer;
begin
  if Assigned( RegD100.RegistroD160 ) then
  begin
     for intFor := 0 to RegD100.RegistroD160.Count - 1 do
     begin
        with RegD100.RegistroD160.Items[intFor] do
        begin
          Add( LFill('D160') +
               LFill( DESPACHO ) +
               LFill( CNPJ_CPF_REM ) +
               LFill( IE_REM ) +
               LFill( COD_MUN_ORI ) +
               LFill( CNPJ_CPF_DEST ) +
               LFill( IE_DEST ) +
               LFill( COD_MUN_DEST ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD161( RegD100.RegistroD160.Items[intFor] ) ;
        WriteRegistroD162( RegD100.RegistroD160.Items[intFor] ) ;

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD160Count := FRegistroD160Count + RegD100.RegistroD160.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD161(RegD160: TRegistroD160) ;
var
  intFor: integer;
  intIND_CARGA: integer;
begin
  if Assigned( RegD160.RegistroD161 ) then
  begin
     for intFor := 0 to RegD160.RegistroD161.Count - 1 do
     begin
        with RegD160.RegistroD161.Items[intFor] do
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
          Add( LFill('D161') +
               LFill( intIND_CARGA, 0 ) +
               LFill( CNPJ_COL ) +
               LFill( IE_COL ) +
               LFill( COD_MUN_COL ) +
               LFill( CNPJ_ENTG ) +
               LFill( IE_ENTG ) +
               LFill( COD_MUN_ENTG ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD161Count := FRegistroD161Count + RegD160.RegistroD161.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD162(RegD160: TRegistroD160) ;
var
  intFor: integer;
begin
  if Assigned( RegD160.RegistroD162 ) then
  begin
     for intFor := 0 to RegD160.RegistroD162.Count - 1 do
     begin
        with RegD160.RegistroD162.Items[intFor] do
        begin
          Add( LFill('D162') +
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( VL_DOC,0,2 ) +
               LFill( VL_MERC,0,2 ) +
               LFill( QTD_VOL, 0 ) +
               LFill( PESO_BRT,0,2 ) +
               LFill( PESO_LIQ,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD162Count := FRegistroD162Count + RegD160.RegistroD162.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD170(RegD100: TRegistroD100) ;
var
  intFor: integer;
begin
  if Assigned( RegD100.RegistroD170 ) then
  begin
     for intFor := 0 to RegD100.RegistroD170.Count - 1 do
     begin
        with RegD100.RegistroD170.Items[intFor] do
        begin
          Add( LFill('D170') +
               LFill( COD_PART_CONSG ) +
               LFill( COD_PART_RED ) +
               LFill( COD_MUN_ORIG ) +
               LFill( COD_MUN_DEST ) +
               LFill( OTM ) +
               LFill( Integer(IND_NAT_FRT), 0 ) +
               LFill( VL_LIQ_FRT,0,2 ) +
               LFill( VL_GRIS,0,2 ) +
               LFill( VL_PDG,0,2 ) +
               LFill( VL_OUT,0,2 ) +
               LFill( VL_FRT,0,2 ) +
               LFill( VL_FRT,0,2 ) +
               LFill( UF_ID ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD170Count := FRegistroD170Count + RegD100.RegistroD170.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD180(RegD100: TRegistroD100) ;
var
  intFor: integer;
begin
  if Assigned( RegD100.RegistroD180 ) then
  begin
     for intFor := 0 to RegD100.RegistroD180.Count - 1 do
     begin
        with RegD100.RegistroD180.Items[intFor] do
        begin
          Add( LFill('D180') +
               LFill( NUM_SEQ ) +
               LFill( Integer(IND_EMIT), 0 ) +
               LFill( CNPJ_EMIT ) +
               LFill( UF_EMIT ) +
               LFill( IE_EMIT ) +
               LFill( COD_MUN_ORIG ) +
               LFill( CNPJ_CPF_TOM ) +
               LFill( UF_TOM ) +
               LFill( IE_TOM ) +
               LFill( COD_MUN_DEST ) +
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( VL_DOC,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD180Count := FRegistroD180Count + RegD100.RegistroD180.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD190(RegD100:TRegistroD100) ;
var
  intFor: integer;
begin
  if Assigned( RegD100.RegistroD190 ) then
  begin
     for intFor := 0 to RegD100.RegistroD190.Count - 1 do
     begin
        with RegD100.RegistroD190.Items[intFor] do
        begin
          Add( LFill('D190') +
               LFill( CST_ICMS ) +
               LFill( CFOP ) +
               LFill( ALIQ_ICMS,0,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD190Count := FRegistroD190Count + RegD100.RegistroD190.Count;
  end;
end;


procedure TBloco_D.WriteRegistroD195(RegD100: TRegistroD100);
var
  intFor: integer;
begin
  if Assigned( RegD100.RegistroD195 ) then
  begin
     for intFor := 0 to RegD100.RegistroD195.Count - 1 do
     begin
        with RegD100.RegistroD195.Items[intFor] do
        begin
          Add( LFill('D195') +
               LFill( COD_OBS ) +
               LFill( TXT_COMPL ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistroD197( RegD100.RegistroD195.Items[intFor] );

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD195Count := FRegistroD195Count + RegD100.RegistroD195.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD197(RegD195: TRegistroD195);
var
  intFor: integer;
begin
  if Assigned(RegD195.RegistroD197 ) then
  begin
     for intFor := 0 to RegD195.RegistroD197.Count - 1 do
     begin
        with RegD195.RegistroD197.Items[intFor] do
        begin
          Add( LFill('D197') +
               LFill( COD_AJ ) +
               LFill( DESCR_COMPL_AJ ) +
               LFill( COD_ITEM ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_OUTROS,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD197Count := FRegistroD197Count + RegD195.RegistroD197.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD300(RegD001: TRegistroD001) ;
var
  intFor: integer;
begin
  if Assigned( RegD001.RegistroD300 ) then
  begin
     for intFor := 0 to RegD001.RegistroD300.Count - 1 do
     begin
        with RegD001.RegistroD300.Items[intFor] do
        begin
          Add( LFill('D300') +
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( NUM_DOC_INI ) +
               LFill( NUM_DOC_FIN ) +
               LFill( CST_ICMS ) +
               LFill( CFOP ) +
               LFill( ALIQ_ICMS,0,2 ) +
               LFill( DT_DOC ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_SERV,0,2 ) +
               LFill( VL_SEG,0,2 ) +
               LFill( VL_OUT_DESP,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS ) +
               LFill( COD_CTA ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD301( RegD001.RegistroD300.Items[intFor] ) ;
        WriteRegistroD310( RegD001.RegistroD300.Items[intFor] ) ;

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD300Count := FRegistroD300Count + RegD001.RegistroD300.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD301(RegD300: TRegistroD300) ;
var
  intFor: integer;
begin
  if Assigned( RegD300.RegistroD301 ) then
  begin
     for intFor := 0 to RegD300.RegistroD301.Count - 1 do
     begin
        with RegD300.RegistroD301.Items[intFor] do
        begin
          Add( LFill('D301') +
               LFill( NUM_DOC_CANC ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD301Count := FRegistroD301Count + RegD300.RegistroD301.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD310(RegD300: TRegistroD300) ;
var
  intFor: integer;
begin
  if Assigned( RegD300.RegistroD310 ) then
  begin
     for intFor := 0 to RegD300.RegistroD310.Count - 1 do
     begin
        with RegD300.RegistroD310.Items[intFor] do
        begin
          Add( LFill('D310') +
               LFill( COD_MUN_ORIG ) +
               LFill( VL_SERV,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD310Count := FRegistroD310Count + RegD300.RegistroD310.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD350(RegD001: TRegistroD001) ;
var
  intFor: integer;
begin
  if Assigned( RegD001.RegistroD350 ) then
  begin
     for intFor := 0 to RegD001.RegistroD350.Count - 1 do
     begin
        with RegD001.RegistroD350.Items[intFor] do
        begin
          Add( LFill('D350') +
               LFill( COD_MOD ) +
               LFill( ECF_MOD ) +
               LFill( ECF_FAB ) +
               LFill( ECF_CX ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD355( RegD001.RegistroD350.Items[intFor] );

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD350Count := FRegistroD350Count + RegD001.RegistroD350.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD355(RegD350: TRegistroD350) ;
var
  intFor: integer;
begin
  if Assigned( RegD350.RegistroD355 ) then
  begin
     for intFor := 0 to RegD350.RegistroD355.Count - 1 do
     begin
        with RegD350.RegistroD355.Items[intFor] do
        begin
          Add( LFill('D355') +
               LFill( DT_DOC ) +
               LFill( CRO, 3 ) +
               LFill( CRZ, 6 ) +
               LFill( NUM_COO_FIN, 6 ) +
               LFill( GT_FIN,0,2 ) +
               LFill( VL_BRT,0,2 ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD360( RegD350.RegistroD355.Items[intFor] ) ;
        WriteRegistroD365( RegD350.RegistroD355.Items[intFor] ) ;
        WriteRegistroD390( RegD350.RegistroD355.Items[intFor] ) ;

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD355Count := FRegistroD355Count + RegD350.RegistroD355.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD360(RegD355: TRegistroD355) ;
var
  intFor: integer;
begin
  if Assigned( RegD355.RegistroD360 ) then
  begin
     for intFor := 0 to RegD355.RegistroD360.Count - 1 do
     begin
        with RegD355.RegistroD360.Items[intFor] do
        begin
          Add( LFill('D360') +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD360Count := FRegistroD360Count + RegD355.RegistroD360.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD365(RegD355: TRegistroD355) ;
var
  intFor: integer;
begin
  if Assigned( RegD355.RegistroD365 ) then
  begin
     for intFor := 0 to RegD355.RegistroD365.Count - 1 do
     begin
        with RegD355.RegistroD365.Items[intFor] do
        begin
          Add( LFill('D365') +
               LFill( COD_TOT_PAR ) +
               LFill( VLR_ACUM_TOT,0,2 ) +
               LFill( NR_TOT ) +
               LFill( DESCR_NR_TOT ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD370( RegD355.RegistroD365.Items[intFor] );

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD365Count := FRegistroD365Count + RegD355.RegistroD365.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD370(RegD365: TRegistroD365) ;
var
  intFor: integer;
begin
  if Assigned( RegD365.RegistroD370 ) then
  begin
     for intFor := 0 to RegD365.RegistroD370.Count - 1 do
     begin
        with RegD365.RegistroD370.Items[intFor] do
        begin
          Add( LFill('D370') +
               LFill( COD_MUN_ORIG ) +
               LFill( VL_SERV,0,2 ) +
               LFill( QTD_BILH,0 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD370Count := FRegistroD370Count + RegD365.RegistroD370.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD390(RegD355: TRegistroD355) ;
var
  intFor: integer;
begin
  if Assigned( RegD355.RegistroD390 ) then
  begin
     for intFor := 0 to RegD355.RegistroD390.Count - 1 do
     begin
        with RegD355.RegistroD390.Items[intFor] do
        begin
          Add( LFill('D390') +
               LFill( CST_ICMS ) +
               LFill( CFOP ) +
               LFill( ALIQ_ICMS,0,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ISSQN,0,2 ) +
               LFill( ALIQ_ISSQN,0,2 ) +
               LFill( VL_ISSQN,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( COD_OBS ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD390Count := FRegistroD390Count + RegD355.RegistroD390.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD400(RegD001: TRegistroD001) ;
var
  intFor: integer;
  strCOD_SIT: String;
begin
  if Assigned( RegD001.RegistroD400 ) then
  begin
     for intFor := 0 to RegD001.RegistroD400.Count - 1 do
     begin
        with RegD001.RegistroD400.Items[intFor] do
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

          Add( LFill('D400') +
               LFill( COD_PART ) +
               LFill( COD_MOD ) +
               LFill( strCOD_SIT ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( VL_DOC,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_SERV,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( COD_CTA ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD410( RegD001.RegistroD400.Items[intFor] ) ;
        WriteRegistroD420( RegD001.RegistroD400.Items[intFor] ) ;

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD400Count := FRegistroD400Count + RegD001.RegistroD400.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD410(RegD400: TRegistroD400) ;
var
  intFor: integer;
begin
  if Assigned( RegD400.RegistroD410 ) then
  begin
     for intFor := 0 to RegD400.RegistroD410.Count - 1 do
     begin
        with RegD400.RegistroD410.Items[intFor] do
        begin
          Add( LFill('D410') +
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( NUM_DOC_INI ) +
               LFill( NUM_DOC_FIN ) +
               LFill( DT_DOC ) +
               LFill( CST_ICMS ) +
               LFill( CFOP ) +
               LFill( ALIQ_ICMS,0,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_SERV,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD411( RegD400.RegistroD410.Items[intFor] );

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD410Count := FRegistroD410Count + RegD400.RegistroD410.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD411(RegD410: TRegistroD410) ;
var
  intFor: integer;
begin
  if Assigned( RegD410.RegistroD411 ) then
  begin
     for intFor := 0 to RegD410.RegistroD411.Count - 1 do
     begin
        with RegD410.RegistroD411.Items[intFor] do
        begin
          Add( LFill('D411') +
               LFill( NUM_DOC_CANC ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD411Count := FRegistroD411Count + RegD410.RegistroD411.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD420(RegD400: TRegistroD400) ;
var
  intFor: integer;
begin
  if Assigned( RegD400.RegistroD420 ) then
  begin
     for intFor := 0 to RegD400.RegistroD420.Count - 1 do
     begin
        with RegD400.RegistroD420.Items[intFor] do
        begin
          Add( LFill('D420') +
               LFill( COD_MUN_ORIG ) +
               LFill( VL_SERV,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD420Count := FRegistroD420Count + RegD400.RegistroD420.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD500(RegD001: TRegistroD001) ;
var
  intFor: integer;
  intTP_ASSINANTE: integer;
  strCOD_SIT: String;
  booConsiderarComoValorNulo: Boolean;
begin
  if Assigned( RegD001.RegistroD500 ) then
  begin
     for intFor := 0 to RegD001.RegistroD500.Count - 1 do
     begin
        with RegD001.RegistroD500.Items[intFor] do
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
          case TP_ASSINANTE of
            assComercialIndustrial: intTP_ASSINANTE := 1;
            assPodrPublico:         intTP_ASSINANTE := 2;
            assResidencial:         intTP_ASSINANTE := 3;
            assPublico:             intTP_ASSINANTE := 4;
            assSemiPublico:         intTP_ASSINANTE := 5;
            assOutros:              intTP_ASSINANTE := 6;
            assNenhum:              intTP_ASSINANTE := 0; //Preencher vazio  
            else                    intTP_ASSINANTE := 0; //Preencher vazio
          end;    
          booConsiderarComoValorNulo := (strCOD_SIT = '02') or {Cancelado}
                                        (strCOD_SIT = '03') or {Cancelado extemporâneo}
                                        (strCOD_SIT = '04') or {Denegado}
                                        (strCOD_SIT = '05');   {Inutilizado}

          Add( LFill('D500') +
               LFill( Integer(IND_OPER), 0 ) +
               LFill( Integer(IND_EMIT), 0 ) + 
               LFill( COD_PART,0, booConsiderarComoValorNulo ) +
               LFill( COD_MOD ) +
               LFill( strCOD_SIT ) +
               LFill( SER ) +   
               LFill( SUB,0, booConsiderarComoValorNulo ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( DT_A_P ) +
               LFill( VL_DOC,0,2, booConsiderarComoValorNulo ) +
               LFill( VL_DESC,0,2, booConsiderarComoValorNulo ) +
               LFill( VL_SERV,0,2, booConsiderarComoValorNulo ) +  
               LFill( VL_SERV_NT,0,2, booConsiderarComoValorNulo ) +  
               LFill( VL_TERC,0,2, booConsiderarComoValorNulo ) +  
               LFill( VL_DA,0,2, booConsiderarComoValorNulo ) +
               LFill( VL_BC_ICMS,0,2, booConsiderarComoValorNulo ) +
               LFill( VL_ICMS,0,2, booConsiderarComoValorNulo ) +
               LFill( COD_INF,0, booConsiderarComoValorNulo ) +
               LFill( VL_PIS,0,2, True ) +
               LFill( VL_COFINS,0,2, True ) +
               LFill( COD_CTA,0, booConsiderarComoValorNulo ) +
               LFill( intTP_ASSINANTE, 0, booConsiderarComoValorNulo ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD510( RegD001.RegistroD500.Items[intFor] ) ;
        WriteRegistroD530( RegD001.RegistroD500.Items[intFor] ) ;
        WriteRegistroD590( RegD001.RegistroD500.Items[intFor] ) ;

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD500Count := FRegistroD500Count + RegD001.RegistroD500.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD510(RegD500: TRegistroD500) ;
var
  intFor: integer;
  intIND_REC: integer;
  strLinha: String;
begin
  if Assigned( RegD500.RegistroD510 ) then
  begin
     //-- Before
     strLinha := '';
     if Assigned(FOnBeforeWriteRegistroD510) then
     begin
        FOnBeforeWriteRegistroD510(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     for intFor := 0 to RegD500.RegistroD510.Count - 1 do
     begin
        with RegD500.RegistroD510.Items[intFor] do
        begin
          case IND_REC of
            recServicoPrestado:       intIND_REC := 0;
            recCobrancaDebitos:       intIND_REC := 1;
            recVendaMerc:             intIND_REC := 2;
            recServicoPrePago:        intIND_REC := 3;
            recOutrasProprias:        intIND_REC := 4;
            recTerceiroCoFaturamento: intIND_REC := 5;
            recTerceiroOutras:        intIND_REC := 9;
            else                      intIND_REC := 0;
          end;

          strLinha := LFill('D510') +
                      lFill( NUM_ITEM ) +
                      lFill( COD_ITEM ) +
                      lFill( COD_CLASS ) +
                      lFill( QTD, 0, 3) +
                      lFill( UNID ) +
                      lFill( VL_ITEM, 0, 2) +
                      lFill( VL_DESC, 0, 2) +
                      lFill( CST_ICMS, 3 ) +
                      lFill( CFOP, 4 ) +
                      lFill( VL_BC_ICMS, 0, 2) +
                      lFill( ALIQ_ICMS, 0, 2) +
                      lFill( VL_ICMS, 0, 2) +
                      lFill( VL_BC_ICMS_UF, 0, 2) +
                      lFill( VL_ICMS_UF, 0, 2) +
                      lFill( intIND_REC, 0 ) +
                      lFill( COD_PART ) +
                      lFill( VL_PIS, 0, 2) +
                      lFill( VL_COFINS, 0, 2) +
                      lFill( COD_CTA );
          //-- Write
          if Assigned(FOnWriteRegistroD510) then
             FOnWriteRegistroD510(strLinha);

          Add(strLinha);
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     //-- After
     strLinha := '';
     if Assigned(FOnAfterWriteRegistroD510) then
     begin
        FOnAfterWriteRegistroD510(strLinha);
        if strLinha <> EmptyStr then
           Add(strLinha);
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD510Count := FRegistroD510Count + RegD500.RegistroD510.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD530(RegD500: TRegistroD500) ;
var
  intFor: integer;
  intIND_SERV: integer;
begin
  if Assigned( RegD500.RegistroD530 ) then
  begin
     for intFor := 0 to RegD500.RegistroD530.Count - 1 do
     begin
        with RegD500.RegistroD530.Items[intFor] do
        begin
          case IND_SERV of
            spTelefonia:        intIND_SERV := 0;
            spComunicacaoDados: intIND_SERV := 1;
            spTVAssinatura:     intIND_SERV := 2;
            spAcessoInternet:   intIND_SERV := 3;
            spMultimidia:       intIND_SERV := 4;
            spOutros:           intIND_SERV := 9;
            else                intIND_SERV := 9;
          end;

          Add( LFill( 'D530' ) +
               LFill( intIND_SERV, 0 ) +
               LFill( DT_INI_SERV ) +
               LFill( DT_FIN_SERV ) +
               LFill( PER_FISCAL ) +
               LFill( COD_AREA ) +
               LFill( TERMINAL ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD530Count := FRegistroD530Count + RegD500.RegistroD530.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD590(RegD500: TRegistroD500) ;
var
  intFor: integer;
begin
  if Assigned( RegD500.RegistroD590 ) then
  begin
     for intFor := 0 to RegD500.RegistroD590.Count - 1 do
     begin
        with RegD500.RegistroD590.Items[intFor] do
        begin
          Add( LFill('D590') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_UF,0,2 ) +
               LFill( VL_ICMS_UF,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS  ) ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD590Count := FRegistroD590Count + RegD500.RegistroD590.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD600(RegD001: TRegistroD001) ;
var
  intFor: integer;
begin
  if Assigned( RegD001.RegistroD600 ) then
  begin
     if RegD001.RegistroD600.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           raise EACBrSPEDFiscalException.Create(ACBrStr('O RegistroD600, não deve ser gerado em movimentações de entrada nem saída, no PerfilA'));
     end;
     for intFor := 0 to RegD001.RegistroD600.Count - 1 do
     begin
        with RegD001.RegistroD600.Items[intFor] do
        begin
          Add( LFill('D600') ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD610( RegD001.RegistroD600.Items[intFor] ) ;
        WriteRegistroD690( RegD001.RegistroD600.Items[intFor] ) ;

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD600Count := FRegistroD600Count + RegD001.RegistroD600.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD610(RegD600: TRegistroD600) ;
var
  intFor: integer;
begin
  if Assigned( RegD600.RegistroD610 ) then
  begin
     if RegD600.RegistroD610.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           raise EACBrSPEDFiscalException.Create(ACBrStr('O RegistroD610, não deve ser gerado em movimentações de entrada nem saída, no PerfilA'));
     end;
     for intFor := 0 to RegD600.RegistroD610.Count - 1 do
     begin
        with RegD600.RegistroD610.Items[intFor] do
        begin
          Add( LFill('D610') ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD610Count := FRegistroD610Count + RegD600.RegistroD610.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD690(RegD600: TRegistroD600) ;
var
  intFor: integer;
begin
  if Assigned( RegD600.RegistroD690 ) then
  begin
     if RegD600.RegistroD690.Count > 0 then
     begin
        if FBloco_0.Registro0000.IND_PERFIL in [pfPerfilA] then
           raise EACBrSPEDFiscalException.Create(ACBrStr('O RegistroD690, não deve ser gerado em movimentações de entrada nem saída, no PerfilA'));
     end;
     for intFor := 0 to RegD600.RegistroD690.Count - 1 do
     begin
        with RegD600.RegistroD690.Items[intFor] do
        begin
          Add( LFill('D690') ) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD690Count := FRegistroD690Count + RegD600.RegistroD690.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD695(RegD001: TRegistroD001) ;
var
  intFor: integer;
begin
  if Assigned( RegD001.RegistroD695 ) then
  begin
     for intFor := 0 to RegD001.RegistroD695.Count - 1 do
     begin
        with RegD001.RegistroD695.Items[intFor] do
        begin
          Add( LFill('D695')+
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( NRO_ORD_INI,9 ) +
               LFill( NRO_ORD_FIN,9 ) +
               LFill( DT_DOC_INI ) +
               LFill( DT_DOC_FIN ) +
               LFill( NOM_MEST ) +
               LFill( CHV_COD_DIG )
              );
        end;
        /// Registros FILHOS
        WriteRegistroD696( RegD001.RegistroD695.Items[intFor] );

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD695Count := FRegistroD695Count + RegD001.RegistroD695.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD696(RegD695: TRegistroD695) ;
var
  intFor: integer;
begin
  if Assigned( RegD695.RegistroD696 ) then
  begin
     for intFor := 0 to RegD695.RegistroD696.Count - 1 do
     begin
        with RegD695.RegistroD696.Items[intFor] do
        begin
          Add( LFill('D696') +
               LFill( CST_ICMS,3 ) +
               LFill( CFOP,4 ) +
               LFill( ALIQ_ICMS,6,2 ) +
               LFill( VL_OPR,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_UF,0,2 ) +
               LFill( VL_ICMS_UF,0,2 ) +
               LFill( VL_RED_BC,0,2 ) +
               LFill( COD_OBS  ) ) ;
        end;
        /// Registros FILHOS
        WriteRegistroD697( RegD695.RegistroD696.Items[intFor] );

        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD696Count := FRegistroD696Count + RegD695.RegistroD696.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD697(RegD696: TRegistroD696) ;
var
  intFor: integer;
begin
  if Assigned( RegD696.RegistroD697 ) then
  begin
     for intFor := 0 to RegD696.RegistroD697.Count - 1 do
     begin
        with RegD696.RegistroD697.Items[intFor] do
        begin
          Add( LFill('D697') +
               LFill( UF,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 )) ;
        end;
        RegistroD990.QTD_LIN_D := RegistroD990.QTD_LIN_D + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroD697Count := FRegistroD697Count + RegD696.RegistroD697.Count;
  end;
end;

procedure TBloco_D.WriteRegistroD990 ;
begin
  if Assigned(RegistroD990) then
  begin
     with RegistroD990 do
     begin
       QTD_LIN_D := QTD_LIN_D + 1;
       ///
       Add( LFill('D990') +
            LFill(QTD_LIN_D,0) ) ;
     end;
  end;
end;

end.
