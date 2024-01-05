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

unit ACBrEFDBloco_1_Class;

interface

uses SysUtils, StrUtils, Classes, DateUtils, ACBrSped, ACBrEFDBloco_1,
     ACBrEFDBloco_0_Class, ACBrEFDBlocos;

type
  /// TBLOCO_1 -
  TBloco_1 = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;

    FRegistro1001: TRegistro1001;      /// BLOCO 1 - Registro1001
    FRegistro1990: TRegistro1990;      /// BLOCO 1 - Registro1990

    FRegistro1010Count: Integer;
    FRegistro1100Count: Integer;
    FRegistro1105Count: Integer;
    FRegistro1110Count: Integer;
    FRegistro1200Count: Integer;
    FRegistro1210Count: Integer;
    FRegistro1250Count: Integer;
    FRegistro1255Count: Integer;
    FRegistro1300Count: Integer;
    FRegistro1310Count: Integer;
    FRegistro1320Count: Integer;
    FRegistro1350Count: Integer;
    FRegistro1360Count: Integer;
    FRegistro1370Count: Integer;
    FRegistro1390Count: Integer;
    FRegistro1391Count: Integer;
    FRegistro1400Count: Integer;
    FRegistro1500Count: Integer;
    FRegistro1510Count: Integer;
    FRegistro1600Count: Integer;
    FRegistro1601Count: Integer;
    FRegistro1700Count: Integer;
    FRegistro1710Count: Integer;
    FRegistro1800Count: Integer;
    FRegistro1900Count: Integer;
    FRegistro1910Count: Integer;
    FRegistro1920Count: Integer;
    FRegistro1921Count: Integer;
    FRegistro1922Count: Integer;
    FRegistro1923Count: Integer;
    FRegistro1925Count: Integer;
    FRegistro1926Count: Integer;
    FRegistro1960Count: Integer;
    FRegistro1970Count: Integer;
    FRegistro1975Count: Integer;
    FRegistro1980Count: Integer;

    procedure WriteRegistro1010(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1100(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1105(Reg1100: TRegistro1100) ;
    procedure WriteRegistro1110(Reg1105: TRegistro1105) ;
    procedure WriteRegistro1200(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1210(Reg1200: TRegistro1200) ;
    procedure WriteRegistro1250(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1255(Reg1250: TRegistro1250) ;
    procedure WriteRegistro1300(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1310(Reg1300: TRegistro1300) ;
    procedure WriteRegistro1320(Reg1310: TRegistro1310) ;
    procedure WriteRegistro1350(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1360(Reg1350: TRegistro1350) ;
    procedure WriteRegistro1370(Reg1350: TRegistro1350) ;
    procedure WriteRegistro1390(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1391(Reg1390: TRegistro1390) ;
    procedure WriteRegistro1400(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1500(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1510(Reg1500: TRegistro1500) ;
    procedure WriteRegistro1600(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1601(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1700(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1710(Reg1700: TRegistro1700) ;
    procedure WriteRegistro1800(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1900(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1910(Reg1900: TRegistro1900) ;
    procedure WriteRegistro1920(Reg1910: TRegistro1910) ;
    procedure WriteRegistro1921(Reg1920: TRegistro1920) ;
    procedure WriteRegistro1922(Reg1921: TRegistro1921) ;
    procedure WriteRegistro1923(Reg1921: TRegistro1921) ;
    procedure WriteRegistro1925(Reg1920: TRegistro1920) ;
    procedure WriteRegistro1926(Reg1920: TRegistro1920) ;
    procedure WriteRegistro1960(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1970(Reg1001: TRegistro1001) ;
    procedure WriteRegistro1975(Reg1970: TRegistro1970) ;
    procedure WriteRegistro1980(Reg1001: TRegistro1001) ;


    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;           /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function Registro1001New: TRegistro1001;
    function Registro1010New: TRegistro1010;
    function Registro1100New: TRegistro1100;
    function Registro1105New: TRegistro1105;
    function Registro1110New: TRegistro1110;
    function Registro1200New: TRegistro1200;
    function Registro1210New: TRegistro1210;
    function Registro1250New: TRegistro1250;
    function Registro1255New: TRegistro1255;
    function Registro1300New: TRegistro1300;
    function Registro1310New: TRegistro1310;
    function Registro1320New: TRegistro1320;
    function Registro1350New: TRegistro1350;
    function Registro1360New: TRegistro1360;
    function Registro1370New: TRegistro1370;
    function Registro1390New: TRegistro1390;
    function Registro1391New: TRegistro1391;
    function Registro1400New: TRegistro1400;
    function Registro1500New: TRegistro1500;
    function Registro1510New: TRegistro1510;
    function Registro1600New: TRegistro1600;
    function Registro1601New: TRegistro1601;
    function Registro1700New: TRegistro1700;
    function Registro1710New: TRegistro1710;
    function Registro1800New: TRegistro1800;
    function Registro1900New: TRegistro1900;
    function Registro1910New: TRegistro1910;
    function Registro1920New: TRegistro1920;
    function Registro1921New: TRegistro1921;
    function Registro1922New: TRegistro1922;
    function Registro1923New: TRegistro1923;
    function Registro1925New: TRegistro1925;
    function Registro1926New: TRegistro1926;
    function Registro1960New: TRegistro1960;
    function Registro1970New: TRegistro1970;
    function Registro1975New: TRegistro1975;
    function Registro1980New: TRegistro1980;

    procedure WriteRegistro1001 ;
    procedure WriteRegistro1990 ;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property Registro1001: TRegistro1001 read FRegistro1001 write FRegistro1001;
    property Registro1990: TRegistro1990 read FRegistro1990 write FRegistro1990;

    property Registro1010Count: Integer read FRegistro1010Count write FRegistro1010Count;
    property Registro1100Count: Integer read FRegistro1100Count write FRegistro1100Count;
    property Registro1105Count: Integer read FRegistro1105Count write FRegistro1105Count;
    property Registro1110Count: Integer read FRegistro1110Count write FRegistro1110Count;
    property Registro1200Count: Integer read FRegistro1200Count write FRegistro1200Count;
    property Registro1210Count: Integer read FRegistro1210Count write FRegistro1210Count;
    property Registro1250Count: Integer read FRegistro1250Count write FRegistro1250Count;
    property Registro1255Count: Integer read FRegistro1255Count write FRegistro1255Count;
    property Registro1300Count: Integer read FRegistro1300Count write FRegistro1300Count;
    property Registro1310Count: Integer read FRegistro1310Count write FRegistro1310Count;
    property Registro1320Count: Integer read FRegistro1320Count write FRegistro1320Count;
    property Registro1350Count: Integer read FRegistro1350Count write FRegistro1350Count;
    property Registro1360Count: Integer read FRegistro1360Count write FRegistro1360Count;
    property Registro1370Count: Integer read FRegistro1370Count write FRegistro1370Count;
    property Registro1390Count: Integer read FRegistro1390Count write FRegistro1390Count;
    property Registro1391Count: Integer read FRegistro1391Count write FRegistro1391Count;
    property Registro1400Count: Integer read FRegistro1400Count write FRegistro1400Count;
    property Registro1500Count: Integer read FRegistro1500Count write FRegistro1500Count;
    property Registro1510Count: Integer read FRegistro1510Count write FRegistro1510Count;
    property Registro1600Count: Integer read FRegistro1600Count write FRegistro1600Count;
    property Registro1601Count: Integer read FRegistro1601Count write FRegistro1601Count;
    property Registro1700Count: Integer read FRegistro1700Count write FRegistro1700Count;
    property Registro1710Count: Integer read FRegistro1710Count write FRegistro1710Count;
    property Registro1800Count: Integer read FRegistro1800Count write FRegistro1800Count;
    property Registro1900Count: Integer read FRegistro1900Count write FRegistro1900Count;
    property Registro1910Count: Integer read FRegistro1910Count write FRegistro1910Count;
    property Registro1920Count: Integer read FRegistro1920Count write FRegistro1920Count;
    property Registro1921Count: Integer read FRegistro1921Count write FRegistro1921Count;
    property Registro1922Count: Integer read FRegistro1922Count write FRegistro1922Count;
    property Registro1923Count: Integer read FRegistro1923Count write FRegistro1923Count;
    property Registro1925Count: Integer read FRegistro1925Count write FRegistro1925Count;
    property Registro1926Count: Integer read FRegistro1926Count write FRegistro1926Count;
    property Registro1960Count: Integer read FRegistro1960Count write FRegistro1960Count;
    property Registro1970Count: Integer read FRegistro1970Count write FRegistro1970Count;
    property Registro1975Count: Integer read FRegistro1975Count write FRegistro1975Count;
    property Registro1980Count: Integer read FRegistro1980Count write FRegistro1980Count;
  end;

implementation

  uses
     ACBrUtil.Strings;

{ TBloco_1 }

constructor TBloco_1.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_1.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_1.CriaRegistros;
begin
  FRegistro1001 := TRegistro1001.Create;
  FRegistro1990 := TRegistro1990.Create;

  Registro1010Count := 0;
  Registro1100Count := 0;
  Registro1105Count := 0;
  Registro1110Count := 0;
  Registro1200Count := 0;
  Registro1210Count := 0;
  Registro1250Count := 0;
  Registro1255Count := 0;
  Registro1300Count := 0;
  Registro1310Count := 0;
  Registro1320Count := 0;
  Registro1350Count := 0;
  Registro1360Count := 0;
  Registro1370Count := 0;
  Registro1390Count := 0;
  Registro1391Count := 0;
  Registro1400Count := 0;
  Registro1500Count := 0;
  Registro1510Count := 0;
  Registro1600Count := 0;
  Registro1601Count := 0;
  Registro1700Count := 0;
  Registro1710Count := 0;
  Registro1800Count := 0;
  Registro1900Count := 0;
  Registro1910Count := 0;
  Registro1920Count := 0;
  Registro1921Count := 0;
  Registro1922Count := 0;
  Registro1923Count := 0;
  Registro1925Count := 0;
  Registro1926Count := 0;
  Registro1960Count := 0;
  Registro1970Count := 0;
  Registro1975Count := 0;
  Registro1980Count := 0;

  FRegistro1990.QTD_LIN_1 := 0;
end;

procedure TBloco_1.LiberaRegistros;
begin
  FRegistro1001.Free;
  FRegistro1990.Free;
end;

procedure TBloco_1.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_1.Registro1001New: TRegistro1001;
begin
   Result := FRegistro1001;
end;

function TBloco_1.Registro1100New: TRegistro1100;
begin
   Result := FRegistro1001.Registro1100.New();
end;

function TBloco_1.Registro1010New: TRegistro1010;
begin
   Result := FRegistro1001.Registro1010.New(FRegistro1001);
end;


function TBloco_1.Registro1105New: TRegistro1105;
var
U1100: TRegistro1100;
U1100Count: Integer;
begin
   U1100Count := FRegistro1001.Registro1100.Count -1;
   if U1100Count = -1 then
      raise Exception.Create('O registro 1105 deve ser filho do registro 1100, e não existe nenhum 1100 pai!');

   U1100  := FRegistro1001.Registro1100.Items[U1100Count];
   Result := U1100.Registro1105.New();
end;

function TBloco_1.Registro1110New: TRegistro1110;
var
U1105: TRegistro1105;
U1100Count: integer;
U1105Count: integer;
begin
   U1100Count := FRegistro1001.Registro1100.Count -1;
   U1105Count := FRegistro1001.Registro1100.Items[U1100Count].Registro1105.Count -1;
   if U1105Count = -1 then
      raise Exception.Create('O registro 1110 deve ser filho do registro 1105, e não existe nenhum 1105 pai!');

   U1105  := FRegistro1001.Registro1100.Items[U1100Count].Registro1105.Items[U1105Count];
   Result := U1105.Registro1110.New(U1105);
end;

function TBloco_1.Registro1200New: TRegistro1200;
begin
   Result := FRegistro1001.Registro1200.New();
end;

function TBloco_1.Registro1210New: TRegistro1210;
var
U1200: TRegistro1200;
U1200Count: Integer;
begin
   U1200Count := FRegistro1001.Registro1200.Count -1;
   if U1200Count = -1 then
      raise Exception.Create('O registro 1210 deve ser filho do registro 1200, e não existe nenhum 1200 pai!');

   U1200  := FRegistro1001.Registro1200.Items[U1200Count];
   Result := U1200.Registro1210.New(U1200);
end;

function TBloco_1.Registro1250New: TRegistro1250;
begin
   Result := FRegistro1001.Registro1250.New();
end;

function TBloco_1.Registro1255New: TRegistro1255;
var
U1250: TRegistro1250;
U1250Count: Integer;
begin
   U1250Count := FRegistro1001.Registro1250.Count -1;
   if U1250Count = -1 then
      raise Exception.Create('O registro 1255 deve ser filho do registro 1250, e não existe nenhum 1250 pai!');

   U1250  := FRegistro1001.Registro1250.Items[U1250Count];
   Result := U1250.Registro1255.New(U1250);
end;

function TBloco_1.Registro1300New: TRegistro1300;
begin
   Result := FRegistro1001.Registro1300.New();
end;

function TBloco_1.Registro1310New: TRegistro1310;
var
U1300: TRegistro1300;
U1300Count: Integer;
begin
   U1300Count := FRegistro1001.Registro1300.Count -1;
   if U1300Count = -1 then
      raise Exception.Create('O registro 1310 deve ser filho do registro 1300, e não existe nenhum 1300 pai!');

   U1300  := FRegistro1001.Registro1300.Items[U1300Count];
   Result := U1300.Registro1310.New();
end;

function TBloco_1.Registro1320New: TRegistro1320;
var
U1310: TRegistro1310;
U1300Count: integer;
U1310Count: integer;
begin
   U1300Count := FRegistro1001.Registro1300.Count -1;
   U1310Count := FRegistro1001.Registro1300.Items[U1300Count].Registro1310.Count -1;
   if U1310Count = -1 then
      raise Exception.Create('O registro 1320 deve ser filho do registro 1310, e não existe nenhum 1310 pai!');

   U1310  := FRegistro1001.Registro1300.Items[U1300Count].Registro1310.Items[U1310Count];
   Result := U1310.Registro1320.New(U1310);
end;

function TBloco_1.Registro1350New: TRegistro1350;
begin
   Result := FRegistro1001.Registro1350.New();
end;

function TBloco_1.Registro1360New: TRegistro1360;
var
U1350: TRegistro1350;
U1350Count: Integer;
begin
   U1350Count := FRegistro1001.Registro1350.Count -1;
   if U1350Count = -1 then
      raise Exception.Create('O registro 1360 deve ser filho do registro 1350, e não existe nenhum 1350 pai!');

   U1350  := FRegistro1001.Registro1350.Items[U1350Count];
   Result := U1350.Registro1360.New(U1350);
end;

function TBloco_1.Registro1370New: TRegistro1370;
var
U1350: TRegistro1350;
U1350Count: Integer;
begin
   U1350Count := FRegistro1001.Registro1350.Count -1;
   if U1350Count = -1 then
      raise Exception.Create('O registro 1370 deve ser filho do registro 1350, e não existe nenhum 1350 pai!');

   U1350  := FRegistro1001.Registro1350.Items[U1350Count];
   Result := U1350.Registro1370.New(U1350);
end;

function TBloco_1.Registro1390New: TRegistro1390;
begin
   Result := FRegistro1001.Registro1390.New();
end;

function TBloco_1.Registro1391New: TRegistro1391;
var U1390: TRegistro1390;
    U1390Count: Integer;
begin
   U1390Count := FRegistro1001.Registro1390.Count -1;
   if U1390Count = -1 then
      raise Exception.Create('O registro 1391 deve ser filho do registro 1390, e não existe nenhum 1390 pai!');
   U1390  := FRegistro1001.Registro1390.Items[U1390Count];
   Result := U1390.Registro1391.New(U1390);
end;

function TBloco_1.Registro1400New: TRegistro1400;
begin
   Result := FRegistro1001.Registro1400.New(FRegistro1001);
end;

function TBloco_1.Registro1500New: TRegistro1500;
begin
   Result := FRegistro1001.Registro1500.New();
end;

function TBloco_1.Registro1510New: TRegistro1510;
var
U1500: TRegistro1500;
U1500Count: Integer;
begin
   U1500Count := FRegistro1001.Registro1500.Count -1;
   if U1500Count = -1 then
      raise Exception.Create('O registro 1510 deve ser filho do registro 1500, e não existe nenhum 1500 pai!');

   U1500  := FRegistro1001.Registro1500.Items[U1500Count];
   Result := U1500.Registro1510.New(U1500);
end;

function TBloco_1.Registro1600New: TRegistro1600;
begin
   Result := FRegistro1001.Registro1600.New(FRegistro1001);
end;

function TBloco_1.Registro1601New: TRegistro1601;
begin
   Result := FRegistro1001.Registro1601.New(FRegistro1001);
end;

function TBloco_1.Registro1700New: TRegistro1700;
begin
   Result := FRegistro1001.Registro1700.New();
end;

function TBloco_1.Registro1710New: TRegistro1710;
var
U1700: TRegistro1700;
U1700Count: Integer;
begin
   U1700Count := FRegistro1001.Registro1700.Count -1;
   if U1700Count = -1 then
      raise Exception.Create('O registro 1710 deve ser filho do registro 1700, e não existe nenhum 1700 pai!');

   U1700  := FRegistro1001.Registro1700.Items[U1700Count];
   Result := U1700.Registro1710.New(U1700);
end;

function TBloco_1.Registro1800New: TRegistro1800;
begin
   Result := FRegistro1001.Registro1800.New(FRegistro1001);
end;

procedure TBloco_1.WriteRegistro1001 ;
begin
  if Assigned(Registro1001) then
  begin
     with Registro1001 do
     begin
       Add( LFill( '1001' ) +
            LFill( Integer(IND_MOV), 0 ) ) ;

       if IND_MOV = imComDados then
       begin
         WriteRegistro1010(Registro1001) ;
         WriteRegistro1100(Registro1001) ;
         WriteRegistro1200(Registro1001) ;
         WriteRegistro1250(Registro1001) ;
         WriteRegistro1300(Registro1001) ;
         WriteRegistro1350(Registro1001) ;
         WriteRegistro1390(Registro1001) ;
         WriteRegistro1400(Registro1001) ;
         WriteRegistro1500(Registro1001) ;
         WriteRegistro1600(Registro1001) ;
         WriteRegistro1601(Registro1001) ;
         WriteRegistro1700(Registro1001) ;
         WriteRegistro1800(Registro1001) ;
         WriteRegistro1900(Registro1001) ;
         if (FBloco_0.Registro0000.COD_VER >= vlVersao112) and
            (FBloco_0.Registro0000.UF = 'PE') then
         begin
           WriteRegistro1960(Registro1001) ;
           WriteRegistro1970(Registro1001) ;
           WriteRegistro1980(Registro1001) ;
         end;
       end;
     end;

     Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
  end;
end;

procedure TBloco_1.WriteRegistro1010(Reg1001: TRegistro1001) ;
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1010 ) then
  begin
    for intFor := 0 to Reg1001.Registro1010.Count - 1 do
    begin
      with Reg1001.Registro1010.Items[intFor] do
      begin
        if FBloco_0.Registro0000.COD_VER >= vlVersao113 then
        begin
          // A partir de 01/01/2020
          Add( LFill('1010') +
               LFill( IND_EXP ) +
               LFill( IND_CCRF ) +
               LFill( IND_COMB  ) +
               LFill( IND_USINA ) +
               LFill( IND_VA  ) +
               LFill( IND_EE ) +
               LFill( IND_CART  ) +
               LFill( IND_FORM  ) +
               LFill( IND_AER ) +
               LFill( IND_GIAF1 ) +
               LFill( IND_GIAF3 ) +
               LFill( IND_GIAF4 ) +
               LFill( IND_REST_RESSARC_COMPL_ICMS )
             ) ;
        end
        else
        begin
          Add( LFill('1010') +
               LFill( IND_EXP ) +
               LFill( IND_CCRF ) +
               LFill( IND_COMB  ) +
               LFill( IND_USINA ) +
               LFill( IND_VA  ) +
               LFill( IND_EE ) +
               LFill( IND_CART  ) +
               LFill( IND_FORM  ) +
               LFill( IND_AER )+
               ifthen(FBloco_0.Registro0000.COD_VER >= vlVersao112,
               LFill( IND_GIAF1 )+
               LFill( IND_GIAF3 )+
               LFill( IND_GIAF4 ),'') ) ;
        end;
         Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
        end;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1010Count := FRegistro1010Count + Reg1001.Registro1010.Count;
  end;
end;


procedure TBloco_1.WriteRegistro1100(Reg1001: TRegistro1001) ;
var
  intFor: integer;
  strTP_CHC: String;
begin
  if Assigned( Reg1001.Registro1100 ) then
  begin
     for intFor := 0 to Reg1001.Registro1100.Count - 1 do
     begin
        with Reg1001.Registro1100.Items[intFor] do
        begin
          case TP_CHC of
           ceAWB:            strTP_CHC := '01';
           ceMAWB:           strTP_CHC := '02';
           ceHAWB:           strTP_CHC := '03';
           ceCOMAT:          strTP_CHC := '04';
           ceRExpressas:     strTP_CHC := '06';
           ceEtiqREspressas: strTP_CHC := '07';
           ceHrExpressas:    strTP_CHC := '08';
           ceAV7:            strTP_CHC := '09';
           ceBL:             strTP_CHC := '10';
           ceMBL:            strTP_CHC := '11';
           ceHBL:            strTP_CHC := '12';
           ceCTR:            strTP_CHC := '13';
           ceDSIC:           strTP_CHC := '14';
           ceComatBL:        strTP_CHC := '16';
           ceRWB:            strTP_CHC := '17';
           ceHRWB:           strTP_CHC := '18';
           ceTifDta:         strTP_CHC := '19';
           ceCP2:            strTP_CHC := '20';
           ceNaoIATA:        strTP_CHC := '91';
           ceMNaoIATA:       strTP_CHC := '92';
           ceHNaoIATA:       strTP_CHC := '93';
           ceCOutros:        strTP_CHC := '99';
          end;

          Add( LFill('1100') +
               LFill( Integer(IND_DOC), 0 ) +
               LFill( NRO_DE ) +
               LFill( DT_DE ) +
               LFill( Integer(NAT_EXP), 0 ) +
               LFill( NRO_RE ) +
               LFill( DT_RE ) +
               LFill( CHC_EMB ) +
               LFill( DT_CHC ) +
               LFill( DT_AVB ) +
               LFill( strTP_CHC ) +
               LFill( PAIS ) ) ;
        end;
        // Registros - FILHO
        WriteRegistro1105( Reg1001.Registro1100.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1100Count := FRegistro1100Count + Reg1001.Registro1100.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1105(Reg1100: TRegistro1100) ;
var
  intFor: integer;
begin
  if Assigned( Reg1100.Registro1105 ) then
  begin
     for intFor := 0 to Reg1100.Registro1105.Count - 1 do
     begin
        with Reg1100.Registro1105.Items[intFor] do
        begin
          Add( LFill('1105') +
               LFill( COD_MOD ) +
               LFill( SERIE ) +
               LFill( NUM_DOC ) +
               LFill( CHV_NFE ) +
               LFill( DT_DOC ) +
               LFill( COD_ITEM ) ) ;
        end;
        // Registros - FILHO
        WriteRegistro1110( Reg1100.Registro1105.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1105Count := FRegistro1105Count + Reg1100.Registro1105.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1110(Reg1105: TRegistro1105) ;
var
  intFor: integer;
begin
  if Assigned( Reg1105.Registro1110 ) then
  begin
     for intFor := 0 to Reg1105.Registro1110.Count - 1 do
     begin
        with Reg1105.Registro1110.Items[intFor] do
        begin
          Add( LFill('1110') +
               LFill( COD_PART ) +
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( CHV_NFE ) +
               LFill( NR_MEMO ) +
               DFill( QTD,3 ) +
               LFill( UNID ) ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1110Count := FRegistro1110Count + Reg1105.Registro1110.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1200(Reg1001: TRegistro1001) ;
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1200 ) then
  begin
     for intFor := 0 to Reg1001.Registro1200.Count - 1 do
     begin
        with Reg1001.Registro1200.Items[intFor] do
        begin
          Add( LFill('1200') +
               LFill( COD_AJ_APUR ) +
               LFill( SLD_CRED,0 ) +
               LFill( CRED_APR,0 ) +
               LFill( CRED_RECEB,0 ) +
               LFill( CRED_UTIL,0 ) +
               LFill( SLD_CRED_FIM,0 ) ) ;
        end;
        // Registros - FILHO
        WriteRegistro1210( Reg1001.Registro1200.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1200Count := FRegistro1200Count + Reg1001.Registro1200.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1210(Reg1200: TRegistro1200) ;
var
  intFor: integer;
begin
  if Assigned( Reg1200.Registro1210 ) then
  begin
     for intFor := 0 to Reg1200.Registro1210.Count - 1 do
     begin
        with Reg1200.Registro1210.Items[intFor] do
        begin
          Add( LFill('1210') +
               LFill( TIPO_UTIL ) +
               LFill( NR_DOC ) +
               LFill( VL_CRED_UTIL,0 ) +
               LFill( CHV_DOCe )
               ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1210Count := FRegistro1210Count + Reg1200.Registro1210.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1250(Reg1001: TRegistro1001);
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1250) then
  begin
     for intFor := 0 to Reg1001.Registro1250.Count - 1 do
     begin
        with Reg1001.Registro1250.Items[intFor] do
        begin
          Add( LFill('1250') +
               LFill( VL_CREDITO_ICMS_OP,0,2 ) +
               LFill( VL_ICMS_ST_REST,0,2 ) +
               LFill( VL_FCP_ST_REST,0,2 ) +
               LFill( VL_ICMS_ST_COMPL,0,2 ) +
               LFill( VL_FCP_ST_COMPL,0,2 ));
        end;
        // Registros - FILHO
        WriteRegistro1255( Reg1001.Registro1250.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1250Count := FRegistro1250Count + Reg1001.Registro1250.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1255(Reg1250: TRegistro1250);
var
  intFor: integer;
begin
  if Assigned( Reg1250.Registro1255 ) then
  begin
     for intFor := 0 to Reg1250.Registro1255.Count - 1 do
     begin
        with Reg1250.Registro1255.Items[intFor] do
        begin
          Add( LFill('1255') +
               LFill( COD_MOT_REST_COMPL ) +
               LFill( VL_CREDITO_ICMS_OP_MOT,0,2 ) +
               LFill( VL_ICMS_ST_REST_MOT,0,2 ) +
               LFill( VL_FCP_ST_REST_MOT,0,2 ) +
               LFill( VL_ICMS_ST_COMPL_MOT,0,2 ) +
               LFill( VL_FCP_ST_COMPL_MOT,0,2 ) );
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1255Count := FRegistro1255Count + Reg1250.Registro1255.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1300(Reg1001: TRegistro1001) ;
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1300 ) then
  begin
     for intFor := 0 to Reg1001.Registro1300.Count - 1 do
     begin
        with Reg1001.Registro1300.Items[intFor] do
        begin
          Add( LFill('1300') +
               LFill( COD_ITEM ) +
               LFill( DT_FECH ) +
               DFill( ESTQ_ABERT,3 ) +
               DFill( VOL_ENTR,3 ) +
               DFill( VOL_DISP,3 ) +
               DFill( VOL_SAIDAS,3 ) +
               DFill( ESTQ_ESCR,3 ) +
               DFill( VAL_AJ_PERDA,3 ) +
               DFill( VAL_AJ_GANHO,3 ) +
               DFill( FECH_FISICO,3 ) ) ;
        end;
        /// Registro FILHOS
        WriteRegistro1310( Reg1001.Registro1300.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1300Count := FRegistro1300Count + Reg1001.Registro1300.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1310(Reg1300: TRegistro1300) ;
var
  intFor: integer;
begin
  if Assigned( Reg1300.Registro1310 ) then
  begin
     for intFor := 0 to Reg1300.Registro1310.Count - 1 do
     begin
        with Reg1300.Registro1310.Items[intFor] do
        begin
          Add( LFill('1310') +
               LFill( NUM_TANQUE ) +
               DFill( ESTQ_ABERT,3 ) +
               DFill( VOL_ENTR,3 ) +
               DFill( VOL_DISP,3 ) +
               DFill( VOL_SAIDAS,3 ) +
               DFill( ESTQ_ESCR,3 ) +
               DFill( VAL_AJ_PERDA,3 ) +
               DFill( VAL_AJ_GANHO,3 ) +
               DFill( FECH_FISICO,3 ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistro1320( Reg1300.Registro1310.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1310Count := FRegistro1310Count + Reg1300.Registro1310.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1320(Reg1310: TRegistro1310) ;
var
  intFor: integer;
begin
  if Assigned( Reg1310.Registro1320 ) then
  begin
     for intFor := 0 to Reg1310.Registro1320.Count - 1 do
     begin
        with Reg1310.Registro1320.Items[intFor] do
        begin
          Add( LFill('1320') +
               LFill( NUM_BICO ) +
               LFill( NR_INTERV ) +
               LFill( MOT_INTERV ) +
               LFill( NOM_INTERV ) +
               LFill( CNPJ_INTERV ) +
               LFill( CPF_INTERV ) +
               DFill( VAL_FECHA,3 ) +
               DFill( VAL_ABERT,3 ) +
               DFill( VOL_AFERI,3 ) +
               DFill( VOL_VENDAS,3 ) ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1320Count := FRegistro1320Count + Reg1310.Registro1320.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1350(Reg1001: TRegistro1001) ;
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1350 ) then
  begin
     for intFor := 0 to Reg1001.Registro1350.Count - 1 do
     begin
        with Reg1001.Registro1350.Items[intFor] do
        begin
          Add( LFill('1350') +
               LFill( SERIE ) +
               LFill( FABRICANTE ) +
               LFill( MODELO ) +
               LFill( Integer(TIPO_MEDICAO), 0 ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistro1360( Reg1001.Registro1350.Items[intFor] ) ;
        WriteRegistro1370( Reg1001.Registro1350.Items[intFor] ) ;

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1350Count := FRegistro1350Count + Reg1001.Registro1350.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1360(Reg1350: TRegistro1350) ;
var
  intFor: integer;
begin
  if Assigned( Reg1350.Registro1360 ) then
  begin
     for intFor := 0 to Reg1350.Registro1360.Count - 1 do
     begin
        with Reg1350.Registro1360.Items[intFor] do
        begin
          Add( LFill('1360') +
               LFill( NUM_LACRE ) +
               LFill( DT_APLICACAO ) ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1360Count := FRegistro1360Count + Reg1350.Registro1360.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1370(Reg1350: TRegistro1350) ;
var
  intFor: integer;
begin
  if Assigned( Reg1350.Registro1370 ) then
  begin
     for intFor := 0 to Reg1350.Registro1370.Count - 1 do
     begin
        with Reg1350.Registro1370.Items[intFor] do
        begin
          Add( LFill('1370') +
               LFill( NUM_BICO ) +
               LFill( COD_ITEM ) +
               LFill( NUM_TANQUE ) ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1370Count := FRegistro1370Count + Reg1350.Registro1370.Count;
  end;
end;


procedure TBloco_1.WriteRegistro1390(Reg1001: TRegistro1001) ;
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1390 ) then
  begin
     for intFor := 0 to Reg1001.Registro1390.Count - 1 do
     begin
        with Reg1001.Registro1390.Items[intFor] do
        begin
          Add( LFill('1390') +
               LFill( COD_PROD ) ) ;
        end;
        /// Registro FILHOS do FILHO
        WriteRegistro1391( Reg1001.Registro1390.Items[intFor] ) ;

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1390Count := FRegistro1390Count + Reg1001.Registro1390.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1391(Reg1390: TRegistro1390) ;
var
  intFor: integer;
  vReg1391: TRegistro1391;
begin
  if Assigned( Reg1390.Registro1391 ) then
  begin
    for intFor := 0 to Reg1390.Registro1391.Count - 1 do
    begin
      vReg1391 := Reg1390.Registro1391.Items[intFor];
      if FBloco_0.Registro0000.COD_VER >= vlVersao117 then
      begin
        // A partir de 01/01/2024
        Add( LFill('1391') +
             LFill( vReg1391.DT_REGISTRO       ) +
             LFill( vReg1391.QTD_MOID     ,0,2 ) +
             LFill( vReg1391.ESTQ_INI     ,0,2 ) +
             LFill( vReg1391.QTD_PRODUZ   ,0,2 ) +
             LFill( vReg1391.ENT_ANID_HID ,0,2 ) +
             LFill( vReg1391.OUTR_ENTR    ,0,2 ) +
             LFill( vReg1391.PERDA        ,0,2 ) +
             LFill( vReg1391.CONS         ,0,2 ) +
             LFill( vReg1391.SAI_ANI_HID  ,0,2 ) +
             LFill( vReg1391.SAIDAS       ,0,2 ) +
             LFill( vReg1391.ESTQ_FIN     ,0,2 ) +
             LFill( vReg1391.ESTQ_INI_MEL ,0,2 ) +
             LFill( vReg1391.PROD_DIA_MEL ,0,2 ) +
             LFill( vReg1391.UTIL_MEL     ,0,2 ) +
             LFill( vReg1391.PROD_ALC_MEL ,0,2 ) +
             LFill( vReg1391.OBS          ) +
             LFill( vReg1391.COD_ITEM ) +
             LFill( vReg1391.TP_RESIDUO ) +
             LFill( vReg1391.QTD_RESIDUO , 0, 2) +
             LFill( vReg1391.QTD_RESIDUO_DDG , 0, 2) +
             LFill( vReg1391.QTD_RESIDUO_WDG , 0, 2) +
             LFill( vReg1391.QTD_RESIDUO_CANA , 0, 2)
           ) ;
      end
      else
      if FBloco_0.Registro0000.COD_VER >= vlVersao113 then
      begin
        // A partir de 01/01/2020
        Add( LFill('1391') +
             LFill( vReg1391.DT_REGISTRO       ) +
             LFill( vReg1391.QTD_MOID     ,0,2 ) +
             LFill( vReg1391.ESTQ_INI     ,0,2 ) +
             LFill( vReg1391.QTD_PRODUZ   ,0,2 ) +
             LFill( vReg1391.ENT_ANID_HID ,0,2 ) +
             LFill( vReg1391.OUTR_ENTR    ,0,2 ) +
             LFill( vReg1391.PERDA        ,0,2 ) +
             LFill( vReg1391.CONS         ,0,2 ) +
             LFill( vReg1391.SAI_ANI_HID  ,0,2 ) +
             LFill( vReg1391.SAIDAS       ,0,2 ) +
             LFill( vReg1391.ESTQ_FIN     ,0,2 ) +
             LFill( vReg1391.ESTQ_INI_MEL ,0,2 ) +
             LFill( vReg1391.PROD_DIA_MEL ,0,2 ) +
             LFill( vReg1391.UTIL_MEL     ,0,2 ) +
             LFill( vReg1391.PROD_ALC_MEL ,0,2 ) +
             LFill( vReg1391.OBS          ) +
             LFill( vReg1391.COD_ITEM ) +
             LFill( vReg1391.TP_RESIDUO ) +
             LFill( vReg1391.QTD_RESIDUO , 0, 2)
           ) ;
      end
      else
      begin
        Add( LFill('1391') +
             LFill( vReg1391.DT_REGISTRO       ) +
             LFill( vReg1391.QTD_MOID     ,0,2 ) +
             LFill( vReg1391.ESTQ_INI     ,0,2 ) +
             LFill( vReg1391.QTD_PRODUZ   ,0,2 ) +
             LFill( vReg1391.ENT_ANID_HID ,0,2 ) +
             LFill( vReg1391.OUTR_ENTR    ,0,2 ) +
             LFill( vReg1391.PERDA        ,0,2 ) +
             LFill( vReg1391.CONS         ,0,2 ) +
             LFill( vReg1391.SAI_ANI_HID  ,0,2 ) +
             LFill( vReg1391.SAIDAS       ,0,2 ) +
             LFill( vReg1391.ESTQ_FIN     ,0,2 ) +
             LFill( vReg1391.ESTQ_INI_MEL ,0,2 ) +
             LFill( vReg1391.PROD_DIA_MEL ,0,2 ) +
             LFill( vReg1391.UTIL_MEL     ,0,2 ) +
             LFill( vReg1391.PROD_ALC_MEL ,0,2 ) +
             LFill( vReg1391.OBS          )) ;
      end;
      Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro1391Count := FRegistro1391Count + Reg1390.Registro1391.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1400(Reg1001: TRegistro1001) ;
var
  intFor: integer;
  vReg1400: TRegistro1400;
begin
  if Assigned( Reg1001.Registro1400 ) then
  begin
    for intFor := 0 to Reg1001.Registro1400.Count - 1 do
    begin
      vReg1400 := Reg1001.Registro1400.Items[intFor];
      if FBloco_0.Registro0000.COD_VER > vlVersao117 then
      begin
        Add( LFill('1400') +
              LFill( vReg1400.COD_ITEM ) +
              LFill( vReg1400.COD_ITEM_IPM ) +
              IfThen((Trim(vReg1400.MUN)= EmptyStr), EmptyStr, LFill( vReg1400.MUN ) ) +
              LFill( vReg1400.VALOR,0,2 ) ) ;
      end
      else
      begin
        Add( LFill('1400') +
              LFill( vReg1400.COD_ITEM_IPM ) +
              IfThen((Trim(vReg1400.MUN)= EmptyStr), EmptyStr, LFill( vReg1400.MUN ) ) +
              LFill( vReg1400.VALOR,0,2 ) ) ;
      end;
      Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro1400Count := FRegistro1400Count + Reg1001.Registro1400.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1500(Reg1001: TRegistro1001) ;
var
  intFor: integer;
  intTP_LIGACAO: integer;
  strCOD_SIT: String;
  strCOD_GRUPO_TENSAO: String;
  strCOD_CONS: String;
begin
  if Assigned( Reg1001.Registro1500 ) then
  begin
     for intFor := 0 to Reg1001.Registro1500.Count - 1 do
     begin
        with Reg1001.Registro1500.Items[intFor] do
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
          case COD_GRUPO_TENSAO of
           gtA1:           strCOD_GRUPO_TENSAO := '01';
           gtA2:           strCOD_GRUPO_TENSAO := '02';
           gtA3:           strCOD_GRUPO_TENSAO := '03';
           gtA3a:          strCOD_GRUPO_TENSAO := '04';
           gtA4:           strCOD_GRUPO_TENSAO := '05';
           gtAS:           strCOD_GRUPO_TENSAO := '06';
           gtB107:         strCOD_GRUPO_TENSAO := '07';
           gtB108:         strCOD_GRUPO_TENSAO := '08';
           gtB209:         strCOD_GRUPO_TENSAO := '09';
           gtB2Rural:      strCOD_GRUPO_TENSAO := '10';
           gtB2Irrigacao:  strCOD_GRUPO_TENSAO := '11';
           gtB3:           strCOD_GRUPO_TENSAO := '12';
           gtB4a:          strCOD_GRUPO_TENSAO := '13';
           gtB4b:          strCOD_GRUPO_TENSAO := '14';
          end;
          case COD_CONS of
           ccComercial:         strCOD_CONS := '01';
           ccConsumoProprio:    strCOD_CONS := '02';
           ccIluminacaoPublica: strCOD_CONS := '03';
           ccIndustrial:        strCOD_CONS := '04';
           ccPoderPublico:      strCOD_CONS := '05';
           ccResidencial:       strCOD_CONS := '06';
           ccRural:             strCOD_CONS := '07';
           ccServicoPublico:    strCOD_CONS := '08';
          end;
          case TP_LIGACAO of
           tlMonofasico: intTP_LIGACAO := 1;
           tlBifasico:   intTP_LIGACAO := 2;
           tlTrifasico:  intTP_LIGACAO := 3;
           else          intTP_LIGACAO := 1;
          end;

          Add( LFill('1500') +
               LFill( IND_OPER ) +
               LFill( IND_EMIT ) +
               LFill( COD_PART ) +
               LFill( COD_MOD ) +
               LFill( strCOD_SIT ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( strCOD_CONS ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( DT_E_S ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_DESC,0,2 ) +
               LFill( VL_FORN,0,2 ) +
               LFill( VL_SERV_NT,0,2 ) +
               LFill( VL_TERC,0,2 ) +
               LFill( VL_DA,0,2 ) +
               LFill( VL_BC_ICMS,0,2 ) +
               LFill( VL_ICMS,0,2 ) +
               LFill( VL_BC_ICMS_ST,0,2 ) +
               LFill( VL_ICMS_ST,0,2 ) +
               LFill( COD_INF ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( intTP_LIGACAO, 0 ) +
               LFill( strCOD_GRUPO_TENSAO ) ) ;
        end;
        WriteRegistro1510( Reg1001.Registro1500.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1500Count := FRegistro1500Count + Reg1001.Registro1500.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1510(Reg1500: TRegistro1500) ;
var
  intFor: integer;
begin
  if Assigned( Reg1500.Registro1510 ) then
  begin
     for intFor := 0 to Reg1500.Registro1510.Count - 1 do
     begin
        with Reg1500.Registro1510.Items[intFor] do
        begin
          Add( LFill('1510') +
               LFill( NUM_ITEM ) +
               LFill( COD_ITEM ) +
               LFill( COD_CLASS ) +
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
               LFill( COD_PART ) +
               LFill( VL_PIS,0,2 ) +
               LFill( VL_COFINS,0,2 ) +
               LFill( COD_CTA ) ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1510Count := FRegistro1510Count + Reg1500.Registro1510.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1600(Reg1001: TRegistro1001) ;
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1600 ) then
  begin
     for intFor := 0 to Reg1001.Registro1600.Count - 1 do
     begin
        with Reg1001.Registro1600.Items[intFor] do
        begin
          Add( LFill('1600') +
               LFill( COD_PART ) +
               LFill( TOT_CREDITO, 0, 2) +
               LFill( TOT_DEBITO, 0, 2) );
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1600Count := FRegistro1600Count + Reg1001.Registro1600.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1601(Reg1001: TRegistro1001);
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1601 ) then
  begin
     for intFor := 0 to Reg1001.Registro1601.Count - 1 do
     begin
        with Reg1001.Registro1601.Items[intFor] do
        begin
          Add( LFill('1601') +
               LFill( COD_PART_IP ) +
               LFill( COD_PART_IT ) +
               LFill( TOT_VS, 0, 2) +
               LFill( TOT_ISS, 0, 2)+
               LFill( TOT_OUTROS, 0, 2) );
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1601Count := FRegistro1601Count + Reg1001.Registro1601.Count;
  end;
end;
procedure TBloco_1.WriteRegistro1700(Reg1001: TRegistro1001) ;
var
  intFor: integer;
  strCOD_DISP: String;
begin
  if Assigned( Reg1001.Registro1700 ) then
  begin
     for intFor := 0 to Reg1001.Registro1700.Count - 1 do
     begin
        with Reg1001.Registro1700.Items[intFor] do
        begin
          case COD_DISP of
           cdaFormSeguranca: strCOD_DISP := '00';
           cdaFSDA:          strCOD_DISP := '01';
           cdaNFe:           strCOD_DISP := '02';
           cdaFormContinuo:  strCOD_DISP := '03';
           cdaBlocos:        strCOD_DISP := '04';
           cdaJogosSoltos:   strCOD_DISP := '05';
          end;

          Check(StrIsNumber(Trim(NUM_DOC_INI)), '(1-1700) Documento Fiscal: Numeração incorreta "%s" para Documento Inicial', [NUM_DOC_INI]);
          Check(StrIsNumber(Trim(NUM_DOC_FIN)), '(1-1700) Documento Fiscal: Numeração incorreta "%s" para Documento Final', [NUM_DOC_FIN]);
          Check(StrIsNumber(Trim((NUM_AUT))), '(1-1700) Documento Fiscal: Numeração incorreta "%s" para Número Autorização', [NUM_AUT]);

          Add( LFill('1700') +
               LFill( strCOD_DISP, 2 ) +
               LFill( COD_MOD, 2 ) +
               LFill( SER, 4 ) +
               LFill( SUB, 3 ) +
               LFill( NUM_DOC_INI, 12 ) +
               LFill( NUM_DOC_FIN, 12 ) +
               LFill( NUM_AUT, 60 ));
        end;
        WriteRegistro1710( Reg1001.Registro1700.Items[intFor] );

        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1700Count := FRegistro1700Count + Reg1001.Registro1700.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1710(Reg1700: TRegistro1700) ;
var
  intFor: integer;
begin
  if Assigned( Reg1700.Registro1710 ) then
  begin
     for intFor := 0 to Reg1700.Registro1710.Count - 1 do
     begin
        with Reg1700.Registro1710.Items[intFor] do
        begin
          Check(StrIsNumber(Trim(NUM_DOC_INI)), '(1-1710) Documento Fiscal Cancelado: Numeração incorreta "%s" para Documento Inicial', [NUM_DOC_INI]);
          Check(StrIsNumber(Trim(NUM_DOC_FIN)), '(1-1710) Documento Fiscal Cancelado: Numeração incorreta "%s" para Documento Final', [NUM_DOC_FIN]);

          Add( LFill('1710') +
               LFill( NUM_DOC_INI, 12) +
               LFill( NUM_DOC_FIN, 12) ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1710Count := FRegistro1710Count + Reg1700.Registro1710.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1800(Reg1001: TRegistro1001) ;
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1800 ) then
  begin
     for intFor := 0 to Reg1001.Registro1800.Count - 1 do
     begin
        with Reg1001.Registro1800.Items[intFor] do
        begin
          Add( LFill('1800') +
               LFill( VL_CARGA, 0, 2, True ) +
               LFill( VL_PASS, 0, 2, True ) +
               LFill( VL_FAT, 0, 2, True ) +
               LFill( IND_RAT, 6, 2, True ) +
               LFill( VL_ICMS_ANT, 0, 2, True ) +
               LFill( VL_BC_ICMS, 0, 2, True ) +
               LFill( VL_ICMS_APUR, 0, 2, True ) +
               LFill( VL_BC_ICMS_APUR, 0, 2, True ) +
               LFill( VL_DIF, 0, 2, True ) ) ;
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1800Count := FRegistro1800Count + Reg1001.Registro1800.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1990 ;
begin
  if Assigned(Registro1990) then
  begin
     with Registro1990 do
     begin
       QTD_LIN_1 := QTD_LIN_1 + 1;
       ///
       Add( LFill('1990') +
            LFill(QTD_LIN_1,0) ) ;
     end;
  end;
end;

function TBloco_1.Registro1900New: TRegistro1900;
begin
   Result := FRegistro1001.Registro1900.New();
end;

procedure TBloco_1.WriteRegistro1900(Reg1001: TRegistro1001);
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1900 ) then
  begin
     for intFor := 0 to Reg1001.Registro1900.Count - 1 do
     begin
        with Reg1001.Registro1900.Items[intFor] do
        begin
          Add( LFill('1900') +
               LFill( IND_APUR_ICMS, 1 ) +
               LFill( DESCR_COMPL_OUT_APUR ) ) ;
        end;
        WriteRegistro1910(Reg1001.Registro1900.Items[intFor]);
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1900Count := FRegistro1900Count + Reg1001.Registro1900.Count;
  end;
end;

function TBloco_1.Registro1910New: TRegistro1910;
var
  U1900: TRegistro1900;
  U1900Count: Integer;
begin
   U1900Count := FRegistro1001.Registro1900.Count -1;
   if U1900Count = -1 then
      raise Exception.Create('O registro 1910 deve ser filho do registro 1900, e não existe nenhum 1900 pai!');

   U1900  := FRegistro1001.Registro1900.Items[U1900Count];
   Result := U1900.Registro1910.New();
end;

function TBloco_1.Registro1920New: TRegistro1920;
var
//  U1910: TRegistro1910;
  U1900Count: Integer;
  U1910Count: Integer;
begin
  U1900Count := FRegistro1001.Registro1900.Count -1;
  U1910Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Count -1;
//  U1910 := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[u1910Count];
  Result := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.New();
end;

function TBloco_1.Registro1921New: TRegistro1921;
var
//  U1920: TRegistro1920;
  U1900Count: Integer;
  U1910Count: Integer;
  U1920Count: Integer;
begin
  U1900Count := FRegistro1001.Registro1900.Count -1;
  U1910Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Count -1;
  U1920Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Count -1;

//  U1920 := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[u1910Count].Registro1920.Items[U1920Count];
  Result := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Items[U1920Count].Registro1921.New();
end;

function TBloco_1.Registro1922New: TRegistro1922;
var
  U1900Count: Integer;
  U1910Count: Integer;
  U1920Count: Integer;
  U1921Count: Integer;
begin
  U1900Count := FRegistro1001.Registro1900.Count -1;
  U1910Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Count -1;
  U1920Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Count -1;
  U1921Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Items[U1920Count].Registro1921.Count -1;

  Result := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Items[U1920Count].Registro1921.Items[U1921Count].Registro1922.New;

end;

function TBloco_1.Registro1923New: TRegistro1923;
var
  U1900Count: Integer;
  U1910Count: Integer;
  U1920Count: Integer;
  U1921Count: Integer;
begin
  U1900Count := FRegistro1001.Registro1900.Count -1;
  U1910Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Count -1;
  U1920Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Count -1;
  U1921Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Items[U1920Count].Registro1921.Count -1;

  Result := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Items[U1920Count].Registro1921.Items[U1921Count].Registro1923.New;
end;

function TBloco_1.Registro1925New: TRegistro1925;
var
  U1900Count: Integer;
  U1910Count: Integer;
  U1920Count: Integer;
begin
  U1900Count := FRegistro1001.Registro1900.Count -1;
  U1910Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Count -1;
  U1920Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Count -1;

  Result := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Items[U1920Count].Registro1925.New;
end;

function TBloco_1.Registro1926New: TRegistro1926;
var
  U1900Count: Integer;
  U1910Count: Integer;
  U1920Count: Integer;
begin
  U1900Count := FRegistro1001.Registro1900.Count -1;
  U1910Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Count -1;
  U1920Count := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Count -1;

  Result := FRegistro1001.Registro1900.Items[U1900Count].Registro1910.Items[U1910Count].Registro1920.Items[U1920Count].Registro1926.New;
end;

function TBloco_1.Registro1960New: TRegistro1960;
begin
   Result := FRegistro1001.Registro1960.New(FRegistro1001);
end;

function TBloco_1.Registro1970New: TRegistro1970;
begin
  Result := FRegistro1001.Registro1970.New();
end;

function TBloco_1.Registro1975New: TRegistro1975;
var
  U1970: TRegistro1970;
  U1970Count: Integer;
begin
  U1970Count := FRegistro1001.Registro1970.Count -1;
  if U1970Count = -1 then
    raise Exception.Create('O registro 1975 deve ser filho do registro 1970, e não existe nenhum 1970 pai!');

  U1970  := FRegistro1001.Registro1970.Items[U1970Count];
  Result := U1970.Registro1975.New();
end;

function TBloco_1.Registro1980New: TRegistro1980;
begin
  Result := FRegistro1001.Registro1980.New(FRegistro1001);
end;

procedure TBloco_1.WriteRegistro1910(Reg1900: TRegistro1900);
var
  intFor: integer;
begin
  if Assigned( Reg1900.Registro1910 ) then
  begin
     for intFor := 0 to Reg1900.Registro1910.Count - 1 do
     begin
        with Reg1900.Registro1910.Items[intFor] do
        begin
          Add( LFill('1910') +
               LFill( DT_INI ) +
               LFill( DT_FIN ) ) ;
        end;
        WriteRegistro1920(Reg1900.Registro1910.Items[intFor]);
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1910Count := FRegistro1910Count + Reg1900.Registro1910.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1920(Reg1910: TRegistro1910);
var
  intFor: integer;
begin
  if Assigned( Reg1910.Registro1920 ) then
  begin
     for intFor := 0 to Reg1910.Registro1920.Count - 1 do
     begin
        with Reg1910.Registro1920.Items[intFor] do
        begin
          Add( LFill('1920') +
               LFill( VL_TOT_TRANSF_DEBITOS_OA, 0, 2 ) +
               LFill( VL_TOT_AJ_DEBITOS_OA, 0, 2 ) +
               LFill( VL_ESTORNOS_CRED_OA, 0, 2 ) +
               LFill( VL_TOT_TRANSF_CREDITOS_OA, 0, 2 ) +
               LFill( VL_TOT_AJ_CREDITOS_OA, 0, 2 ) +
               LFill( VL_ESTORNOS_DEB_OA, 0, 2 ) +
               LFill( VL_SLD_CREDOR_ANT_OA, 0, 2 ) +
               LFill( VL_SLD_APURADO_OA, 0, 2 ) +
               LFill( VL_TOT_DED, 0, 2 ) +
               LFill( VL_ICMS_RECOLHER_OA, 0, 2 ) +
               LFill( VL_SLD_CREDOR_TRANSP_OA, 0, 2 ) +
               LFill( DEB_ESP_OA, 0, 2 ) );
        end;
        WriteRegistro1921(Reg1910.Registro1920.Items[intFor]);
        WriteRegistro1925(Reg1910.Registro1920.Items[intFor]);
        WriteRegistro1926(Reg1910.Registro1920.Items[intFor]);
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1920Count := FRegistro1920Count + Reg1910.Registro1920.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1921(Reg1920: TRegistro1920);
var
  intFor: integer;
begin
  if Assigned( Reg1920.Registro1921 ) then
  begin
     for intFor := 0 to Reg1920.Registro1921.Count - 1 do
     begin
        with Reg1920.Registro1921.Items[intFor] do
        begin
          Add( LFill('1921') +
               LFill( COD_AJ_APUR ) +
               LFill( DESCR_COMPL_AJ ) +
               LFill( VL_AJ_APUR, 0, 2 ) );
        end;
        WriteRegistro1922(Reg1920.Registro1921.Items[intFor]);
        WriteRegistro1923(Reg1920.Registro1921.Items[intFor]);
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1921Count := FRegistro1921Count + Reg1920.Registro1921.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1922(Reg1921: TRegistro1921);
var
  intFor: integer;
begin
  if Assigned( Reg1921.Registro1922 ) then
  begin
     for intFor := 0 to Reg1921.Registro1922.Count - 1 do
     begin
        with Reg1921.Registro1922.Items[intFor] do
        begin
          Add( LFill('1922') +
               LFill( NUM_DA ) +
               LFill( NUM_PROC ) +
               LFill( IND_PROC ) +
               LFill( PROC ) +
               LFill( TXT_COMPL ) );
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1922Count := FRegistro1922Count + Reg1921.Registro1922.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1923(Reg1921: TRegistro1921);
var
  intFor: integer;
begin
  if Assigned( Reg1921.Registro1923 ) then
  begin
     for intFor := 0 to Reg1921.Registro1923.Count - 1 do
     begin
        with Reg1921.Registro1923.Items[intFor] do
        begin
          Add( LFill('1923') +
               LFill( COD_PART ) +
               LFill( COD_MOD ) +
               LFill( SER ) +
               LFill( SUB ) +
               LFill( NUM_DOC ) +
               LFill( DT_DOC ) +
               LFill( COD_ITEM ) +
               LFill( VL_AJ_ITEM,0,2 ) +
               LFill( CHV_DOCe ) );
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1923Count := FRegistro1923Count + Reg1921.Registro1923.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1925(Reg1920: TRegistro1920);
var
  intFor: integer;
begin
  if Assigned( Reg1920.Registro1925 ) then
  begin
     for intFor := 0 to Reg1920.Registro1925.Count - 1 do
     begin
        with Reg1920.Registro1925.Items[intFor] do
        begin
          Add( LFill('1925') +
               LFill( COD_INF_ADIC ) +
               LFill( VL_INF_ADIC,0,2 ) +
               LFill( DESCR_COMPL_AJ ) );
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1925Count := FRegistro1925Count + Reg1920.Registro1925.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1926(Reg1920: TRegistro1920);
var
  intFor: integer;
begin
  if Assigned( Reg1920.Registro1926 ) then
  begin
     for intFor := 0 to Reg1920.Registro1926.Count - 1 do
     begin
        with Reg1920.Registro1926.Items[intFor] do
        begin
          Add( LFill('1926') +
               LFill( COD_OR ) +
               LFill( VL_OR,0,2 ) +
               LFill( DT_VCTO ) +
               LFill( COD_REC ) +
               LFill( NUM_PROC ) +
               LFill( IND_PROC ) +
               LFill( PROC ) +
               LFill( TXT_COMPL ) +
               LFill( MES_REF ) );
        end;
        Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistro1926Count := FRegistro1926Count + Reg1920.Registro1926.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1960(Reg1001: TRegistro1001);
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1960 ) then
  begin
    for intFor := 0 to Reg1001.Registro1960.Count - 1 do
    begin
      with Reg1001.Registro1960.Items[intFor] do
      begin
        Add( LFill('1960') +
             LFill( IND_AP ) +
             LFill( G1_01, 0, 2) +
             LFill( G1_02, 0, 2) +
             LFill( G1_03, 0, 2) +
             LFill( G1_04, 0, 2) +
             LFill( G1_05, 0, 2) +
             LFill( G1_06, 0, 2) +
             LFill( G1_07, 0, 2) +
             LFill( G1_08, 0, 2) +
             LFill( G1_09, 0, 2) +
             LFill( G1_10, 0, 2) +
             LFill( G1_11, 0, 2));
      end;
      Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro1960Count := FRegistro1960Count + Reg1001.Registro1960.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1970(Reg1001: TRegistro1001);
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1970 ) then
  begin
    for intFor := 0 to Reg1001.Registro1970.Count - 1 do
    begin
      with Reg1001.Registro1970.Items[intFor] do
      begin
        Add( LFill('1970') +
             LFill( IND_AP  ) +
             LFill( G3_01 , 0, 2) +
             LFill( G3_02 , 0, 2) +
             LFill( G3_03 , 0, 2) +
             LFill( G3_04 , 0, 2) +
             LFill( G3_05 , 0, 2) +
             LFill( G3_06 , 0, 2) +
             LFill( G3_07 , 0, 2) +
             LFill( G3_T  , 0, 2) +
             LFill( G3_08 , 0, 2) +
             LFill( G3_09 , 0, 2));
      end;
      Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
      WriteRegistro1975( Reg1001.Registro1970.Items[intFor] );
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro1970Count := FRegistro1970Count + Reg1001.Registro1970.Count;
  end;
end;

procedure TBloco_1.WriteRegistro1975(Reg1970: TRegistro1970);
var
  intFor: integer;
begin
  if Assigned( Reg1970.Registro1975) then
  begin
    for intFor := 0 to Reg1970.Registro1975.Count - 1 do
    begin
      with Reg1970.Registro1975.Items[intFor] do
      begin
        Add( LFill('1975') +
             LFill( ALIQ_IMP_BASE ,0, 2 ) +
             LFill( G3_10, 0, 2) +
             LFill( G3_11, 0, 2) +
             LFill( G3_12, 0, 2));
      end;
      Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro1975Count := FRegistro1975Count + Reg1970.Registro1975.Count;
  end;

end;

procedure TBloco_1.WriteRegistro1980(Reg1001: TRegistro1001);
var
  intFor: integer;
begin
  if Assigned( Reg1001.Registro1980 ) then
  begin
    for intFor := 0 to Reg1001.Registro1980.Count - 1 do
    begin
      with Reg1001.Registro1980.Items[intFor] do
      begin
        Add( LFill('1980') +
             LFill( IND_AP   ) +
             LFill( G4_01 , 0, 2) +
             LFill( G4_02 , 0, 2) +
             LFill( G4_03 , 0, 2) +
             LFill( G4_04 , 0, 2) +
             LFill( G4_05 , 0, 2) +
             LFill( G4_06 , 0, 2) +
             LFill( G4_07 , 0, 2) +
             LFill( G4_08 , 0, 2) +
             LFill( G4_09 , 0, 2) +
             LFill( G4_10 , 0, 2) +
             LFill( G4_11 , 0, 2) +
             LFill( G4_12 , 0, 2));
      end;

      Registro1990.QTD_LIN_1 := Registro1990.QTD_LIN_1 + 1;
    end;
    /// Variavél para armazenar a quantidade de registro do tipo.
    FRegistro1980Count := FRegistro1980Count + Reg1001.Registro1980.Count;
  end;
end;

end.
