{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010   Isaque Pinheiro                      }
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
|* 15/02/2011: Isaque Pinheiro e Fernando Amado
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrEPCBloco_M_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrEPCBloco_M, ACBrEPCBlocos,
     ACBrEPCBloco_0_Class;

type
  /// TBloco_M -

  { TBloco_M }

  TBloco_M = class(TACBrSPED)
  private
    FRegistroM001                       : TRegistroM001;      /// BLOCO M - RegistroM001
    FRegistroM990                       : TRegistroM990;      /// BLOCO M - RegistroM990

    FRegistroM100Count                  : integer;
    FRegistroM105Count                  : integer;
    FRegistroM110Count                  : integer;
    FRegistroM115Count                  : integer;
    FRegistroM200Count                  : integer;
    FRegistroM205Count                  : integer;
    FRegistroM210Count                  : integer;
    FRegistroM211Count                  : integer;
    FRegistroM220Count                  : integer;
    FRegistroM225Count                  : integer;
    FRegistroM230Count                  : integer;
    FRegistroM300Count                  : integer;
    FRegistroM350Count                  : integer;
    FRegistroM400Count                  : integer;
    FRegistroM410Count                  : integer;
    FRegistroM500Count                  : integer;
    FRegistroM505Count                  : integer;
    FRegistroM510Count                  : integer;
    FRegistroM515Count                  : integer;
    FRegistroM600Count                  : integer;
    FRegistroM605Count                  : integer;
    FRegistroM610Count                  : integer;
    FRegistroM611Count                  : integer;
    FRegistroM620Count                  : integer;
    FRegistroM625Count                  : integer;
    FRegistroM630Count                  : integer;
    FRegistroM700Count                  : integer;
    FRegistroM800Count                  : integer;
    FRegistroM810Count                  : integer;
    FBloco_0                            : TBloco_0;

    procedure WriteRegistroM100(RegM001 : TRegistroM001);
    procedure WriteRegistroM105(RegM100 : TRegistroM100);
    procedure WriteRegistroM110(RegM100 : TRegistroM100);
    procedure WriteRegistroM115(RegM110 : TRegistroM110);
    procedure WriteRegistroM200(RegM001 : TRegistroM001);
    procedure WriteRegistroM205(RegM200 : TRegistroM200);
    procedure WriteRegistroM210(RegM200 : TRegistroM200);
    procedure WriteRegistroM211(RegM210 : TRegistroM210);
    procedure WriteRegistroM220(RegM210 : TRegistroM210);
    procedure WriteRegistroM225(RegM220 : TRegistroM220);
    procedure WriteRegistroM230(RegM210 : TRegistroM210);
    procedure WriteRegistroM300(RegM001 : TRegistroM001);
    procedure WriteRegistroM350(RegM001 : TRegistroM001);
    procedure WriteRegistroM400(RegM001 : TRegistroM001);
    procedure WriteRegistroM410(RegM400 : TRegistroM400);
    procedure WriteRegistroM500(RegM001 : TRegistroM001);
    procedure WriteRegistroM505(RegM500 : TRegistroM500);
    procedure WriteRegistroM510(RegM500 : TRegistroM500);
    procedure WriteRegistroM515(RegM510 : TRegistroM510);
    procedure WriteRegistroM600(RegM001 : TRegistroM001);
    procedure WriteRegistroM605(RegM600 : TRegistroM600);
    procedure WriteRegistroM610(RegM600 : TRegistroM600);
    procedure WriteRegistroM611(RegM610 : TRegistroM610);
    procedure WriteRegistroM620(RegM610 : TRegistroM610);
    procedure WriteRegistroM625(RegM620 : TRegistroM620);
    procedure WriteRegistroM630(RegM610 : TRegistroM610);
    procedure WriteRegistroM700(RegM001 : TRegistroM001);
    procedure WriteRegistroM800(RegM001 : TRegistroM001);
    procedure WriteRegistroM810(RegM800 : TRegistroM800);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create ;                                      /// Create
    destructor  Destroy; override;                            /// Destroy
    procedure LimpaRegistros; override;

    function RegistroM001New            : TRegistroM001;
    function RegistroM100New            : TRegistroM100;
    function RegistroM105New            : TRegistroM105;
    function RegistroM110New            : TRegistroM110;
    function RegistroM115New            : TRegistroM115;
    function RegistroM200New            : TRegistroM200;
    function RegistroM205New            : TRegistroM205;
    function RegistroM210New            : TRegistroM210;
    function RegistroM211New            : TRegistroM211;
    function RegistroM220New            : TRegistroM220;
    function RegistroM225New            : TRegistroM225;
    function RegistroM230New            : TRegistroM230;
    function RegistroM300New            : TRegistroM300;
    function RegistroM350New            : TRegistroM350;
    function RegistroM400New            : TRegistroM400;
    function RegistroM410New            : TRegistroM410;
    function RegistroM500New            : TRegistroM500;
    function RegistroM505New            : TRegistroM505;
    function RegistroM510New            : TRegistroM510;
    function RegistroM515New            : TRegistroM515;
    function RegistroM600New            : TRegistroM600;
    function RegistroM605New            : TRegistroM605;
    function RegistroM610New            : TRegistroM610;
    function RegistroM611New            : TRegistroM611;
    function RegistroM620New            : TRegistroM620;
    function RegistroM625New            : TRegistroM625;
    function RegistroM630New            : TRegistroM630;
    function RegistroM700New            : TRegistroM700;
    function RegistroM800New            : TRegistroM800;
    function RegistroM810New            : TRegistroM810;

    procedure WriteRegistroM001;
    procedure WriteRegistroM990;

    property Bloco_0                    : TBloco_0      read FBloco_0           write FBloco_0;
    property RegistroM001               : TRegistroM001 read FRegistroM001      write FRegistroM001;
    property RegistroM990               : TRegistroM990 read FRegistroM990      write FRegistroM990;

    property RegistroM100Count          : integer       read FRegistroM100Count write FRegistroM100Count;
    property RegistroM105Count          : integer       read FRegistroM105Count write FRegistroM105Count;
    property RegistroM110Count          : integer       read FRegistroM110Count write FRegistroM110Count;
    property RegistroM115Count          : integer       read FRegistroM115Count write FRegistroM115Count;
    property RegistroM200Count          : integer       read FRegistroM200Count write FRegistroM200Count;
    property RegistroM205Count          : integer       read FRegistroM205Count write FRegistroM205Count;
    property RegistroM210Count          : integer       read FRegistroM210Count write FRegistroM210Count;
    property RegistroM211Count          : integer       read FRegistroM211Count write FRegistroM211Count;
    property RegistroM220Count          : integer       read FRegistroM220Count write FRegistroM220Count;
    property RegistroM225Count          : integer       read FRegistroM225Count write FRegistroM225Count;
    property RegistroM230Count          : integer       read FRegistroM230Count write FRegistroM230Count;
    property RegistroM300Count          : integer       read FRegistroM300Count write FRegistroM300Count;
    property RegistroM350Count          : integer       read FRegistroM350Count write FRegistroM350Count;
    property RegistroM400Count          : integer       read FRegistroM400Count write FRegistroM400Count;
    property RegistroM410Count          : integer       read FRegistroM410Count write FRegistroM410Count;
    property RegistroM500Count          : integer       read FRegistroM500Count write FRegistroM500Count;
    property RegistroM505Count          : integer       read FRegistroM505Count write FRegistroM505Count;
    property RegistroM510Count          : integer       read FRegistroM510Count write FRegistroM510Count;
    property RegistroM515Count          : integer       read FRegistroM515Count write FRegistroM515Count;
    property RegistroM600Count          : integer       read FRegistroM600Count write FRegistroM600Count;
    property RegistroM605Count          : integer       read FRegistroM605Count write FRegistroM605Count;
    property RegistroM610Count          : integer       read FRegistroM610Count write FRegistroM610Count;
    property RegistroM611Count          : integer       read FRegistroM611Count write FRegistroM611Count;
    property RegistroM620Count          : integer       read FRegistroM620Count write FRegistroM620Count;
    property RegistroM625Count          : integer       read FRegistroM625Count write FRegistroM625Count;
    property RegistroM630Count          : integer       read FRegistroM630Count write FRegistroM630Count;
    property RegistroM700Count          : integer       read FRegistroM700Count write FRegistroM700Count;
    property RegistroM800Count          : integer       read FRegistroM800Count write FRegistroM800Count;
    property RegistroM810Count          : integer       read FRegistroM810Count write FRegistroM810Count;
  end;

implementation

{ TBloco_M }

constructor TBloco_M.Create;
begin
  inherited ;
  CriaRegistros;
end;

destructor TBloco_M.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_M.CriaRegistros;
begin
  FRegistroM001           := TRegistroM001.Create;
  FRegistroM990           := TRegistroM990.Create;

  FRegistroM100Count      := 0;
  FRegistroM105Count      := 0;
  FRegistroM110Count      := 0;
  FRegistroM115Count      := 0;
  FRegistroM200Count      := 0;
  FRegistroM205Count      := 0;
  FRegistroM210Count      := 0;
  FRegistroM211Count      := 0;
  FRegistroM220Count      := 0;
  FRegistroM225Count      := 0;
  FRegistroM230Count      := 0;
  FRegistroM300Count      := 0;
  FRegistroM350Count      := 0;
  FRegistroM400Count      := 0;
  FRegistroM410Count      := 0;
  FRegistroM500Count      := 0;
  FRegistroM505Count      := 0;
  FRegistroM510Count      := 0;
  FRegistroM515Count      := 0;
  FRegistroM600Count      := 0;
  FRegistroM605Count      := 0;
  FRegistroM610Count      := 0;
  FRegistroM611Count      := 0;
  FRegistroM620Count      := 0;
  FRegistroM625Count      := 0;
  FRegistroM630Count      := 0;
  FRegistroM700Count      := 0;
  FRegistroM800Count      := 0;
  FRegistroM810Count      := 0;

  FRegistroM990.QTD_LIN_M := 0;
end;

procedure TBloco_M.LiberaRegistros;
begin
  FRegistroM001.Free;
  FRegistroM990.Free;
end;

procedure TBloco_M.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_M.RegistroM001New: TRegistroM001;
begin
   Result := FRegistroM001;
end;

function TBloco_M.RegistroM100New: TRegistroM100;
begin
   Result := FRegistroM001.RegistroM100.New;
end;

function TBloco_M.RegistroM105New: TRegistroM105;
  var
    M100Count: integer;
begin
   M100Count := FRegistroM001.RegistroM100.Count -1;
   //
   Result := FRegistroM001.RegistroM100.Items[M100Count].RegistroM105.New;
end;

function TBloco_M.RegistroM110New: TRegistroM110;
var
   M100Count: integer;
begin
   M100Count := FRegistroM001.RegistroM100.Count -1;
   //
   Result := FRegistroM001.RegistroM100.Items[M100Count].RegistroM110.New;
end;

function TBloco_M.RegistroM115New: TRegistroM115;
var
   M100Count: integer;
   M110Count : integer;
begin
   M100Count := FRegistroM001.RegistroM100.Count -1;
   M110Count := FRegistroM001.RegistroM100.Items[M100Count].RegistroM110.Count -1;

   Result := FRegistroM001.RegistroM100.Items[M100Count].RegistroM110.Items[M110Count].RegistroM115.New;
end;

function TBloco_M.RegistroM200New: TRegistroM200;
begin
   Result := FRegistroM001.RegistroM200;
end;

function TBloco_M.RegistroM205New: TRegistroM205;
begin
  Result := FRegistroM001.RegistroM200.RegistroM205.New;
end;

function TBloco_M.RegistroM210New: TRegistroM210;
begin
   Result := FRegistroM001.RegistroM200.RegistroM210.New;
end;

function TBloco_M.RegistroM211New: TRegistroM211;
  var
    M210Count: integer;
begin
   M210Count := FRegistroM001.RegistroM200.RegistroM210.Count -1;
   //
   Result := FRegistroM001.RegistroM200.RegistroM210.Items[M210Count].RegistroM211;
end;

function TBloco_M.RegistroM220New: TRegistroM220;
var
   M210Count: integer;
begin
   M210Count := FRegistroM001.RegistroM200.RegistroM210.Count -1;
   //
   Result := FRegistroM001.RegistroM200.RegistroM210.Items[M210Count].RegistroM220.New;
end;

function TBloco_M.RegistroM225New: TRegistroM225;
var
   M210Count: integer;
   M220Count: integer;
begin
   M210Count := FRegistroM001.RegistroM200.RegistroM210.Count -1;
   M220Count := FRegistroM001.RegistroM200.RegistroM210.Items[M210Count].RegistroM220.Count -1;;
   //
   Result := FRegistroM001.RegistroM200.RegistroM210.Items[M210Count].RegistroM220.Items[M220Count].RegistroM225.New;
end;

function TBloco_M.RegistroM230New: TRegistroM230;
  var
    M210Count: integer;
begin
   M210Count := FRegistroM001.RegistroM200.RegistroM210.Count -1;
   //
   Result := FRegistroM001.RegistroM200.RegistroM210.Items[M210Count].RegistroM230.New;
end;

function TBloco_M.RegistroM300New: TRegistroM300;
begin
   Result := FRegistroM001.RegistroM300.New;
end;

function TBloco_M.RegistroM350New: TRegistroM350;
begin
   Result := FRegistroM001.RegistroM350;
end;

function TBloco_M.RegistroM400New: TRegistroM400;
begin
   Result := FRegistroM001.RegistroM400.New;
end;

function TBloco_M.RegistroM410New: TRegistroM410;
  var
    M400Count: integer;
begin
   M400Count := FRegistroM001.RegistroM400.Count -1;
   //
   Result := FRegistroM001.RegistroM400.Items[M400Count].RegistroM410.New;
end;

function TBloco_M.RegistroM500New: TRegistroM500;
begin
   Result := FRegistroM001.RegistroM500.New;
end;

function TBloco_M.RegistroM505New: TRegistroM505;
  var
    M500Count: integer;
begin
   M500Count := FRegistroM001.RegistroM500.Count -1;
   //
   Result := FRegistroM001.RegistroM500.Items[M500Count].RegistroM505.New;
end;

function TBloco_M.RegistroM510New: TRegistroM510;
var
   M500Count: integer;
begin
   M500Count := FRegistroM001.RegistroM500.Count -1;
   //
   Result := FRegistroM001.RegistroM500.Items[M500Count].RegistroM510.New;
end;

function TBloco_M.RegistroM515New: TRegistroM515;
var
   M500Count: integer;
   M510Count: integer;
begin
   M500Count := FRegistroM001.RegistroM500.Count -1;
   M510Count := FRegistroM001.RegistroM500.Items[M500Count].RegistroM510.Count -1;;
   //
   Result := FRegistroM001.RegistroM500.Items[M500Count].RegistroM510.Items[M510Count].RegistroM515.New;
end;

function TBloco_M.RegistroM600New: TRegistroM600;
begin
   Result := FRegistroM001.RegistroM600;
end;

function TBloco_M.RegistroM605New: TRegistroM605;
begin
   Result := FRegistroM001.RegistroM600.RegistroM605.New;
end;

function TBloco_M.RegistroM610New: TRegistroM610;
begin
   Result := FRegistroM001.RegistroM600.RegistroM610.New;
end;

function TBloco_M.RegistroM611New: TRegistroM611;
  var
    M610Count: integer;
begin
   M610Count := FRegistroM001.RegistroM600.RegistroM610.Count -1;
   //
   Result := FRegistroM001.RegistroM600.RegistroM610.Items[M610Count].RegistroM611;
end;

function TBloco_M.RegistroM620New: TRegistroM620;
var
  M610Count: integer;
begin
   M610Count := FRegistroM001.RegistroM600.RegistroM610.Count -1;
   //
   Result := FRegistroM001.RegistroM600.RegistroM610.Items[M610Count].RegistroM620.New;
end;

function TBloco_M.RegistroM625New: TRegistroM625;
var
  M610Count: integer;
  M620Count: integer;
begin
  M610Count := FRegistroM001.RegistroM600.RegistroM610.Count -1;
  M620Count := FRegistroM001.RegistroM600.RegistroM610.Items[M610Count].RegistroM620.Count -1;;
   //
  Result := FRegistroM001.RegistroM600.RegistroM610.Items[M610Count].RegistroM620.Items[M620Count].RegistroM625.New;
end;

function TBloco_M.RegistroM630New: TRegistroM630;
  var
    M610Count: integer;
begin
   M610Count := FRegistroM001.RegistroM600.RegistroM610.Count -1;
   //
   Result := FRegistroM001.RegistroM600.RegistroM610.Items[M610Count].RegistroM630.New;
end;

function TBloco_M.RegistroM700New: TRegistroM700;
begin
   Result := FRegistroM001.RegistroM700.New;
end;

function TBloco_M.RegistroM800New: TRegistroM800;
begin
   Result := FRegistroM001.RegistroM800.New;
end;

function TBloco_M.RegistroM810New: TRegistroM810;
  var
    M800Count: integer;
begin
   M800Count := FRegistroM001.RegistroM800.Count -1;
   //
   Result := FRegistroM001.RegistroM800.Items[M800Count].RegistroM810.New;
end;

procedure TBloco_M.WriteRegistroM001 ;
begin
  if Assigned(FRegistroM001) then
  begin
     with FRegistroM001 do
     begin
        Add( LFill( 'M001' ) +
             LFill( Integer(IND_MOV), 0 ) ) ;

        if IND_MOV = imComDados then
        begin
          WriteRegistroM100(FRegistroM001) ;
          WriteRegistroM200(FRegistroM001) ;
          WriteRegistroM300(FRegistroM001) ;
          WriteRegistroM350(FRegistroM001) ;
          WriteRegistroM400(FRegistroM001) ;
          WriteRegistroM500(FRegistroM001) ;
          WriteRegistroM600(FRegistroM001) ;
          WriteRegistroM700(FRegistroM001) ;
          WriteRegistroM800(FRegistroM001) ;
        end;
     end;

     RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
  end;
end;

procedure TBloco_M.WriteRegistroM100(RegM001: TRegistroM001) ;
  var
    intFor           : integer;
    strIND_CRED_ORI  : String;
    strIND_DESC_CRED : String;
begin
  if Assigned(RegM001.RegistroM100) then
  begin
     for intFor := 0 to RegM001.RegistroM100.Count - 1 do
     begin
        with RegM001.RegistroM100.Items[intFor] do
        begin

          case IND_CRED_ORI of
            icoOperProprias   : strIND_CRED_ORI :='0' ;// 0 // Operações próprias
            icoEvenFusaoCisao : strIND_CRED_ORI :='1' ;// 1 // Evento de incorporação, cisão ou fusão
          end;

          case IND_DESC_CRED of
             idcTotal   : strIND_DESC_CRED :='0' ; // 0 // Utilização do valor total para desconto da contribuição apurada no período, no Registro M200;
             idcParcial : strIND_DESC_CRED :='1' ; // 1 // Utilização de valor parcial para desconto da contribuição apurada no período, no Registro M200
          end;

          Add( LFill('M100')                +
               LFill( COD_CRED )            +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( strIND_CRED_ORI )     +
               VLFill( VL_BC_PIS, 0, 2)     +
               VDFill( ALIQ_PIS, 4)         +
               VLFill( QUANT_BC_PIS, 3)     +  // Veja nota abaixo e também http://www.djsystem.com.br/acbr/mantis/view.php?id=1010
               VDFill( ALIQ_PIS_QUANT, 4)   +  //Deve permitir nulo, pois só deve ser preenchido caso COD_CRED ser 103, 203, 303, 105, 205, 305, 108, 208 e 308.
               LFill( VL_CRED ,0,2)         +
               LFill( VL_AJUS_ACRES, 0, 2)  +
               LFill( VL_AJUS_REDUC, 0, 2)  +
               LFill( VL_CRED_DIF,   0, 2)  +
               LFill( VL_CRED_DISP,  0, 2)  +
               LFill( strIND_DESC_CRED )    +
               VLFill( VL_CRED_DESC, 0, 2)  +
               LFill( SLD_CRED, 0, 2 ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroM105( RegM001.RegistroM100.Items[intFor] );
        WriteRegistroM110( RegM001.RegistroM100.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM100Count := FRegistroM100Count + RegM001.RegistroM100.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM105(RegM100: TRegistroM100) ;
  var
    intFor         : integer;
    strNAT_BC_CRED : String;
begin
  if Assigned(RegM100.RegistroM105) then
  begin
     for intFor := 0 to RegM100.RegistroM105.Count - 1 do
     begin
        with RegM100.RegistroM105.Items[intFor] do
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

          Add( LFill('M105')                 +
               LFill( strNAT_BC_CRED )       +
               LFill( CstPisToStr(CST_PIS) )           +
               VLFill( VL_BC_PIS_TOT, 0, 2)  +
               VLFill( VL_BC_PIS_CUM, 0, 2)  +
               VLFill( VL_BC_PIS_NC,  0, 2)  +
               VLFill( VL_BC_PIS, 0, 2)      +
               VDFill( QUANT_BC_PIS_TOT, 3)  +
               VDFill( QUANT_BC_PIS , 3 )    +
               LFill( DESC_CRED ) ) ;
        end;

        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM105Count := FRegistroM105Count + RegM100.RegistroM105.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM110(RegM100: TRegistroM100) ;
  var
    intFor    : integer;
    strIND_AJ : String;
    strCOD_AJ : String;    
begin
  if Assigned(RegM100.RegistroM110) then
  begin
     for intFor := 0 to RegM100.RegistroM110.Count - 1 do
     begin
        with RegM100.RegistroM110.Items[intFor] do
        begin
          case IND_AJ of
            indAjReducao   : strIND_AJ := '0' ;
            indAjAcressimo : strIND_AJ := '1' ;
          end;

          case COD_AJ of
            codAjAcaoJudicial      : strCOD_AJ := '01'; // Ajuste Oriundo de Ação Judicial
            codAjProAdministrativo : strCOD_AJ := '02'; // Ajuste Oriundo de Processo Administrativo
            codAjLegTributaria     : strCOD_AJ := '03'; // Ajuste Oriundo da Legislação Tributária
            codAjEspRTI            : strCOD_AJ := '04'; // Ajuste Oriundo Especificamente do RTT
            codAjOutrasSituacaoes  : strCOD_AJ := '05'; // Ajuste Oriundo de Outras Situações
            codAjEstorno           : strCOD_AJ := '06'; // Estorno
          end;

          Add( LFill('M110')       +
               LFill( strIND_AJ )  +        
               LFill( VL_AJ ,0,2 ) +
               LFill( strCOD_AJ )  +
               LFill( NUM_DOC )    +
               LFill( DESCR_AJ )   +
               LFill( DT_REF ) ) ;
        end;

        WriteRegistroM115(RegM100.RegistroM110.Items[intFor]);
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM110Count := FRegistroM110Count + RegM100.RegistroM110.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM115(RegM110: TRegistroM110);
var
   intFor      : integer;
begin
    if Assigned(RegM110.RegistroM115) then
    begin
       for intFor := 0 to RegM110.RegistroM115.Count - 1 do
       begin
          with RegM110.RegistroM115.Items[intFor] do
          begin
            Add( LFill('M115')                     +
                 LFill( DET_VALOR_AJ ,0, 2 )       +
                 LFill( CstPisToStr(CST_PIS) , 2,True ) +
                 LFill( DET_BC_CRED ,0,3 )         +
                 LFill( DET_ALIQ, 8, 4)            +
                 LFill( DT_OPER_AJ)                +
                 LFill( DESC_AJ )                  +
                 LFill( COD_CTA )              +
                 LFill( INFO_COMPL)) ;
          end;
          ///
          RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
       end;
       /// Variavél para armazenar a quantidade de registro do tipo.
       FRegistroM115Count := FRegistroM115Count + RegM110.RegistroM115.Count;
    end;
end;

procedure TBloco_M.WriteRegistroM200(RegM001: TRegistroM001) ;
begin
  if Assigned(RegM001.RegistroM200) then
  begin
     with RegM001.RegistroM200 do
     begin
       Add( LFill('M200')                     +
            LFill( VL_TOT_CONT_NC_PER,0,2 )   +
            LFill( VL_TOT_CRED_DESC,0,2 )     +
            LFill( VL_TOT_CRED_DESC_ANT,0,2 ) +
            LFill( VL_TOT_CONT_NC_DEV,0,2 )   +
            LFill( VL_RET_NC,0,2 )            +
            LFill( VL_OUT_DED_NC,0,2 )        +
            LFill( VL_CONT_NC_REC,0,2 )       +
            LFill( VL_TOT_CONT_CUM_PER,0,2 )  +
            LFill( VL_RET_CUM,0,2 )           +
            LFill( VL_OUT_DED_CUM,0,2 )       +
            LFill( VL_CONT_CUM_REC,0,2 )      +
            LFill( VL_TOT_CONT_REC,0,2 ) ) ;
     end;
     // Registros FILHOS
     WriteRegistroM205( RegM001.RegistroM200 );
     WriteRegistroM210( RegM001.RegistroM200 );
     ///
     RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM200Count := FRegistroM200Count + 1;
  end;
end;

procedure TBloco_M.WriteRegistroM205(RegM200: TRegistroM200);
var
   intFor      : integer;
begin
    if Assigned(RegM200.RegistroM205) then
    begin
       for intFor := 0 to RegM200.RegistroM205.Count - 1 do
       begin
          with RegM200.RegistroM205.Items[intFor] do
          begin
            Add( LFill('M205')                  +
                 LFill( NUM_CAMPO , 2 )         +
                 LFill( COD_REC , 6 )           +
                 LFill( VL_DEBITO ,0,2 )) ;
          end;
          ///
          RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
       end;
       /// Variavél para armazenar a quantidade de registro do tipo.
       FRegistroM205Count := FRegistroM205Count + RegM200.RegistroM205.Count;
    end;
end;

procedure TBloco_M.WriteRegistroM210(RegM200: TRegistroM200) ;
var
    intFor      : integer;
    strCOD_CONT : String;
begin
  if Assigned(RegM200.RegistroM210) then
  begin
     for intFor := 0 to RegM200.RegistroM210.Count - 1 do
     begin
        with RegM200.RegistroM210.Items[intFor] do
        begin

          case COD_CONT of
                    ccNaoAcumAliqBasica                 : strCOD_CONT :='01' ; // Contribuição não-cumulativa apurada a alíquota básica
                    ccNaoAcumAliqDiferenciada           : strCOD_CONT :='02' ; // Contribuição não-cumulativa apurada a alíquotas diferenciadas
                    ccNaoAcumAliqUnidProduto            : strCOD_CONT :='03' ; // Contribuição não-cumulativa apurada a alíquota por unidade de medida de produto
                    ccNaoAcumAliqBasicaAtivImobiliaria  : strCOD_CONT :='04' ; // Contribuição não-cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaPorST                      : strCOD_CONT :='31' ; // Contribuição apurada por substituição tributária
                    ccApuradaPorSTManaus                : strCOD_CONT :='32' ; // Contribuição apurada por substituição tributária - Vendas à Zona Franca de Manaus
                    ccAcumAliqBasica                    : strCOD_CONT :='51' ; // Contribuição cumulativa apurada a alíquota básica
                    ccAcumAliqDiferenciada              : strCOD_CONT :='52' ; // Contribuição cumulativa apurada a alíquotas diferenciadas
                    ccAcumAliqUnidProduto               : strCOD_CONT :='53' ; // Contribuição cumulativa apurada a alíquota por unidade de medida de produto
                    ccAcumAliqBasicaAtivImobiliaria     : strCOD_CONT :='54' ; // Contribuição cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaAtivImobiliaria            : strCOD_CONT :='70' ; // Contribuição apurada da Atividade Imobiliária - RET
                    ccApuradaSCPNaoCumulativa           : strCOD_CONT :='71' ; // Contribuição apurada de SCP - Incidência Não Cumulativa
                    ccApuradaSCPCumulativa              : strCOD_CONT :='72' ; // Contribuição apurada de SCP - Incidência Cumulativa
                    ccPISPasepSalarios                  : strCOD_CONT :='99' ; // Contribuição para o PIS/Pasep - Folha de Salários
          end;

          Add( LFill('M210')                     +
               LFill( strCOD_CONT )              +
               LFill( VL_REC_BRT, 0, 2 )         +
               LFill( VL_BC_CONT, 0, 2 )         +
               VDFill( ALIQ_PIS       , 4)       +
               VDFill( QUANT_BC_PIS   , 3)       +
               VDFill( ALIQ_PIS_QUANT , 4)       +
               LFill( VL_CONT_APUR,   0, 2 )     +
               LFill( VL_AJUS_ACRES,  0, 2 )     +
               LFill( VL_AJUS_REDUC,  0, 2 )     +
               VLFill( VL_CONT_DIFER, 0, 2)      +
               VLFill( VL_CONT_DIFER_ANT, 0, 2)  +
               LFill( VL_CONT_PER ,0,2 ) ) ;
        end;

        // Registros FILHOS
        if (Bloco_0.Registro0000.IND_NAT_PJ = indNatPJSocCooperativa) then
          WriteRegistroM211( RegM200.RegistroM210.Items[intFor] );
        WriteRegistroM220( RegM200.RegistroM210.Items[intFor] );
        WriteRegistroM230( RegM200.RegistroM210.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM210Count := FRegistroM210Count + RegM200.RegistroM210.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM211(RegM210: TRegistroM210) ;
  var
    strIND_TIP_COOP : String;
begin
  if Assigned(RegM210.RegistroM211) then
  begin
     with RegM210.RegistroM211 do
     begin
        case IND_TIP_COOP of
          itcProdAgropecuaria: strIND_TIP_COOP :='01' ; // Cooperativa de Produção Agropecuária;
          itcConsumo         : strIND_TIP_COOP :='02' ; // Cooperativa de Consumo;
          itcCredito         : strIND_TIP_COOP :='03' ; // Cooperativa de Crédito;
          itcEletRural       : strIND_TIP_COOP :='04' ; // Cooperativa de Eletrificação Rural;
          itcTransCargas     : strIND_TIP_COOP :='05' ; // Cooperativa de Transporte Rodoviário de Cargas;
          itcMedicos         : strIND_TIP_COOP :='06' ; // Cooperativa de Médicos;
          itcOutras          : strIND_TIP_COOP :='99' ; // Outras.
        end;

       Add( LFill('M211')                        +
            LFill( strIND_TIP_COOP )             +    
            LFill( VL_BC_CONT_ANT_EXC_COOP,0,2 ) +
            LFill( VL_EXC_COOP_GER,0,2 )         +
            LFill( VL_EXC_ESP_COOP,0,2 )         +
            LFill( VL_BC_CONT,0,2 ) ) ;
     end;

     ///
     RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM211Count := FRegistroM211Count + 1;
  end;
end;

procedure TBloco_M.WriteRegistroM220(RegM210: TRegistroM210) ;
  var
    intFor : integer;
    strIND_AJ : String;
    strCOD_AJ : String;
begin
  if Assigned(RegM210.RegistroM220) then
  begin
     for intFor := 0 to RegM210.RegistroM220.Count - 1 do
     begin
        with RegM210.RegistroM220.Items[intFor] do
        begin

          case IND_AJ of
            indAjReducao   : strIND_AJ := '0' ;
            indAjAcressimo : strIND_AJ := '1' ;
          end;

          case COD_AJ of
            codAjAcaoJudicial      : strCOD_AJ := '01'; // Ajuste Oriundo de Ação Judicial
            codAjProAdministrativo : strCOD_AJ := '02'; // Ajuste Oriundo de Processo Administrativo
            codAjLegTributaria     : strCOD_AJ := '03'; // Ajuste Oriundo da Legislação Tributária
            codAjEspRTI            : strCOD_AJ := '04'; // Ajuste Oriundo Especificamente do RTT
            codAjOutrasSituacaoes  : strCOD_AJ := '05'; // Ajuste Oriundo de Outras Situações
            codAjEstorno           : strCOD_AJ := '06'; // Estorno
          end;

          Add( LFill('M220')       +
               LFill( strIND_AJ )  +        
               LFill( VL_AJ ,0,2 ) +
               LFill( strCOD_AJ )  +        
               LFill( NUM_DOC )    +
               LFill( DESCR_AJ )   +
               LFill( DT_REF ) ) ;
        end;
        WriteRegistroM225( RegM210.RegistroM220.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM220Count := FRegistroM220Count + RegM210.RegistroM220.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM225(RegM220: TRegistroM220);
var
  intFor : integer;
begin
  if Assigned(RegM220.RegistroM225) then
  begin
     for intFor := 0 to RegM220.RegistroM225.Count - 1 do
     begin
        with RegM220.RegistroM225.Items[intFor] do
        begin
          Add( LFill('M225')                 +
               LFill( DET_VALOR_AJ, 0, 2 )   +
               LFill( CstPisToStr(CST_PIS) ) +
               LFill( DET_BC_CRED, 0, 3 )    +
               LFill( DET_ALIQ, 8, 4 )       +
               LFill( DT_OPER_AJ )           +
               LFill( DESC_AJ )              +
               LFill( COD_CTA)           +
               LFill( INFO_COMPL)) ;
        end;
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM225Count := FRegistroM225Count + RegM220.RegistroM225.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM230(RegM210: TRegistroM210) ;
  var
    intFor: integer;
begin
  if Assigned(RegM210.RegistroM230) then
  begin
     for intFor := 0 to RegM210.RegistroM230.Count - 1 do
     begin
        with RegM210.RegistroM230.Items[intFor] do
        begin
          Add( LFill('M230')             +
               LFill( CNPJ )             +
               LFill( VL_VEND,0,2 )      +
               LFill( VL_NAO_RECEB,0,2 ) +
               LFill( VL_CONT_DIF,0,2 )  +
               LFill( VL_CRED_DIF,0,2 )  +
               LFill( COD_CRED ) ) ;       //Verificar criação da tabela no ACBrEPCBlocos
        end;
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM230Count := FRegistroM230Count + RegM210.RegistroM230.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM300(RegM001: TRegistroM001) ;
  var
    intFor           : integer;
    strNAT_CRED_DESC : String;
    strCOD_CONT      : String;
begin
  if Assigned(RegM001.RegistroM300) then
  begin
     for intFor := 0 to RegM001.RegistroM300.Count - 1 do
     begin
        with RegM001.RegistroM300.Items[intFor] do
        begin

          case NAT_CRED_DESC of
            ncdAliqBasica         : strNAT_CRED_DESC :='01' ;// Crédito a Alíquota Básica;
            ncdAliqDiferenciada   : strNAT_CRED_DESC :='02' ;// Crédito a Alíquota Diferenciada;
            ncdAliqUnidProduto    : strNAT_CRED_DESC :='03' ;// Crédito a Alíquota por Unidade de Produto;
            ncdPresAgroindustria  : strNAT_CRED_DESC :='04' ;// Crédito Presumido da Agroindústria.
          end;

          case COD_CONT of
                    ccNaoAcumAliqBasica                 : strCOD_CONT :='01' ; // Contribuição não-cumulativa apurada a alíquota básica
                    ccNaoAcumAliqDiferenciada           : strCOD_CONT :='02' ; // Contribuição não-cumulativa apurada a alíquotas diferenciadas
                    ccNaoAcumAliqUnidProduto            : strCOD_CONT :='03' ; // Contribuição não-cumulativa apurada a alíquota por unidade de medida de produto
                    ccNaoAcumAliqBasicaAtivImobiliaria  : strCOD_CONT :='04' ; // Contribuição não-cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaPorST                      : strCOD_CONT :='31' ; // Contribuição apurada por substituição tributária
                    ccApuradaPorSTManaus                : strCOD_CONT :='32' ; // Contribuição apurada por substituição tributária - Vendas à Zona Franca de Manaus
                    ccAcumAliqBasica                    : strCOD_CONT :='51' ; // Contribuição cumulativa apurada a alíquota básica
                    ccAcumAliqDiferenciada              : strCOD_CONT :='52' ; // Contribuição cumulativa apurada a alíquotas diferenciadas
                    ccAcumAliqUnidProduto               : strCOD_CONT :='53' ; // Contribuição cumulativa apurada a alíquota por unidade de medida de produto
                    ccAcumAliqBasicaAtivImobiliaria     : strCOD_CONT :='54' ; // Contribuição cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaAtivImobiliaria            : strCOD_CONT :='70' ; // Contribuição apurada da Atividade Imobiliária - RET
                    ccApuradaSCPNaoCumulativa           : strCOD_CONT :='71' ; // Contribuição apurada de SCP - Incidência Não Cumulativa
                    ccApuradaSCPCumulativa              : strCOD_CONT :='72' ; // Contribuição apurada de SCP - Incidência Cumulativa
                    ccPISPasepSalarios                  : strCOD_CONT :='99' ; // Contribuição para o PIS/Pasep - Folha de Salários
          end;

          Add( LFill('M300')                     +  {REG}
               LFill( strCOD_CONT )              +  {COD_CONT}           
               LFill( VL_CONT_APUR_DIFER,0,2 )   +  {VL_CONT_APUR_DIFER}
               LFill( strNAT_CRED_DESC )         +  {NAT_CRED_DESC}
               LFill( VL_CRED_DESC_DIFER,0,2 )   +  {VL_CRED_DESC_DIFER}
               LFill( VL_CONT_DIFER_ANT,0,2 )    +  {VL_CONT_DIFER_ANT}
               LFill( PER_APUR )                 +  {PER_APUR}
               LFill( DT_RECEB ) ) ;                {DT_RECEB}




        end;

        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM300Count := FRegistroM300Count + RegM001.RegistroM300.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM350(RegM001: TRegistroM001) ;
begin
  if Assigned(RegM001.RegistroM350) then
  begin
     with RegM001.RegistroM350 do
     begin
        if VL_TOT_FOL > 0 then
        begin
           Add( LFill('M350')             +
                LFill( VL_TOT_FOL,0,2 )   +
                LFill( VL_EXC_BC,0,2 )    +
                LFill( VL_TOT_BC,0,2 )    +
                LFill( ALIQ_PIS_FOL,0,2 ) +
                LFill( VL_TOT_CONT_FOL,0,2 ) ) ;

           RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
           /// Variavél para armazenar a quantidade de registro do tipo.
           FRegistroM350Count := FRegistroM350Count + 1 ;
        end;
     end;
  end;
end;

procedure TBloco_M.WriteRegistroM400(RegM001: TRegistroM001) ;
  var
    intFor     : integer;
begin
  if Assigned(RegM001.RegistroM400) then
  begin
     for intFor := 0 to RegM001.RegistroM400.Count - 1 do
     begin
        with RegM001.RegistroM400.Items[intFor] do
        begin

          Add( LFill('M400')           +
               LFill( CstPisToStr(CST_PIS) )     +
               LFill( VL_TOT_REC,0,2 ) +
               LFill( COD_CTA )        +
               LFill( DESC_COMPL ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroM410( RegM001.RegistroM400.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM400Count := FRegistroM400Count + RegM001.RegistroM400.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM410(RegM400: TRegistroM400) ;
  var
    intFor: integer;
begin
  if Assigned(RegM400.RegistroM410) then
  begin
     for intFor := 0 to RegM400.RegistroM410.Count - 1 do
     begin
        with RegM400.RegistroM410.Items[intFor] do
        begin

          Add( LFill('M410')        +
               LFill( NAT_REC )     +        //Verificar criação da tabela no ACBrEPCBlocos
               LFill( VL_REC ,0,2 ) +
               LFill( COD_CTA )     +
               LFill( DESC_COMPL ) ) ;
        end;

        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM410Count := FRegistroM410Count + RegM400.RegistroM410.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM500(RegM001: TRegistroM001) ;
  var
    intFor           : integer;
    strIND_CRED_ORI  : String;
    strIND_DESC_CRED : String;
begin
  if Assigned(RegM001.RegistroM500) then
  begin
     for intFor := 0 to RegM001.RegistroM500.Count - 1 do
     begin
        with RegM001.RegistroM500.Items[intFor] do
        begin

          case IND_CRED_ORI of
            icoOperProprias   : strIND_CRED_ORI :='0' ;// 0 // Operações próprias
            icoEvenFusaoCisao : strIND_CRED_ORI :='1' ;// 1 // Evento de incorporação, cisão ou fusão
          end;

          case IND_DESC_CRED of
             idcTotal   : strIND_DESC_CRED :='0' ; // 0 // Utilização do valor total para desconto da contribuição apurada no período, no Registro M200;
             idcParcial : strIND_DESC_CRED :='1' ; // 1 // Utilização de valor parcial para desconto da contribuição apurada no período, no Registro M200
          end;

          Add( LFill('M500')                      +
               LFill( COD_CRED )                  +  //Verificar criação da tabela no ACBrEPCBlocos
               LFill( strIND_CRED_ORI )           +
               VLFill( VL_BC_COFINS, 0, 2 )       +
               VDFill( ALIQ_COFINS, 4)            +
               VLFill( QUANT_BC_COFINS, 3)        +
               VDFill( ALIQ_COFINS_QUANT, 4)      +
               LFill( VL_CRED,0,2 )               +
               LFill( VL_AJUS_ACRES,0,2 )         +
               LFill( VL_AJUS_REDUC,0,2 )         +
               LFill( VL_CRED_DIFER,0,2 )         +
               LFill( VL_CRED_DISP,0,2 )          +
               LFill( strIND_DESC_CRED )          +
               VLFill( VL_CRED_DESC,0,2 )         +
               LFill( SLD_CRED,0,2 ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroM505( RegM001.RegistroM500.Items[intFor] );
        WriteRegistroM510( RegM001.RegistroM500.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM500Count := FRegistroM500Count + RegM001.RegistroM500.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM505(RegM500: TRegistroM500) ;
  var
    intFor         : integer;
    strNAT_BC_CRED : String;
begin
  if Assigned(RegM500.RegistroM505) then
  begin
     for intFor := 0 to RegM500.RegistroM505.Count - 1 do
     begin
        with RegM500.RegistroM505.Items[intFor] do
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

          Add( LFill('M505')                    +
               LFill( strNAT_BC_CRED )          +
               LFill( CstCofinsToStr(CST_COFINS) )           +
               VLFill( VL_BC_COFINS_TOT,0,2 )   +
               VLFill( VL_BC_COFINS_CUM,0,2 )   +
               VLFill( VL_BC_COFINS_NC,0,2 )    +
               VLFill( VL_BC_COFINS,0,2 )       +
               VDFill( QUANT_BC_COFINS_TOT, 3)  +
               VDFill( QUANT_BC_COFINS, 3 )     +
               LFill( DESC_CRED ) ) ;
        end;

        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM505Count := FRegistroM505Count + RegM500.RegistroM505.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM510(RegM500: TRegistroM500) ;
  var
    intFor    : integer;
    strIND_AJ : String;
    strCOD_AJ : String;    
begin
  if Assigned(RegM500.RegistroM510) then
  begin
     for intFor := 0 to RegM500.RegistroM510.Count - 1 do
     begin
        with RegM500.RegistroM510.Items[intFor] do
        begin

          case IND_AJ of
            indAjReducao   : strIND_AJ := '0' ;
            indAjAcressimo : strIND_AJ := '1' ;
          end;

          case COD_AJ of
            codAjAcaoJudicial      : strCOD_AJ := '01'; // Ajuste Oriundo de Ação Judicial
            codAjProAdministrativo : strCOD_AJ := '02'; // Ajuste Oriundo de Processo Administrativo
            codAjLegTributaria     : strCOD_AJ := '03'; // Ajuste Oriundo da Legislação Tributária
            codAjEspRTI            : strCOD_AJ := '04'; // Ajuste Oriundo Especificamente do RTT
            codAjOutrasSituacaoes  : strCOD_AJ := '05'; // Ajuste Oriundo de Outras Situações
            codAjEstorno           : strCOD_AJ := '06'; // Estorno
          end;

          Add( LFill('M510')       +
               LFill( strIND_AJ )  +        
               LFill( VL_AJ ,0,2 ) +
               LFill( strCOD_AJ )  +
               LFill( NUM_DOC )    +
               LFill( DESCR_AJ )   +
               LFill( DT_REF ) ) ;
        end;
        WriteRegistroM515( RegM500.RegistroM510.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM510Count := FRegistroM510Count + RegM500.RegistroM510.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM515(RegM510: TRegistroM510);
var
  intFor    : integer;
begin
  if Assigned(RegM510.RegistroM515) then
  begin
     for intFor := 0 to RegM510.RegistroM515.Count - 1 do
     begin
        with RegM510.RegistroM515.Items[intFor] do
        begin
          Add( LFill('M515')                          +
               LFill( DET_VALOR_AJ, 0, 2 )            +
               LFill( CstCofinsToStr(CST_COFINS), 2,True ) +
               LFill( DET_BC_CRED, 0, 3 )             +
               LFill( DET_ALIQ, 8, 4 )                +
               LFill( DT_OPER_AJ )                    +
               LFill( DESC_AJ )                       +
               LFill( COD_CTA)                    +
               LFill( INFO_COMPL)) ;
        end;
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM515Count := FRegistroM515Count + RegM510.RegistroM515.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM600(RegM001: TRegistroM001) ;
begin
  if Assigned(RegM001.RegistroM600) then
  begin
     with RegM001.RegistroM600 do
     begin
       Add( LFill('M600')                     +
            LFill( VL_TOT_CONT_NC_PER,0,2 )   +
            LFill( VL_TOT_CRED_DESC,0,2 )     +
            LFill( VL_TOT_CRED_DESC_ANT,0,2 ) +
            LFill( VL_TOT_CONT_NC_DEV,0,2 )   +
            LFill( VL_RET_NC,0,2 )            +
            LFill( VL_OUT_DED_NC,0,2 )        +
            LFill( VL_CONT_NC_REC,0,2 )       +
            LFill( VL_TOT_CONT_CUM_PER,0,2 )  +
            LFill( VL_RET_CUM,0,2 )           +
            LFill( VL_OUT_DED_CUM,0,2 )       +
            LFill( VL_CONT_CUM_REC,0,2 )      +
            LFill( VL_TOT_CONT_REC,0,2 ) ) ;
     end;
     // Registros FILHOS
     WriteRegistroM605( RegM001.RegistroM600 );
     WriteRegistroM610( RegM001.RegistroM600 );
     ///
     RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM600Count := FRegistroM600Count + 1;
  end;
end;

procedure TBloco_M.WriteRegistroM605(RegM600: TRegistroM600);
var
  intFor      : integer;
begin
  if Assigned(RegM600.RegistroM605) then
  begin
     for intFor := 0 to RegM600.RegistroM605.Count - 1 do
     begin
        with RegM600.RegistroM605.Items[intFor] do
        begin
          Add( LFill('M605')           +
               LFill( NUM_CAMPO, 2 )   +
               LFill( COD_REC , 6 )    +
               LFill( VL_DEBITO ,0,2 ) ) ;
        end;
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM605Count := FRegistroM605Count + RegM600.RegistroM605.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM610(RegM600: TRegistroM600) ;
var
  intFor      : integer;
  strCOD_CONT : String;
begin
  if Assigned(RegM600.RegistroM610) then
  begin
     for intFor := 0 to RegM600.RegistroM610.Count - 1 do
     begin
        with RegM600.RegistroM610.Items[intFor] do
        begin

          case COD_CONT of
                    ccNaoAcumAliqBasica                 : strCOD_CONT :='01' ; // Contribuição não-cumulativa apurada a alíquota básica
                    ccNaoAcumAliqDiferenciada           : strCOD_CONT :='02' ; // Contribuição não-cumulativa apurada a alíquotas diferenciadas
                    ccNaoAcumAliqUnidProduto            : strCOD_CONT :='03' ; // Contribuição não-cumulativa apurada a alíquota por unidade de medida de produto
                    ccNaoAcumAliqBasicaAtivImobiliaria  : strCOD_CONT :='04' ; // Contribuição não-cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaPorST                      : strCOD_CONT :='31' ; // Contribuição apurada por substituição tributária
                    ccApuradaPorSTManaus                : strCOD_CONT :='32' ; // Contribuição apurada por substituição tributária - Vendas à Zona Franca de Manaus
                    ccAcumAliqBasica                    : strCOD_CONT :='51' ; // Contribuição cumulativa apurada a alíquota básica
                    ccAcumAliqDiferenciada              : strCOD_CONT :='52' ; // Contribuição cumulativa apurada a alíquotas diferenciadas
                    ccAcumAliqUnidProduto               : strCOD_CONT :='53' ; // Contribuição cumulativa apurada a alíquota por unidade de medida de produto
                    ccAcumAliqBasicaAtivImobiliaria     : strCOD_CONT :='54' ; // Contribuição cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaAtivImobiliaria            : strCOD_CONT :='70' ; // Contribuição apurada da Atividade Imobiliária - RET
                    ccApuradaSCPNaoCumulativa           : strCOD_CONT :='71' ; // Contribuição apurada de SCP - Incidência Não Cumulativa
                    ccApuradaSCPCumulativa              : strCOD_CONT :='72' ; // Contribuição apurada de SCP - Incidência Cumulativa
                    ccPISPasepSalarios                  : strCOD_CONT :='99' ; // Contribuição para o PIS/Pasep - Folha de Salários
          end;

          Add( LFill('M610')                      +
               LFill( strCOD_CONT )               +
               LFill( VL_REC_BRT, 0, 2 )          +
               LFill( VL_BC_CONT, 0, 2 )          +
               VDFill( ALIQ_COFINS, 4 )           +
               VDFill( QUANT_BC_COFINS,   3)      +
               VDFill( ALIQ_COFINS_QUANT, 4)      +
               LFill( VL_CONT_APUR,  0, 2 )       +
               LFill( VL_AJUS_ACRES, 0, 2 )       +
               LFill( VL_AJUS_REDUC, 0, 2 )       +
               VLFill( VL_CONT_DIFER, 0, 2 )      +
               VLFill( VL_CONT_DIFER_ANT, 0, 2)   +
               LFill( VL_CONT_PER ,0,2 ) ) ;
        end;

        // Registros FILHOS
        if (Bloco_0.Registro0000.IND_NAT_PJ = indNatPJSocCooperativa) then
          WriteRegistroM611( RegM600.RegistroM610.Items[intFor] );
        WriteRegistroM620( RegM600.RegistroM610.Items[intFor] );
        WriteRegistroM630( RegM600.RegistroM610.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM610Count := FRegistroM610Count + RegM600.RegistroM610.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM611(RegM610: TRegistroM610) ;
  var
    strIND_TIP_COOP : String;
begin
  if Assigned(RegM610.RegistroM611) then
  begin
     with RegM610.RegistroM611 do
     begin
        case IND_TIP_COOP of
          itcProdAgropecuaria: strIND_TIP_COOP :='01' ; // Cooperativa de Produção Agropecuária;
          itcConsumo         : strIND_TIP_COOP :='02' ; // Cooperativa de Consumo;
          itcCredito         : strIND_TIP_COOP :='03' ; // Cooperativa de Crédito;
          itcEletRural       : strIND_TIP_COOP :='04' ; // Cooperativa de Eletrificação Rural;
          itcTransCargas     : strIND_TIP_COOP :='05' ; // Cooperativa de Transporte Rodoviário de Cargas;
          itcMedicos         : strIND_TIP_COOP :='06' ; // Cooperativa de Médicos;
          itcOutras          : strIND_TIP_COOP :='99' ; // Outras.
        end;

       Add( LFill('M611')                        +
            LFill( strIND_TIP_COOP )             +  
            LFill( VL_BC_CONT_ANT_EXC_COOP,0,2 ) +
            LFill( VL_EXC_COOP_GER,0,2 )         +
            LFill( VL_EXC_ESP_COOP,0,2 )         +
            LFill( VL_BC_CONT,0,2 ) ) ;
     end;

     ///
     RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM611Count := FRegistroM611Count + 1;
  end;
end;

procedure TBloco_M.WriteRegistroM620(RegM610: TRegistroM610) ;
var
  intFor    : integer;
  strIND_AJ : String;
  strCOD_AJ : String;
begin
  if Assigned(RegM610.RegistroM620) then
  begin
     for intFor := 0 to RegM610.RegistroM620.Count - 1 do
     begin
        with RegM610.RegistroM620.Items[intFor] do
        begin

          case IND_AJ of
            indAjReducao   : strIND_AJ := '0' ;
            indAjAcressimo : strIND_AJ := '1' ;
          end;

          case COD_AJ of
            codAjAcaoJudicial      : strCOD_AJ := '01'; // Ajuste Oriundo de Ação Judicial
            codAjProAdministrativo : strCOD_AJ := '02'; // Ajuste Oriundo de Processo Administrativo
            codAjLegTributaria     : strCOD_AJ := '03'; // Ajuste Oriundo da Legislação Tributária
            codAjEspRTI            : strCOD_AJ := '04'; // Ajuste Oriundo Especificamente do RTT
            codAjOutrasSituacaoes  : strCOD_AJ := '05'; // Ajuste Oriundo de Outras Situações
            codAjEstorno           : strCOD_AJ := '06'; // Estorno
          end;
                    
          Add( LFill('M620')       +
               LFill( strIND_AJ )  +
               LFill( VL_AJ ,0,2 ) +
               LFill( strCOD_AJ )  +
               LFill( NUM_DOC )    +
               LFill( DESCR_AJ )   +
               LFill( DT_REF ) ) ;
        end;
        WriteRegistroM625( RegM610.RegistroM620.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM620Count := FRegistroM620Count + RegM610.RegistroM620.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM625(RegM620: TRegistroM620);
var
  intFor    : integer;
begin
  if Assigned(RegM620.RegistroM625) then
  begin
     for intFor := 0 to RegM620.RegistroM625.Count - 1 do
     begin
        with RegM620.RegistroM625.Items[intFor] do
        begin
          Add( LFill('M625')                         +
               LFill( DET_VALOR_AJ, 0, 2 )           +
               LFill( CstCofinsToStr( CST_COFINS ) ) +
               LFill( DET_BC_CRED, 0, 3 )            +
               LFill( DET_ALIQ, 8, 4 )               +
               LFill( DT_OPER_AJ )                   +
               LFill( DESC_AJ )                      +
               LFill( COD_CTA)                   +
               LFill( INFO_COMPL)) ;
        end;
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM625Count := FRegistroM625Count + RegM620.RegistroM625.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM630(RegM610: TRegistroM610) ;
  var
    intFor: integer;
begin
  if Assigned(RegM610.RegistroM630) then
  begin
     for intFor := 0 to RegM610.RegistroM630.Count - 1 do
     begin
        with RegM610.RegistroM630.Items[intFor] do
        begin

          Add( LFill('M630')             +
               LFill( CNPJ )             +
               LFill( VL_VEND,0,2 )      +
               LFill( VL_NAO_RECEB,0,2 ) +
               LFill( VL_CONT_DIF,0,2 )  +
               LFill( VL_CRED_DIF,0,2 )  +
               LFill( COD_CRED ) ) ;       //Verificar criação da tabela no ACBrEPCBlocos
        end;

        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM630Count := FRegistroM630Count + RegM610.RegistroM630.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM700(RegM001: TRegistroM001) ;
  var
    intFor           : integer;
    strNAT_CRED_DESC : String;
    strCOD_CONT      : String;
begin
  if Assigned(RegM001.RegistroM700) then
  begin
     for intFor := 0 to RegM001.RegistroM700.Count - 1 do
     begin
        with RegM001.RegistroM700.Items[intFor] do
        begin
          case NAT_CRED_DESC of
            ncdAliqBasica         : strNAT_CRED_DESC :='01' ;// Crédito a Alíquota Básica;
            ncdAliqDiferenciada   : strNAT_CRED_DESC :='02' ;// Crédito a Alíquota Diferenciada;
            ncdAliqUnidProduto    : strNAT_CRED_DESC :='03' ;// Crédito a Alíquota por Unidade de Produto;
            ncdPresAgroindustria  : strNAT_CRED_DESC :='04' ;// Crédito Presumido da Agroindústria.
          end;

          case COD_CONT of
                    ccNaoAcumAliqBasica                 : strCOD_CONT :='01' ; // Contribuição não-cumulativa apurada a alíquota básica
                    ccNaoAcumAliqDiferenciada           : strCOD_CONT :='02' ; // Contribuição não-cumulativa apurada a alíquotas diferenciadas
                    ccNaoAcumAliqUnidProduto            : strCOD_CONT :='03' ; // Contribuição não-cumulativa apurada a alíquota por unidade de medida de produto
                    ccNaoAcumAliqBasicaAtivImobiliaria  : strCOD_CONT :='04' ; // Contribuição não-cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaPorST                      : strCOD_CONT :='31' ; // Contribuição apurada por substituição tributária
                    ccApuradaPorSTManaus                : strCOD_CONT :='32' ; // Contribuição apurada por substituição tributária - Vendas à Zona Franca de Manaus
                    ccAcumAliqBasica                    : strCOD_CONT :='51' ; // Contribuição cumulativa apurada a alíquota básica
                    ccAcumAliqDiferenciada              : strCOD_CONT :='52' ; // Contribuição cumulativa apurada a alíquotas diferenciadas
                    ccAcumAliqUnidProduto               : strCOD_CONT :='53' ; // Contribuição cumulativa apurada a alíquota por unidade de medida de produto
                    ccAcumAliqBasicaAtivImobiliaria     : strCOD_CONT :='54' ; // Contribuição cumulativa apurada a alíquota básica - Atividade Imobiliária
                    ccApuradaAtivImobiliaria            : strCOD_CONT :='70' ; // Contribuição apurada da Atividade Imobiliária - RET
                    ccApuradaSCPNaoCumulativa           : strCOD_CONT :='71' ; // Contribuição apurada de SCP - Incidência Não Cumulativa
                    ccApuradaSCPCumulativa              : strCOD_CONT :='72' ; // Contribuição apurada de SCP - Incidência Cumulativa
                    ccPISPasepSalarios                  : strCOD_CONT :='99' ; // Contribuição para o PIS/Pasep - Folha de Salários
          end;

          Add( LFill('M700')                     + {REG}
               LFill( strCOD_CONT )              + {COD_CONT}             
               LFill( VL_CONT_APUR_DIFER,0,2 )   + {VL_CONT_APUR_DIFER}
               LFill( strNAT_CRED_DESC )         + {NAT_CRED_DESC}      
               LFill( VL_CRED_DESC_DIFER,0,2 )   + {VL_CRED_DESC_DIFER}
               LFill( VL_CONT_DIFER_ANT,0,2 )    + {VL_CONT_DIFER_ANT}
               LFill( PER_APUR )                 + {PER_APUR}
               LFill( DT_RECEB ) ) ;               {DT_RECEB}
        end;
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM700Count := FRegistroM700Count + RegM001.RegistroM700.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM800(RegM001: TRegistroM001) ;
  var
    intFor        : integer;
begin
  if Assigned(RegM001.RegistroM800) then
  begin
     for intFor := 0 to RegM001.RegistroM800.Count - 1 do
     begin
        with RegM001.RegistroM800.Items[intFor] do
        begin

          Add( LFill('M800')           +
               LFill( CstCofinsToStr(CST_COFINS) )  +
               LFill( VL_TOT_REC,0,2 ) +
               LFill( COD_CTA )        +
               LFill( DESC_COMPL ) ) ;
        end;

        // Registros FILHOS
        WriteRegistroM810( RegM001.RegistroM800.Items[intFor] );
        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM800Count := FRegistroM800Count + RegM001.RegistroM800.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM810(RegM800: TRegistroM800) ;
  var
    intFor: integer;
begin
  if Assigned(RegM800.RegistroM810) then
  begin
     for intFor := 0 to RegM800.RegistroM810.Count - 1 do
     begin
        with RegM800.RegistroM810.Items[intFor] do
        begin
          Add( LFill('M810')        +
               LFill( NAT_REC )     +        //Verificar criação da tabela no ACBrEPCBlocos
               LFill( VL_REC ,0,2 ) +
               LFill( COD_CTA )     +
               LFill( DESC_COMPL ) ) ;
        end;

        ///
        RegistroM990.QTD_LIN_M := RegistroM990.QTD_LIN_M + 1;
     end;
     /// Variavél para armazenar a quantidade de registro do tipo.
     FRegistroM810Count := FRegistroM810Count + RegM800.RegistroM810.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM990 ;
begin
  if Assigned(RegistroM990) then
  begin
     with RegistroM990 do
     begin
       QTD_LIN_M := QTD_LIN_M + 1;
       ///
       Add( LFill('M990')     +
            LFill(QTD_LIN_M,0) );
     end;
  end;
end;

end.
