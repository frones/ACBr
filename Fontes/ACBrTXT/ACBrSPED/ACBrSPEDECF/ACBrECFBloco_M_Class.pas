{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					                    2015   Isaque Pinheiro	    	             }
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
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_M_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_M, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_M -

  { TBloco_M }

  TBloco_M = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroM001: TRegistroM001;
    FRegistroM010: TRegistroM010List;
    FRegistroM030: TRegistroM030List;
    FRegistroM990: TRegistroM990;

    FRegistroM300Count: Integer;
    FRegistroM305Count: Integer;
    FRegistroM310Count: Integer;
    FRegistroM312Count: Integer;
    FRegistroM315Count: Integer;
    FRegistroM350Count: Integer;
    FRegistroM355Count: Integer;
    FRegistroM360Count: Integer;
    FRegistroM362Count: Integer;
    FRegistroM365Count: Integer;
    FRegistroM410Count: Integer;
    FRegistroM415Count: Integer;
    FRegistroM500Count: Integer;

    function WriteRegistroM300(RegM030: TRegistroM030): String;
    function WriteRegistroM350(RegM030: TRegistroM030): String;
    function WriteRegistroM410(RegM030: TRegistroM030): String;
    function WriteRegistroM500(RegM030: TRegistroM030): String;

    function WriteRegistroM305(RegM300: TRegistroM300): String;
    function WriteRegistroM310(RegM300: TRegistroM300): String;
    function WriteRegistroM315(RegM300: TRegistroM300): String;

    function WriteRegistroM312(RegM310: TRegistroM310): String;

    function WriteRegistroM355(RegM350: TRegistroM350): String;
    function WriteRegistroM360(RegM350: TRegistroM350): String;
    function WriteRegistroM365(RegM350: TRegistroM350): String;

    function WriteRegistroM362(RegM360: TRegistroM360): String;

    function WriteRegistroM415(RegM410: TRegistroM410): String;

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    constructor Create;
    destructor Destroy;
    procedure LimpaRegistros;

    function WriteRegistroM001: String;
    function WriteRegistroM010: String;
    function WriteRegistroM030: String;
    function WriteRegistroM990: String;

    property RegistroM001: TRegistroM001 read FRegistroM001 write FRegistroM001;
    property RegistroM010: TRegistroM010List read FRegistroM010 write FRegistroM010;
    property RegistroM030: TRegistroM030List read FRegistroM030 write FRegistroM030;
    property RegistroM990: TRegistroM990 read FRegistroM990 write FRegistroM990;

    property RegistroM300Count: Integer read FRegistroM300Count write FRegistroM300Count;
    property RegistroM305Count: Integer read FRegistroM305Count write FRegistroM305Count;
    property RegistroM310Count: Integer read FRegistroM310Count write FRegistroM310Count;
    property RegistroM312Count: Integer read FRegistroM312Count write FRegistroM312Count;
    property RegistroM315Count: Integer read FRegistroM315Count write FRegistroM315Count;
    property RegistroM350Count: Integer read FRegistroM350Count write FRegistroM350Count;
    property RegistroM355Count: Integer read FRegistroM355Count write FRegistroM355Count;
    property RegistroM360Count: Integer read FRegistroM360Count write FRegistroM360Count;
    property RegistroM362Count: Integer read FRegistroM362Count write FRegistroM362Count;
    property RegistroM365Count: Integer read FRegistroM365Count write FRegistroM365Count;
    property RegistroM410Count: Integer read FRegistroM410Count write FRegistroM410Count;
    property RegistroM415Count: Integer read FRegistroM415Count write FRegistroM415Count;
    property RegistroM500Count: Integer read FRegistroM500Count write FRegistroM500Count;
  published
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_M }

constructor TBloco_M.Create;
begin
  inherited;
  FRegistroM001 := TRegistroM001.Create;
  FRegistroM010 := TRegistroM010List.Create;
  FRegistroM030 := TRegistroM030List.Create;
  FRegistroM990 := TRegistroM990.Create;

  FRegistroM990.QTD_LIN := 0;
end;

procedure TBloco_M.CriaRegistros;
begin
   inherited;
   FRegistroM001 := TRegistroM001.Create;
   FRegistroM010 := TRegistroM010List.Create;
   FRegistroM030 := TRegistroM030List.Create;
   FRegistroM990 := TRegistroM990.Create;

   FRegistroM990.QTD_LIN := 0;
end;

destructor TBloco_M.Destroy;
begin
  FRegistroM001.Free;
  FRegistroM010.Free;
  FRegistroM030.Free;
  FRegistroM990.Free;
  inherited;
end;

procedure TBloco_M.LiberaRegistros;
begin
  inherited;
  FRegistroM001.Free;
  FRegistroM010.Free;
  FRegistroM030.Free;
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

function TBloco_M.WriteRegistroM001: String;
begin
  Result := '';

  if Assigned(FRegistroM001) then
  begin
     with FRegistroM001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(J-J001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('M001') +
                 LFill( Integer(IND_DAD), 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroM990.QTD_LIN:= FRegistroM990.QTD_LIN + 1;
     end;
  end;
end;

function TBloco_M.WriteRegistroM010: String;
var
  intFor: integer;
  strRegistroM010: String;
begin
  strRegistroM010 := '';

  if Assigned(RegistroM010) then
  begin
     for intFor := 0 to RegistroM010.Count - 1 do
     begin
        with RegistroM010.Items[intFor] do
        begin
           ///
           strRegistroM010 :=  strRegistroM010 + LFill('M010')            +
                                                 LFill(COD_CTA_B)         +
                                                 LFill(DESC_CTA_LAL)      +
                                                 LFill(DT_AP_LAL)         +
                                                 LFill(COD_LAN_ORIG)      +
                                                 LFill(DESC_LAN_ORIG)     +
                                                 LFill(DT_LIM_LAL)        +
                                                 LFill(COD_TRIBUTO)       +
                                                 LFill(VL_SALDO_INI,2)    +
                                                 LFill(IND_Vl_SALDO_INI)  +
                                                 LFill(CNPJ_SIT_ESP)      +
                                                 Delimitador              +
                                                 #13#10;
        end;
       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
  end;
  Result := strRegistroM010;
end;

function TBloco_M.WriteRegistroM030: String;
var
  intFor: integer;
  strRegistroM030: String;
begin
  strRegistroM030 := '';

  if Assigned(FRegistroM030) then
  begin
     for intFor := 0 to FRegistroM030.Count - 1 do
     begin
        with FRegistroM030.Items[intFor] do
        begin
           ///
           strRegistroM030 :=  strRegistroM030 + LFill('M030')    +
                                                 LFill(DT_INI)    +
                                                 LFill(DT_FIN)    +
                                                 LFill(PER_APUR)  +
                                                 Delimitador      +
                                                 #13#10;
        end;
        // Registros Filhos
        strRegistroM030 := strRegistroM030 +
                           WriteRegistroM300(FRegistroM030.Items[intFor] ) +
                           WriteRegistroM350(FRegistroM030.Items[intFor] ) +
                           WriteRegistroM410(FRegistroM030.Items[intFor] ) +
                           WriteRegistroM500(FRegistroM030.Items[intFor] );

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
  end;
  Result := strRegistroM030;
end;

function TBloco_M.WriteRegistroM300(RegM030: TRegistroM030): String;
var
  intFor: integer;
  strRegistroM300: String;
begin
  strRegistroM300 := '';

  if Assigned(RegM030.RegistroM300) then
  begin
     for intFor := 0 to RegM030.RegistroM300.Count - 1 do
     begin
        with RegM030.RegistroM300.Items[intFor] do
        begin
           ///
           strRegistroM300 :=  strRegistroM300 + LFill('M300')          +
                                                 LFill(DESCRICAO)       +
                                                 LFill(DESCRICAO)       +
                                                 LFill(TIPO_LANCAMENTO) +
                                                 LFill(IND_RELACAO)     +
                                                 LFill(VALOR,2)         +
                                                 LFill(HIST_LAN_LAL)    +
                                                 Delimitador            +
                                                 #13#10;
        end;
        //REGISTROS FILHOS
        strRegistroM300 := strRegistroM300 +
                           WriteRegistroM305(RegM030.RegistroM300.Items[intFor] ) +
                           WriteRegistroM310(RegM030.RegistroM300.Items[intFor] ) +
                           WriteRegistroM315(RegM030.RegistroM300.Items[intFor] );

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM300Count := FRegistroM300Count + RegM030.RegistroM300.Count;
  end;
  Result := strRegistroM300;
end;

function TBloco_M.WriteRegistroM305(RegM300: TRegistroM300): String;
var
  intFor: integer;
  strRegistroM305: String;
begin
  strRegistroM305 := '';

  if Assigned(RegM300.RegistroM305) then
  begin
     for intFor := 0 to RegM300.RegistroM305.Count - 1 do
     begin
        with RegM300.RegistroM305.Items[intFor] do
        begin
           ///
           strRegistroM305 :=  strRegistroM305 + LFill('M305')        +
                                                 LFill(COD_CTA_B)     +
                                                 LFill(VL_CTA,2)      +
                                                 LFill(IND_VL_CTA)    +
                                                 Delimitador          +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM305Count := FRegistroM305Count + RegM300.RegistroM305.Count;
  end;
  Result := strRegistroM305;
end;

function TBloco_M.WriteRegistroM310(RegM300: TRegistroM300): String;
var
  intFor: integer;
  strRegistroM310: String;
begin
  strRegistroM310 := '';

  if Assigned(RegM300.RegistroM310) then
  begin
     for intFor := 0 to RegM300.RegistroM310.Count - 1 do
     begin
        with RegM300.RegistroM310.Items[intFor] do
        begin
           ///
           strRegistroM310 :=  strRegistroM310 + LFill('M310')      +
                                                 LFill(COD_CTA)     +
                                                 LFill(COD_CCUS)    +
                                                 LFill(VL_CTA,2)    +
                                                 LFill(IND_VL_CTA)  +
                                                 Delimitador        +
                                                 #13#10;
        end;
        //REGISTROS FILHOS
        strRegistroM310 := strRegistroM310 +
                           WriteRegistroM312(RegM300.RegistroM310.Items[intFor] );

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM310Count := FRegistroM310Count + RegM300.RegistroM310.Count;
  end;
  Result := strRegistroM310;
end;

function TBloco_M.WriteRegistroM312(RegM310: TRegistroM310): String;
var
  intFor: integer;
  strRegistroM312: String;
begin
  strRegistroM312 := '';

  if Assigned(RegM310.RegistroM312) then
  begin
     for intFor := 0 to RegM310.RegistroM312.Count - 1 do
     begin
        with RegM310.RegistroM312.Items[intFor] do
        begin
           ///
           strRegistroM312 :=  strRegistroM312 + LFill('M312')    +
                                                 LFill(NUM_LCTO)  +
                                                 Delimitador      +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM312Count := FRegistroM312Count + RegM310.RegistroM312.Count;
  end;
  Result := strRegistroM312;
end;

function TBloco_M.WriteRegistroM315(RegM300: TRegistroM300): String;
var
  intFor: integer;
  strRegistroM315: String;
begin
  strRegistroM315 := '';

  if Assigned(RegM300.RegistroM315) then
  begin
     for intFor := 0 to RegM300.RegistroM315.Count - 1 do
     begin
        with RegM300.RegistroM315.Items[intFor] do
        begin
           ///
           strRegistroM315 :=  strRegistroM315 + LFill('M315')    +
                                                 LFill(IND_PROC)  +
                                                 LFill(NUM_PROC)  +
                                                 Delimitador      +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM315Count := FRegistroM315Count + RegM300.RegistroM315.Count;
  end;
  Result := strRegistroM315;
end;

function TBloco_M.WriteRegistroM350(RegM030: TRegistroM030): String;
var
  intFor: integer;
  strRegistroM350: String;
begin
  strRegistroM350 := '';

  if Assigned(RegM030.RegistroM350) then
  begin
     for intFor := 0 to RegM030.RegistroM350.Count - 1 do
     begin
        with RegM030.RegistroM350.Items[intFor] do
        begin
           ///
           strRegistroM350 :=  strRegistroM350 + LFill('M350')          +
                                                 LFill(CODIGO)          +
                                                 LFill(DESCRICAO)       +
                                                 LFill(TIPO_LANCAMENTO) +
                                                 LFill(IND_RELACAO)     +
                                                 LFill(VALOR,2)         +
                                                 LFill(HIST_LAN_LAL)    +
                                                 Delimitador            +
                                                 #13#10;
        end;
        //REGISTROS FILHOS
        strRegistroM350 := strRegistroM350 +
                           WriteRegistroM355(RegM030.RegistroM350.Items[intFor] ) +
                           WriteRegistroM360(RegM030.RegistroM350.Items[intFor] ) +
                           WriteRegistroM365(RegM030.RegistroM350.Items[intFor] );

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM350Count := FRegistroM350Count + RegM030.RegistroM350.Count;
  end;
  Result := strRegistroM350;
end;

function TBloco_M.WriteRegistroM355(RegM350: TRegistroM350): String;
var
  intFor: integer;
  strRegistroM355: String;
begin
  strRegistroM355 := '';

  if Assigned(RegM350.RegistroM355) then
  begin
     for intFor := 0 to RegM350.RegistroM355.Count - 1 do
     begin
        with RegM350.RegistroM355.Items[intFor] do
        begin
           ///
           strRegistroM355 :=  strRegistroM355 + LFill('M355')      +
                                                 LFill(COD_CTA_B)   +
                                                 LFill(VL_CTA,2)    +
                                                 LFill(IND_VL_CTA)  +
                                                 Delimitador        +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM355Count := FRegistroM355Count + RegM350.RegistroM355.Count;
  end;
  Result := strRegistroM355;
end;

function TBloco_M.WriteRegistroM360(RegM350: TRegistroM350): String;
var
  intFor: integer;
  strRegistroM360: String;
begin
  strRegistroM360 := '';

  if Assigned(RegM350.RegistroM360) then
  begin
     for intFor := 0 to RegM350.RegistroM360.Count - 1 do
     begin
        with RegM350.RegistroM360.Items[intFor] do
        begin
           ///
           strRegistroM360 :=  strRegistroM360 + LFill('M360')      +
                                                 LFill(COD_CTA)     +
                                                 LFill(COD_CCUS)    +
                                                 LFill(VL_CTA,2)    +
                                                 LFill(IND_VL_CTA)  +
                                                 Delimitador        +
                                                 #13#10;
        end;
        //REGISTROS FILHOS
        strRegistroM360 := strRegistroM360 +
                           WriteRegistroM362(RegM350.RegistroM360.Items[intFor] );

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM360Count := FRegistroM360Count + RegM350.RegistroM360.Count;
  end;
  Result := strRegistroM360;
end;

function TBloco_M.WriteRegistroM362(RegM360: TRegistroM360): String;
var
  intFor: integer;
  strRegistroM362: String;
begin
  strRegistroM362 := '';

  if Assigned(RegM360.RegistroM362) then
  begin
     for intFor := 0 to RegM360.RegistroM362.Count - 1 do
     begin
        with RegM360.RegistroM362.Items[intFor] do
        begin
           ///
           strRegistroM362 :=  strRegistroM362 + LFill('M362')    +
                                                 LFill(NUM_LCTO)  +
                                                 Delimitador      +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM362Count := FRegistroM362Count + RegM360.RegistroM362.Count;
  end;
  Result := strRegistroM362;
end;

function TBloco_M.WriteRegistroM365(RegM350: TRegistroM350): String;
var
  intFor: integer;
  strRegistroM365: String;
begin
  strRegistroM365 := '';

  if Assigned(RegM350.RegistroM365) then
  begin
     for intFor := 0 to RegM350.RegistroM365.Count - 1 do
     begin
        with RegM350.RegistroM365.Items[intFor] do
        begin
           ///
           strRegistroM365 :=  strRegistroM365 + LFill('M365')    +
                                                 LFill(IND_PROC)  +
                                                 LFill(NUM_PROC)  +
                                                 Delimitador      +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM365Count := FRegistroM365Count + RegM350.RegistroM365.Count;
  end;
  Result := strRegistroM365;
end;

function TBloco_M.WriteRegistroM410(RegM030: TRegistroM030): String;
var
  intFor: integer;
  strRegistroM410: String;
begin
  strRegistroM410 := '';

  if Assigned(RegM030.RegistroM410) then
  begin
     for intFor := 0 to RegM030.RegistroM410.Count - 1 do
     begin
        with RegM030.RegistroM410.Items[intFor] do
        begin
           ///
           strRegistroM410 :=  strRegistroM410 + LFill('M410')              +
                                                 LFill(COD_CTA_B)           +
                                                 LFill(COD_TRIBUTO)         +
                                                 LFill(VAL_LAN_LALB_PB,2)   +
                                                 LFill(IND_VAL_LAN_LALB_PB) +
                                                 LFill(COD_CTA_B_CTP)       +
                                                 LFill(HIST_LAN_LALB)       +
                                                 LFill(IND_LAN_ANT)         +
                                                 Delimitador                +
                                                 #13#10;
        end;
        //REGISTROS FILHOS
        strRegistroM410 := strRegistroM410 +
                           WriteRegistroM415(RegM030.RegistroM410.Items[intFor] );

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM410Count := FRegistroM410Count + RegM030.RegistroM410.Count;
  end;
  Result := strRegistroM410;
end;

function TBloco_M.WriteRegistroM415(RegM410: TRegistroM410): String;
var
  intFor: integer;
  strRegistroM415: String;
begin
  strRegistroM415 := '';

  if Assigned(RegM410.RegistroM415) then
  begin
     for intFor := 0 to RegM410.RegistroM415.Count - 1 do
     begin
        with RegM410.RegistroM415.Items[intFor] do
        begin
           ///
           strRegistroM415 :=  strRegistroM415 + LFill('M415')    +
                                                 LFill(IND_PROC)  +
                                                 LFill(NUM_PROC)  +
                                                 Delimitador      +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM415Count := FRegistroM415Count + RegM410.RegistroM415.Count;
  end;
  Result := strRegistroM415;
end;

function TBloco_M.WriteRegistroM500(RegM030: TRegistroM030): String;
var
  intFor: integer;
  strRegistroM500: String;
begin
  strRegistroM500 := '';

  if Assigned(RegM030.RegistroM500) then
  begin
     for intFor := 0 to RegM030.RegistroM500.Count - 1 do
     begin
        with RegM030.RegistroM500.Items[intFor] do
        begin
           ///
           strRegistroM500 :=  strRegistroM500 + LFill('M500')              +
                                                 LFill(COD_CTA_B)           +
                                                 LFill(COD_TRIBUTO)         +
                                                 LFill(SD_INI_LAL,2)        +
                                                 LFill(IND_SD_INI_LAL)      +
                                                 LFill(VL_LCTO_PARTE_A,2)   +
                                                 LFill(IND_VL_LCTO_PARTE_A) +
                                                 LFill(VL_LCTO_PARTE_B,2)   +
                                                 LFill(IND_VL_LCTO_PARTE_B) +
                                                 LFill(SD_FIM_LAL,2)        +
                                                 LFill(IND_SD_FIM_LAL)      +
                                                 Delimitador                +
                                                 #13#10;
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM500Count := FRegistroM500Count + RegM030.RegistroM500.Count;
  end;
  Result := strRegistroM500;
end;

function TBloco_M.WriteRegistroM990: String;
begin
  Result := '';

  if Assigned(FRegistroM990) then
  begin
     with FRegistroM990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Result := LFill('M990') +
                 LFill(QTD_LIN, 0) +
                 Delimitador +
                 #13#10;
     end;
  end;
end;

end.
