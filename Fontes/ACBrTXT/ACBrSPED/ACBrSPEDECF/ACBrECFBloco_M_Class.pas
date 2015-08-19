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

    procedure WriteRegistroM300(RegM030: TRegistroM030);
    procedure WriteRegistroM350(RegM030: TRegistroM030);
    procedure WriteRegistroM410(RegM030: TRegistroM030);
    procedure WriteRegistroM500(RegM030: TRegistroM030);

    procedure WriteRegistroM305(RegM300: TRegistroM300);
    procedure WriteRegistroM310(RegM300: TRegistroM300);
    procedure WriteRegistroM315(RegM300: TRegistroM300);

    procedure WriteRegistroM312(RegM310: TRegistroM310);

    procedure WriteRegistroM355(RegM350: TRegistroM350);
    procedure WriteRegistroM360(RegM350: TRegistroM350);
    procedure WriteRegistroM365(RegM350: TRegistroM350);

    procedure WriteRegistroM362(RegM360: TRegistroM360);

    procedure WriteRegistroM415(RegM410: TRegistroM410);

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    constructor Create;
    destructor Destroy;
    procedure LimpaRegistros;

    procedure WriteRegistroM001;
    procedure WriteRegistroM010;
    procedure WriteRegistroM030;
    procedure WriteRegistroM990;

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

procedure TBloco_M.WriteRegistroM001;
begin
  if Assigned(FRegistroM001) then
  begin
     with FRegistroM001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(M-M001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add(LFill('M001') +
           LFill( Integer(IND_DAD), 1));
       ///
       FRegistroM990.QTD_LIN:= FRegistroM990.QTD_LIN + 1;
       WriteRegistroM010;
       WriteRegistroM030;
     end;
  end;
end;

procedure TBloco_M.WriteRegistroM010;
var
  intFor: integer;
begin
  if Assigned(RegistroM010) then
  begin
     for intFor := 0 to RegistroM010.Count - 1 do
     begin
        with RegistroM010.Items[intFor] do
        begin
           ///
           Add(LFill('M010')            +
               LFill(COD_CTA_B)         +
               LFill(DESC_CTA_LAL)      +
               LFill(DT_AP_LAL)         +
               LFill(COD_LAN_ORIG)      +
               LFill(DESC_LAN_ORIG)     +
               LFill(DT_LIM_LAL)        +
               LFill(COD_TRIBUTO)       +
              VLFill(VL_SALDO_INI,19,2) +
               LFill(IND_Vl_SALDO_INI)  +
               LFill(CNPJ_SIT_ESP));
        end;
       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_M.WriteRegistroM030;
var
  intFor: integer;
begin
  if Assigned(FRegistroM030) then
  begin
     for intFor := 0 to FRegistroM030.Count - 1 do
     begin
        with FRegistroM030.Items[intFor] do
        begin
           ///
           Add(LFill('M030')    +
               LFill(DT_INI)    +
               LFill(DT_FIN)    +
               LFill(PER_APUR));
        end;
        // Registros Filhos
        WriteRegistroM300(FRegistroM030.Items[intFor]);
        WriteRegistroM350(FRegistroM030.Items[intFor]);
        WriteRegistroM410(FRegistroM030.Items[intFor]);
        WriteRegistroM500(FRegistroM030.Items[intFor]);

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_M.WriteRegistroM300(RegM030: TRegistroM030);
var
  intFor: integer;
begin
  if Assigned(RegM030.RegistroM300) then
  begin
     for intFor := 0 to RegM030.RegistroM300.Count - 1 do
     begin
        with RegM030.RegistroM300.Items[intFor] do
        begin
           ///
           Add(LFill('M300')          +
               LFill(DESCRICAO)       +
               LFill(DESCRICAO)       +
               LFill(TIPO_LANCAMENTO) +
               LFill(IND_RELACAO)     +
              VLFill(VALOR,19,2)      +
               LFill(HIST_LAN_LAL));
        end;
        //REGISTROS FILHOS
        WriteRegistroM305(RegM030.RegistroM300.Items[intFor] );
        WriteRegistroM310(RegM030.RegistroM300.Items[intFor] );
        WriteRegistroM315(RegM030.RegistroM300.Items[intFor] );

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM300Count := FRegistroM300Count + RegM030.RegistroM300.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM305(RegM300: TRegistroM300);
var
  intFor: integer;
begin
  if Assigned(RegM300.RegistroM305) then
  begin
     for intFor := 0 to RegM300.RegistroM305.Count - 1 do
     begin
        with RegM300.RegistroM305.Items[intFor] do
        begin
           ///
           Add(LFill('M305')        +
               LFill(COD_CTA_B)     +
               LFill(VL_CTA, 19, 2) +
               LFill(IND_VL_CTA));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM305Count := FRegistroM305Count + RegM300.RegistroM305.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM310(RegM300: TRegistroM300);
var
  intFor: integer;
begin
  if Assigned(RegM300.RegistroM310) then
  begin
     for intFor := 0 to RegM300.RegistroM310.Count - 1 do
     begin
        with RegM300.RegistroM310.Items[intFor] do
        begin
           ///
           Add(LFill('M310')     +
               LFill(COD_CTA)    +
               LFill(COD_CCUS)   +
              VLFill(VL_CTA,19,2) +
               LFill(IND_VL_CTA));
        end;
        //REGISTROS FILHOS
        WriteRegistroM312(RegM300.RegistroM310.Items[intFor]);

        FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;

     FRegistroM310Count := FRegistroM310Count + RegM300.RegistroM310.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM312(RegM310: TRegistroM310);
var
  intFor: integer;
begin
  if Assigned(RegM310.RegistroM312) then
  begin
     for intFor := 0 to RegM310.RegistroM312.Count - 1 do
     begin
        with RegM310.RegistroM312.Items[intFor] do
        begin
           ///
           Add(LFill('M312')    +
               LFill(NUM_LCTO));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM312Count := FRegistroM312Count + RegM310.RegistroM312.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM315(RegM300: TRegistroM300);
var
  intFor: integer;
begin
  if Assigned(RegM300.RegistroM315) then
  begin
     for intFor := 0 to RegM300.RegistroM315.Count - 1 do
     begin
        with RegM300.RegistroM315.Items[intFor] do
        begin
           ///
           Add(LFill('M315')   +
               LFill(IND_PROC) +
               LFill(NUM_PROC));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM315Count := FRegistroM315Count + RegM300.RegistroM315.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM350(RegM030: TRegistroM030);
var
  intFor: integer;
begin
  if Assigned(RegM030.RegistroM350) then
  begin
     for intFor := 0 to RegM030.RegistroM350.Count - 1 do
     begin
        with RegM030.RegistroM350.Items[intFor] do
        begin
           ///
           Add(LFill('M350')          +
               LFill(CODIGO)          +
               LFill(DESCRICAO)       +
               LFill(TIPO_LANCAMENTO) +
               LFill(IND_RELACAO)     +
              VLFill(VALOR,19,2)      +
               LFill(HIST_LAN_LAL));
        end;

        //REGISTROS FILHOS

        WriteRegistroM355(RegM030.RegistroM350.Items[intFor] );
        WriteRegistroM360(RegM030.RegistroM350.Items[intFor] );
        WriteRegistroM365(RegM030.RegistroM350.Items[intFor] );

        FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM350Count := FRegistroM350Count + RegM030.RegistroM350.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM355(RegM350: TRegistroM350);
var
  intFor: integer;
begin
  if Assigned(RegM350.RegistroM355) then
  begin
     for intFor := 0 to RegM350.RegistroM355.Count - 1 do
     begin
        with RegM350.RegistroM355.Items[intFor] do
        begin
           ///
           Add(LFill('M355')      +
               LFill(COD_CTA_B)   +
              VLFill(VL_CTA,19,2) +
               LFill(IND_VL_CTA));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM355Count := FRegistroM355Count + RegM350.RegistroM355.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM360(RegM350: TRegistroM350);
var
  intFor: integer;
  
begin
  if Assigned(RegM350.RegistroM360) then
  begin
     for intFor := 0 to RegM350.RegistroM360.Count - 1 do
     begin
        with RegM350.RegistroM360.Items[intFor] do
        begin
           ///
           Add(LFill('M360')      +
               LFill(COD_CTA)     +
               LFill(COD_CCUS)    +
              VLFill(VL_CTA,19,2) +
               LFill(IND_VL_CTA));
        end;

        //REGISTROS FILHOS
        WriteRegistroM362(RegM350.RegistroM360.Items[intFor] );

        FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM360Count := FRegistroM360Count + RegM350.RegistroM360.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM362(RegM360: TRegistroM360);
var
  intFor: integer;
begin
  if Assigned(RegM360.RegistroM362) then
  begin
     for intFor := 0 to RegM360.RegistroM362.Count - 1 do
     begin
        with RegM360.RegistroM362.Items[intFor] do
        begin
           ///
           Add(LFill('M362')    +
               LFill(NUM_LCTO));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM362Count := FRegistroM362Count + RegM360.RegistroM362.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM365(RegM350: TRegistroM350);
var
  intFor: integer;
begin
  if Assigned(RegM350.RegistroM365) then
  begin
     for intFor := 0 to RegM350.RegistroM365.Count - 1 do
     begin
        with RegM350.RegistroM365.Items[intFor] do
        begin
           ///
           Add(LFill('M365')    +
               LFill(IND_PROC)  +
               LFill(NUM_PROC));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM365Count := FRegistroM365Count + RegM350.RegistroM365.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM410(RegM030: TRegistroM030);
var
  intFor: integer;
begin
  if Assigned(RegM030.RegistroM410) then
  begin
     for intFor := 0 to RegM030.RegistroM410.Count - 1 do
     begin
        with RegM030.RegistroM410.Items[intFor] do
        begin
           ///
           Add(LFill('M410')               +
               LFill(COD_CTA_B)            +
               LFill(COD_TRIBUTO)          +
              VLFill(VAL_LAN_LALB_PB,19,2) +
               LFill(IND_VAL_LAN_LALB_PB)  +
               LFill(COD_CTA_B_CTP)        +
               LFill(HIST_LAN_LALB)        +
               LFill(IND_LAN_ANT));
        end;

        //REGISTROS FILHOS
        WriteRegistroM415(RegM030.RegistroM410.Items[intFor] );

        FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM410Count := FRegistroM410Count + RegM030.RegistroM410.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM415(RegM410: TRegistroM410);
var
  intFor: integer;
begin
  if Assigned(RegM410.RegistroM415) then
  begin
     for intFor := 0 to RegM410.RegistroM415.Count - 1 do
     begin
        with RegM410.RegistroM415.Items[intFor] do
        begin
           ///
           Add(LFill('M415')    +
               LFill(IND_PROC)  +
               LFill(NUM_PROC));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM415Count := FRegistroM415Count + RegM410.RegistroM415.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM500(RegM030: TRegistroM030);
var
  intFor: integer;
begin
  if Assigned(RegM030.RegistroM500) then
  begin
     for intFor := 0 to RegM030.RegistroM500.Count - 1 do
     begin
        with RegM030.RegistroM500.Items[intFor] do
        begin
           ///
           Add(LFill('M500')               +
               LFill(COD_CTA_B)            +
               LFill(COD_TRIBUTO)          +
              VLFill(SD_INI_LAL,19,2)      +
               LFill(IND_SD_INI_LAL)       +
              VLFill(VL_LCTO_PARTE_A,19,2) +
               LFill(IND_VL_LCTO_PARTE_A)  +
              VLFill(VL_LCTO_PARTE_B,19,2) +
               LFill(IND_VL_LCTO_PARTE_B)  +
              VLFill(SD_FIM_LAL,19,2)      +
               LFill(IND_SD_FIM_LAL));
        end;

       FRegistroM990.QTD_LIN := FRegistroM990.QTD_LIN + 1;
     end;
     FRegistroM500Count := FRegistroM500Count + RegM030.RegistroM500.Count;
  end;
end;

procedure TBloco_M.WriteRegistroM990;
begin
  if Assigned(FRegistroM990) then
  begin
     with FRegistroM990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Add(LFill('M990') +
           LFill(QTD_LIN, 0));
     end;
  end;
end;

end.
