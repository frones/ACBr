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

unit ACBrECFBloco_X_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_X, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_X -

  { TBloco_X }

  TBloco_X = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroX001: TRegistroX001;
    FRegistroX990: TRegistroX990;

    FRegistroX280: TRegistroX280List;
    FRegistroX291: TRegistroX291List;
    FRegistroX292: TRegistroX292List;
    FRegistroX300: TRegistroX300List;
    FRegistroX320: TRegistroX320List;
    FRegistroX340: TRegistroX340List;
    FRegistroX390: TRegistroX390List;
    FRegistroX400: TRegistroX400List;
    FRegistroX410: TRegistroX410List;
    FRegistroX420: TRegistroX420List;
    FRegistroX430: TRegistroX430List;
    FRegistroX450: TRegistroX450List;
    FRegistroX460: TRegistroX460List;
    FRegistroX470: TRegistroX470List;
    FRegistroX480: TRegistroX480List;
    FRegistroX490: TRegistroX490List;
    FRegistroX500: TRegistroX500List;
    FRegistroX510: TRegistroX510List;

    FRegistroX310Count: Integer;

    FRegistroX330Count: Integer;

    FRegistroX350Count: Integer;
    FRegistroX351Count: Integer;
    FRegistroX352Count: Integer;
    FRegistroX353Count: Integer;
    FRegistroX354Count: Integer;
    FRegistroX355Count: Integer;
    FRegistroX356Count: Integer;

    procedure WriteRegistroX310(RegX300: TRegistroX300);

    procedure WriteRegistroX330(RegX320: TRegistroX320);

    procedure WriteRegistroX350(RegX340: TRegistroX340);
    procedure WriteRegistroX351(RegX340: TRegistroX340);
    procedure WriteRegistroX352(RegX340: TRegistroX340);
    procedure WriteRegistroX353(RegX340: TRegistroX340);
    procedure WriteRegistroX354(RegX340: TRegistroX340);
    procedure WriteRegistroX355(RegX340: TRegistroX340);
    procedure WriteRegistroX356(RegX340: TRegistroX340);

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    constructor Create;
    destructor Destroy;
    procedure LimpaRegistros;

    procedure WriteRegistroX001;
    procedure WriteRegistroX280;
    procedure WriteRegistroX291;
    procedure WriteRegistroX292;
    procedure WriteRegistroX300;
    procedure WriteRegistroX320;
    procedure WriteRegistroX340;
    procedure WriteRegistroX390;
    procedure WriteRegistroX400;
    procedure WriteRegistroX410;
    procedure WriteRegistroX420;
    procedure WriteRegistroX430;
    procedure WriteRegistroX450;
    procedure WriteRegistroX460;
    procedure WriteRegistroX470;
    procedure WriteRegistroX480;
    procedure WriteRegistroX490;
    procedure WriteRegistroX500;
    procedure WriteRegistroX510;

    procedure WriteRegistroX990;

    property RegistroX001: TRegistroX001 read FRegistroX001 write FRegistroX001;

    property RegistroX280: TRegistroX280List read FRegistroX280 write FRegistroX280;
    property RegistroX291: TRegistroX291List read FRegistroX291 write FRegistroX291;
    property RegistroX292: TRegistroX292List read FRegistroX292 write FRegistroX292;
    property RegistroX300: TRegistroX300List read FRegistroX300 write FRegistroX300;
    property RegistroX320: TRegistroX320List read FRegistroX320 write FRegistroX320;
    property RegistroX340: TRegistroX340List read FRegistroX340 write FRegistroX340;
    property RegistroX390: TRegistroX390List read FRegistroX390 write FRegistroX390;
    property RegistroX400: TRegistroX400List read FRegistroX400 write FRegistroX400;
    property RegistroX410: TRegistroX410List read FRegistroX410 write FRegistroX410;
    property RegistroX420: TRegistroX420List read FRegistroX420 write FRegistroX420;
    property RegistroX430: TRegistroX430List read FRegistroX430 write FRegistroX430;
    property RegistroX450: TRegistroX450List read FRegistroX450 write FRegistroX450;
    property RegistroX460: TRegistroX460List read FRegistroX460 write FRegistroX460;
    property RegistroX470: TRegistroX470List read FRegistroX470 write FRegistroX470;
    property RegistroX480: TRegistroX480List read FRegistroX480 write FRegistroX480;
    property RegistroX490: TRegistroX490List read FRegistroX490 write FRegistroX490;
    property RegistroX500: TRegistroX500List read FRegistroX500 write FRegistroX500;
    property RegistroX510: TRegistroX510List read FRegistroX510 write FRegistroX510;

    property RegistroX990: TRegistroX990 read FRegistroX990 write FRegistroX990;

    property RegistroX310Count: Integer read FRegistroX310Count write FRegistroX310Count;

    property RegistroX330Count: Integer read FRegistroX330Count write FRegistroX330Count;

    property RegistroX350Count: Integer read FRegistroX350Count write FRegistroX350Count;
    property RegistroX351Count: Integer read FRegistroX351Count write FRegistroX351Count;
    property RegistroX352Count: Integer read FRegistroX352Count write FRegistroX352Count;
    property RegistroX353Count: Integer read FRegistroX353Count write FRegistroX353Count;
    property RegistroX354Count: Integer read FRegistroX354Count write FRegistroX354Count;
    property RegistroX355Count: Integer read FRegistroX355Count write FRegistroX355Count;
    property RegistroX356Count: Integer read FRegistroX356Count write FRegistroX356Count;
  published
  end;

implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_X }

constructor TBloco_X.Create;
begin
  FRegistroX001 := TRegistroX001.Create;
  FRegistroX990 := TRegistroX990.Create;

  FRegistroX280 := TRegistroX280List.Create;
  FRegistroX291 := TRegistroX291List.Create;
  FRegistroX292 := TRegistroX292List.Create;
  FRegistroX300 := TRegistroX300List.Create;
  FRegistroX320 := TRegistroX320List.Create;
  FRegistroX340 := TRegistroX340List.Create;
  FRegistroX390 := TRegistroX390List.Create;
  FRegistroX400 := TRegistroX400List.Create;
  FRegistroX410 := TRegistroX410List.Create;
  FRegistroX420 := TRegistroX420List.Create;
  FRegistroX430 := TRegistroX430List.Create;
  FRegistroX450 := TRegistroX450List.Create;
  FRegistroX460 := TRegistroX460List.Create;
  FRegistroX470 := TRegistroX470List.Create;
  FRegistroX480 := TRegistroX480List.Create;
  FRegistroX490 := TRegistroX490List.Create;
  FRegistroX500 := TRegistroX500List.Create;
  FRegistroX510 := TRegistroX510List.Create;

  FRegistroX990.QTD_LIN := 0;
end;

procedure TBloco_X.CriaRegistros;
begin
  inherited;

  FRegistroX001 := TRegistroX001.Create;
  FRegistroX990 := TRegistroX990.Create;

  FRegistroX280 := TRegistroX280List.Create;
  FRegistroX291 := TRegistroX291List.Create;
  FRegistroX292 := TRegistroX292List.Create;
  FRegistroX300 := TRegistroX300List.Create;
  FRegistroX320 := TRegistroX320List.Create;
  FRegistroX340 := TRegistroX340List.Create;
  FRegistroX390 := TRegistroX390List.Create;
  FRegistroX400 := TRegistroX400List.Create;
  FRegistroX410 := TRegistroX410List.Create;
  FRegistroX420 := TRegistroX420List.Create;
  FRegistroX430 := TRegistroX430List.Create;
  FRegistroX450 := TRegistroX450List.Create;
  FRegistroX460 := TRegistroX460List.Create;
  FRegistroX470 := TRegistroX470List.Create;
  FRegistroX480 := TRegistroX480List.Create;
  FRegistroX490 := TRegistroX490List.Create;
  FRegistroX500 := TRegistroX500List.Create;
  FRegistroX510 := TRegistroX510List.Create;

  FRegistroX990.QTD_LIN := 0;
end;

destructor TBloco_X.Destroy;
begin
  FRegistroX001.Free;
  FRegistroX990.Free;

  FRegistroX280.Free;
  FRegistroX291.Free;
  FRegistroX292.Free;
  FRegistroX300.Free;
  FRegistroX320.Free;
  FRegistroX340.Free;
  FRegistroX390.Free;
  FRegistroX400.Free;
  FRegistroX410.Free;
  FRegistroX420.Free;
  FRegistroX430.Free;
  FRegistroX450.Free;
  FRegistroX460.Free;
  FRegistroX470.Free;
  FRegistroX480.Free;
  FRegistroX490.Free;
  FRegistroX500.Free;
  FRegistroX510.Free;

  inherited;
end;

procedure TBloco_X.LiberaRegistros;
begin
   inherited;

   FRegistroX001.Free;
   FRegistroX990.Free;
end;

procedure TBloco_X.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_X.WriteRegistroX001;
begin
  if Assigned(FRegistroX001) then
  begin
     with FRegistroX001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(X-X001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Add(LFill('X001') +
           LFill( Integer(IND_DAD), 1));
       ///
       FRegistroX990.QTD_LIN:= FRegistroX990.QTD_LIN + 1;
     end;

     WriteRegistroX280;
     WriteRegistroX291;
     WriteRegistroX292;
     WriteRegistroX300;
     WriteRegistroX320;
     WriteRegistroX340;
     WriteRegistroX390;
     WriteRegistroX400;
     WriteRegistroX410;
     WriteRegistroX420;
     WriteRegistroX430;
     WriteRegistroX450;
     WriteRegistroX460;
     WriteRegistroX470;
     WriteRegistroX480;
     WriteRegistroX490;
     WriteRegistroX500;
     WriteRegistroX510;
  end;
end;

procedure TBloco_X.WriteRegistroX280;
var
  intFor: integer;
begin
  if Assigned(FRegistroX280) then
  begin
     for intFor := 0 to FRegistroX280.Count - 1 do
     begin
        with FRegistroX280.Items[intFor] do
        begin
           ///
           Add(LFill('X280')      +
               LFill(IND_ATIV,2)  +
               LFill(IND_PROJ,2)  +
               LFill(ATO_CONC)    +
               LFill(VIG_INI)     +
               LFill(VIG_FIM));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX291;
var
  intFor: integer;
begin
  if Assigned(FRegistroX291) then
  begin
     for intFor := 0 to FRegistroX291.Count - 1 do
     begin
        with FRegistroX291.Items[intFor] do
        begin
           ///
           Add(LFill('X291')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19));
        end;
        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX292;
var
  intFor: integer;
begin
  if Assigned(FRegistroX292) then
  begin
     for intFor := 0 to FRegistroX292.Count - 1 do
     begin
        with FRegistroX292.Items[intFor] do
        begin
           ///
           Add(LFill('X292')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR, 19));
        end;
        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX300;
var
  intFor: integer;
begin
  if Assigned(FRegistroX300) then
  begin
     for intFor := 0 to FRegistroX300.Count - 1 do
     begin
        with FRegistroX300.Items[intFor] do
        begin
           ///
           Add(LFill('X300')          +
               LFill(NUM_ORDEM)       +
               LFill(TIP_EXP, 2)      +
               LFill(DESC_EXP)        +
              VLFill(TOT_OPER,19)     +
               LFill(COD_NCM)         +
              VLFill(QTDE,19)         +
               LFill(UNI_MED,2)       +
               LFill(IND_OPER,1)      +
               LFill(TIP_MET)         +
              VLFill(VL_PAR,19)       +
              VLFill(VL_PRAT,19)      +
              VLFill(VL_AJ,19)        +
              VLFill(VL_JUR,19)       +
              VLFill(VL_JUR_MIN,7,4)  +
              VLFill(VL_JUR_MAX,7,4)  +
               LFill(COD_CNC)         +
               LFill(TIP_MOEDA));
        end;

        // Registros Filhos
        WriteRegistroX310(FRegistroX300.Items[intFor] );

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX310(RegX300: TRegistroX300);
var
  intFor: integer;
begin
  if Assigned(RegX300.RegistroX310) then
  begin
     for intFor := 0 to RegX300.RegistroX310.Count - 1 do
     begin
        with RegX300.RegistroX310.Items[intFor] do
        begin
           ///
           Add(LFill('X310')      +
               LFill(NOME)        +
               LFill(PAIS)        +
              VLFill(VL_OPER,19)  +
               LFill(COND_PES,2));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;

     FRegistroX310Count := FRegistroX310Count + RegX300.RegistroX310.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX320;
var
  intFor: integer;
begin
  if Assigned(FRegistroX320) then
  begin
     for intFor := 0 to FRegistroX320.Count - 1 do
     begin
        with FRegistroX320.Items[intFor] do
        begin
           ///
           Add(LFill('X320')          +
               LFill(NUM_ORD)         +
               LFill(TIP_IMP,2)       +
               LFill(DESC_IMP)        +
              VLFill(TOT_OPER,19)     +
               LFill(COD_NCM)         +
              VLFill(QTDE,19)         +
               LFill(UNI_MED,2)       +
               LFill(TIP_MET)         +
              VLFill(VL_PAR,19)       +
              VLFill(VL_PRAT,19)      +
              VLFill(VL_AJ,19)        +
              VLFill(VL_JUR,19)       +
              VLFill(VL_JUR_MIN,7,4)  +
              VLFill(VL_JUR_MAX,7,4)  +
               LFill(COD_CNC)         +
               LFill(TIP_MOEDA));
        end;
        // Registros Filhos
        WriteRegistroX330(FRegistroX320.Items[intFor] );

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX330(RegX320: TRegistroX320);
var
  intFor: integer;
begin
  if Assigned(RegX320.RegistroX330) then
  begin
     for intFor := 0 to RegX320.RegistroX330.Count - 1 do
     begin
        with RegX320.RegistroX330.Items[intFor] do
        begin
           ///
           Add(LFill('X330')      +
               LFill(NOME)        +
               LFill(PAIS)        +
              VLFill(VL_OPER,19)  +
               LFill(COND_PES,2));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX330Count := FRegistroX330Count + RegX320.RegistroX330.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX340;
var
  intFor: integer;
begin
  if Assigned(FRegistroX340) then
  begin
     for intFor := 0 to FRegistroX340.Count - 1 do
     begin
        with FRegistroX340.Items[intFor] do
        begin
           ///
           Add(LFill('X340')          +
               LFill(RAZ_SOCIAL)      +
               LFill(NIF)             +
               LFill(IND_CONTROLE, 1) +
               LFill(PAIS)            +
               LFill(IND_ISEN_PETR)   +
               LFill(IND_CONSOL)      +
               LFill(MOT_NAO_CONSOL));
        end;

        // Registros Filhos
        WriteRegistroX350(FRegistroX340.Items[intFor] );
        WriteRegistroX351(FRegistroX340.Items[intFor] );
        WriteRegistroX352(FRegistroX340.Items[intFor] );
        WriteRegistroX353(FRegistroX340.Items[intFor] );
        WriteRegistroX354(FRegistroX340.Items[intFor] );
        WriteRegistroX355(FRegistroX340.Items[intFor] );
        WriteRegistroX356(FRegistroX340.Items[intFor] );

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX350(RegX340: TRegistroX340);
var
  intFor: integer;
begin
  if Assigned(RegX340.RegistroX350) then
  begin
     for intFor := 0 to RegX340.RegistroX350.Count - 1 do
     begin
        with RegX340.RegistroX350.Items[intFor] do
        begin
           ///
           Add(LFill('X350')              +
              VLFill(REC_LIQ,19)          +
              VLFill(CUSTOS,19)           +
              VLFill(LUC_BRUTO,19)        +
              VLFill(REC_AUFERIDAS,19)    +
              VLFill(REC_OUTRAS_OPER,19)  +
              VLFill(DESP_BRASIL,19)      +
              VLFill(DESP_OPER,19)        +
              VLFill(LUC_OPER,19)         +
              VLFill(REC_PARTIC,19)       +
              VLFill(REC_OUTRAS,19)       +
              VLFill(DESP_OUTRAS,19)      +
              VLFill(LUC_LIQ_ANT_IR,19)   +
              VLFill(IMP_DEV,19)          +
              VLFill(LUC_LIQ,19)          +
              VLFill(LUC_ARB_ANT_IMP,19)  +
              VLFill(IMP_DEV_ARB,19)      +
              VLFill(LUC_ARB_PER_APUR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX350Count := FRegistroX350Count + RegX340.RegistroX350.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX351(RegX340: TRegistroX340);
var
  intFor: integer;
begin
  if Assigned(RegX340.RegistroX351) then
  begin
     for intFor := 0 to RegX340.RegistroX351.Count - 1 do
     begin
        with RegX340.RegistroX351.Items[intFor] do
        begin
           ///
           Add(LFill('X351')                    +
              VLFill(RES_INV_PER,19)            +
              VLFill(RES_INV_PER_REAL,19)       +
              VLFill(RES_ISEN_PETR_PER,19)      +
              VLFill(RES_ISEN_PETR_PER_REAL,19) +
              VLFill(RES_NEG_ACUM,19)           +
              VLFill(RES_POS_TRIB,19)           +
              VLFill(RES_POS_TRIB_REAL,19)      +
              VLFill(IMP_LUCR,19)               +
              VLFill(IMP_LUCR_REAL,19)          +
              VLFill(IMP_PAG_REND,19)           +
              VLFill(IMP_PAG_REND_REAL,19)      +
              VLFill(IMP_RET_EXT,19)            +
              VLFill(IMP_RET_EXT_REAL,19)       +
              VLFill(IMP_RET_BR,19))
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX351Count := FRegistroX351Count + RegX340.RegistroX351.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX352(RegX340: TRegistroX340);
var
  intFor: integer;
begin
  if Assigned(RegX340.RegistroX352) then
  begin
     for intFor := 0 to RegX340.RegistroX352.Count - 1 do
     begin
        with RegX340.RegistroX352.Items[intFor] do
        begin
           ///
           Add(LFill('X352')            +
              VLFill(RES_PER,19)        +
              VLFill(RES_PER_REAL,19)   +
              VLFill(LUC_DISP,19)       +
              VLFill(LUC_DISP_REAL,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX352Count := FRegistroX352Count + RegX340.RegistroX352.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX353(RegX340: TRegistroX340);
var
  intFor: integer;
begin
  if Assigned(RegX340.RegistroX353) then
  begin
     for intFor := 0 to RegX340.RegistroX353.Count - 1 do
     begin
        with RegX340.RegistroX353.Items[intFor] do
        begin
           ///
           Add(LFill('X353')                          +
              VLFill(RES_NEG_UTIL,19)                 +
              VLFill(RES_NEG_UTIL_REAL,19)            +
              VLFill(SALDO_RES_NEG_NAO_UTIL,19)       +
              VLFill(SALDO_RES_NEG_NAO_UTIL_REAL,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX353Count := FRegistroX353Count + RegX340.RegistroX353.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX354(RegX340: TRegistroX340);
var
  intFor: integer;
begin
  if Assigned(RegX340.RegistroX354) then
  begin
     for intFor := 0 to RegX340.RegistroX354.Count - 1 do
     begin
        with RegX340.RegistroX354.Items[intFor] do
        begin
           ///
           Add(LFill('X354')            +
              VLFill(RES_NEG,19)        +
              VLFill(RES_NEG_REAL,19)   +
              VLFill(SALDO_RES_NEG,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX354Count := FRegistroX354Count + RegX340.RegistroX354.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX355(RegX340: TRegistroX340);
var
  intFor: integer;
begin
  if Assigned(RegX340.RegistroX355) then
  begin
     for intFor := 0 to RegX340.RegistroX355.Count - 1 do
     begin
        with RegX340.RegistroX355.Items[intFor] do
        begin
           ///
           Add(LFill('X355')                  +
              VLFill(REND_PASS_PROP,19)       +
              VLFill(REND_PASS_PROP_REAL,19)  +
              VLFill(REND_TOTAL,19)           +
              VLFill(REND_TOTAL_REAL,19)      +
              VLFill(REND_ATIV_PROP,19)       +
              VLFill(REND_ATIV_PROP_REAL,19)  +
              VLFill(PERCENTUAL,7));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX355Count := FRegistroX355Count + RegX340.RegistroX355.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX356(RegX340: TRegistroX340);
var
  intFor: integer;
begin
  if Assigned(RegX340.RegistroX356) then
  begin
     for intFor := 0 to RegX340.RegistroX356.Count - 1 do
     begin
        with RegX340.RegistroX356.Items[intFor] do
        begin
           ///
           Add(LFill('X356')          +
              VLFill(PERC_PART,4)     +
              VLFill(ATIVO_TOTAL,19)  +
              VLFill(PAT_LIQUIDO,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
     FRegistroX356Count := FRegistroX356Count + RegX340.RegistroX356.Count;
  end;
end;

procedure TBloco_X.WriteRegistroX390;
var
  intFor: integer;
begin
  if Assigned(FRegistroX390) then
  begin
     for intFor := 0 to FRegistroX390.Count - 1 do
     begin
        with FRegistroX390.Items[intFor] do
        begin
           ///
           Add(LFill('X390') +
               LFill(CODIGO) +
               LFill(DESCRICAO) +
              VLFill(VALOR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX400;
var
  intFor: integer;
begin
  if Assigned(FRegistroX400) then
  begin
     for intFor := 0 to FRegistroX400.Count - 1 do
     begin
        with FRegistroX400.Items[intFor] do
        begin
           ///
           Add(LFill('X400')    +
               LFill(CODIGO)    +
               LFill(DESCRICAO) +
              VLFill(VALOR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX410;
var
  intFor: integer;
begin
  if Assigned(FRegistroX410) then
  begin
     for intFor := 0 to FRegistroX410.Count - 1 do
     begin
        with FRegistroX410.Items[intFor] do
        begin
           ///
           Add(LFill('X410')        +
               LFill(PAIS)          +
               LFill(IND_HOME_DISP) +
               LFill(IND_SERV_DISP))
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX420;
var
  intFor: integer;
begin
  if Assigned(FRegistroX420) then
  begin
     for intFor := 0 to FRegistroX420.Count - 1 do
     begin
        with FRegistroX420.Items[intFor] do
        begin
           ///
           Add(LFill('X420')              +
               LFill(TIP_ROY)             +
               LFill(PAIS)                +
              VLFill(VL_EXPL_DIR_SW,19)   +
              VLFill(VL_EXPL_DIR_AUT,19)  +
              VLFill(VL_EXPL_MARCA,19)    +
              VLFill(VL_EXPL_PAT,19)      +
              VLFill(VL_EXPL_KNOW,19)     +
              VLFill(VL_EXPL_FRANQ,19)    +
              VLFill(VL_EXPL_INT,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX430;
var
  intFor: integer;
begin
  if Assigned(FRegistroX430) then
  begin
     for intFor := 0 to FRegistroX430.Count - 1 do
     begin
        with FRegistroX430.Items[intFor] do
        begin
           ///
           Add(LFill('X430')                    +
               LFill(PAIS)                      +
              VLFill(VL_SERV_ASSIST,19)         +
              VLFill(VL_SERV_SEM_ASSIST,19)     +
              VLFill(VL_SERV_SEM_ASSIST_EXT,19) +
              VLFill(VL_JURO,19)                +
              VLFill(VL_DEMAIS_JUROS,19)        +
              VLFill(VL_DIVID,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX450;
var
  intFor: integer;
begin
  if Assigned(FRegistroX450) then
  begin
     for intFor := 0 to FRegistroX450.Count - 1 do
     begin
        with FRegistroX450.Items[intFor] do
        begin
           ///
           Add(LFill('X450')                    +
               LFill(PAIS)                      +
              VLFill(VL_SERV_ASSIST,19)         +
              VLFill(VL_SERV_SEM_ASSIST,19)     +
              VLFill(VL_SERV_SEM_ASSIST_EXT,19) +
              VLFill(VL_JURO_PF,19)             +
              VLFill(VL_JURO_PJ,19)             +
              VLFill(VL_DEMAIS_JUROS,19)        +
              VLFill(VL_DIVID_PF,19)            +
              VLFill(VL_DIVID_PF,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX460;
var
  intFor: integer;
begin
  if Assigned(FRegistroX460) then
  begin
     for intFor := 0 to FRegistroX460.Count - 1 do
     begin
        with FRegistroX460.Items[intFor] do
        begin
           ///
           Add(LFill('X460')      +
               LFill(CODIGO)      +
               LFill(DESCRICAO)   +
              VLFill(VALOR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX470;
var
  intFor: integer;
begin
  if Assigned(FRegistroX470) then
  begin
     for intFor := 0 to FRegistroX470.Count - 1 do
     begin
        with FRegistroX470.Items[intFor] do
        begin
           ///
           Add(LFill('X470')      +
               LFill(CODIGO)      +
               LFill(DESCRICAO)   +
              VLFill(VALOR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX480;
var
  intFor: integer;
begin
  if Assigned(FRegistroX480) then
  begin
     for intFor := 0 to FRegistroX480.Count - 1 do
     begin
        with FRegistroX480.Items[intFor] do
        begin
           ///
           Add(LFill('X480')      +
               LFill(CODIGO)      +
               LFill(DESCRICAO)   +
              VLFill(VALOR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX490;
var
  intFor: integer;
begin
  if Assigned(FRegistroX490) then
  begin
     for intFor := 0 to FRegistroX490.Count - 1 do
     begin
        with FRegistroX490.Items[intFor] do
        begin
           ///
           Add(LFill('X490')      +
               LFill(CODIGO)      +
               LFill(DESCRICAO)   +
              VLFill(VALOR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX500;
var
  intFor: integer;
begin
  if Assigned(FRegistroX500) then
  begin
     for intFor := 0 to FRegistroX500.Count - 1 do
     begin
        with FRegistroX500.Items[intFor] do
        begin
           ///
           Add(LFill('X500')      +
               LFill(CODIGO)      +
               LFill(DESCRICAO)   +
              VLFill(VALOR,19));
        end;
        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX510;
var
  intFor: integer;
begin
  if Assigned(FRegistroX510) then
  begin
     for intFor := 0 to FRegistroX510.Count - 1 do
     begin
        with FRegistroX510.Items[intFor] do
        begin
           ///
           Add(LFill('X510')      +
               LFill(CODIGO)      +
               LFill(DESCRICAO)   +
              VLFill(VALOR,19));
        end;

        FRegistroX990.QTD_LIN := FRegistroX990.QTD_LIN + 1;
     end;
  end;
end;

procedure TBloco_X.WriteRegistroX990;
begin
  if Assigned(FRegistroX990) then
  begin
     with FRegistroX990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Add(LFill('X990') +
           LFill(QTD_LIN, 0));
     end;
  end;
end;

end.
