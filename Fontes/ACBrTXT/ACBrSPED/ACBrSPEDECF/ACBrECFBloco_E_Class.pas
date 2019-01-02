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
|* 11/09/2015 - Ariel Guareschi - Identar no padrao utilizado pela ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_E_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_E, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_E -

  { TBloco_E }

  TBloco_E = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroE001: TRegistroE001;
    FRegistroE990: TRegistroE990;

    FRegistroE010Count: Integer;
    FRegistroE015Count: Integer;
    FRegistroE020Count: Integer;
    FRegistroE030Count: Integer;
    FRegistroE155Count: Integer;
    FRegistroE355Count: Integer;

    procedure WriteRegistroE010(RegE001: TRegistroE001);
    procedure WriteRegistroE015(RegE010: TRegistroE010);
    procedure WriteRegistroE020(RegE001: TRegistroE001);
    procedure WriteRegistroE030(RegE001: TRegistroE001);
    procedure WriteRegistroE155(RegE030: TRegistroE030);
    procedure WriteRegistroE355(RegE030: TRegistroE030);


    procedure CriaRegistros;
    procedure LiberaRegistros;

  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    constructor Create; overload;
    destructor Destroy; override;

    procedure LimpaRegistros; override;

    function RegistroE001New: TRegistroE001;
    function RegistroE010New: TRegistroE010;
    function RegistroE015New: TRegistroE015;
    function RegistroE020New: TRegistroE020;
    function RegistroE030New: TRegistroE030;
    function RegistroE155New: TRegistroE155;
    function RegistroE355New: TRegistroE355;

    procedure WriteRegistroE001;
    procedure WriteRegistroE990;

    property RegistroE001: TRegistroE001 read FRegistroE001 write FRegistroE001;
    property RegistroE990: TRegistroE990 read FRegistroE990 write FRegistroE990;

    property RegistroE010Count: Integer read FRegistroE010Count write FRegistroE010Count;
    property RegistroE015Count: Integer read FRegistroE015Count write FRegistroE015Count;
    property RegistroE020Count: Integer read FRegistroE020Count write FRegistroE020Count;
    property RegistroE030Count: Integer read FRegistroE030Count write FRegistroE030Count;
    property RegistroE155Count: Integer read FRegistroE155Count write FRegistroE155Count;
    property RegistroE355Count: Integer read FRegistroE355Count write FRegistroE355Count;
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_E }

constructor TBloco_E.Create;
begin
  inherited Create;
  CriaRegistros;
end;

procedure TBloco_E.CriaRegistros;
begin
  FRegistroE001 := TRegistroE001.Create;
  FRegistroE990 := TRegistroE990.Create;

  FRegistroE990.QTD_LIN := 0;
end;

destructor TBloco_E.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_E.LiberaRegistros;
begin
  FRegistroE001.Free;
  FRegistroE990.Free;
end;

procedure TBloco_E.LimpaRegistros;
begin
  LiberaRegistros;
  Conteudo.Clear;

  CriaRegistros;
end;

function TBloco_E.RegistroE001New: TRegistroE001;
begin
  Result := FRegistroE001;
end;

function TBloco_E.RegistroE010New: TRegistroE010;
begin
  Result := FRegistroE001.RegistroE010.New();
end;

function TBloco_E.RegistroE015New: TRegistroE015;
var
  UE010: TRegistroE010;
  UE010Count: Integer;
begin
  UE010Count := FRegistroE001.RegistroE010.Count -1;
  if UE010Count = -1 then
    raise Exception.Create('O registro E015 deve ser filho do registro E010, e não existe nenhum E010 pai!');

  UE010  := FRegistroE001.RegistroE010.Items[UE010Count];
  Result := UE010.RegistroE015.New();
end;

function TBloco_E.RegistroE020New: TRegistroE020;
begin
  Result := FRegistroE001.RegistroE020.New();
end;

function TBloco_E.RegistroE030New: TRegistroE030;
begin
  Result := FRegistroE001.RegistroE030.New();
end;

function TBloco_E.RegistroE155New: TRegistroE155;
var
  UE030: TRegistroE030;
  UE030Count: Integer;
begin
  UE030Count := FRegistroE001.RegistroE030.Count -1;
  if UE030Count = -1 then
    raise Exception.Create('O registro E155 deve ser filho do registro E030, e não existe nenhum E030 pai!');

  UE030  := FRegistroE001.RegistroE030.Items[UE030Count];
  Result := UE030.RegistroE155.New();
end;

function TBloco_E.RegistroE355New: TRegistroE355;
var
  UE030: TRegistroE030;
  UE030Count: Integer;
begin
  UE030Count := FRegistroE001.RegistroE030.Count -1;
  if UE030Count = -1 then
    raise Exception.Create('O registro E355 deve ser filho do registro E030, e não existe nenhum E030 pai!');

  UE030  := FRegistroE001.RegistroE030.Items[UE030Count];
  Result := UE030.RegistroE355.New();
end;

procedure TBloco_E.WriteRegistroE001;
begin
  if Assigned(FRegistroE001) then
  begin
    with FRegistroE001 do
    begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(E-E001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
      Add(LFill('E001') +
          LFill( Integer(IND_DAD), 1));
      WriteRegistroE010(FRegistroE001);
      WriteRegistroE020(FRegistroE001);
      WriteRegistroE030(FRegistroE001);
      FRegistroE990.QTD_LIN:= FRegistroE990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_E.WriteRegistroE010(RegE001: TRegistroE001);
var
  intFor: Integer;
begin
  if Assigned(RegE001.RegistroE010) then
  begin
    for intFor := 0 to RegE001.RegistroE010.Count - 1 do
    begin
      with RegistroE001.RegistroE010.Items[intFor] do
      begin
        Add(LFill('E010') +
            LFill(COD_NAT) +
            LFill(COD_CTA_REF) +
            LFill(DESC_CTA_REF) +
            LFill(VAL_CTA_REF, 19, 2) +
            LFill(IND_VAL_CTA_REF));
      end;
      WriteRegistroE015(RegE001.RegistroE010.Items[intFor]);
      FRegistroE990.QTD_LIN := FRegistroE990.QTD_LIN + 1;
    end;
    FRegistroE010Count := FRegistroE010Count + RegE001.RegistroE010.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE015(RegE010: TRegistroE010);
var
  intFor: Integer;
begin
  if Assigned(RegE010.RegistroE015) then
  begin
    for intFor := 0 to RegE010.RegistroE015.Count - 1 do
    begin
      with RegE010.RegistroE015.Items[intFor] do
      begin
        Add(LFill('E015') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(DESC_CTA) +
            LFill(VAL_CTA, 19, 2) +
            LFill(IND_VAL_CTA));
      end;
      FRegistroE990.QTD_LIN := FRegistroE990.QTD_LIN + 1;
    end;
    FRegistroE015Count := FRegistroE015Count + RegE010.RegistroE015.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE020(RegE001: TRegistroE001);
var
  intFor: Integer;
begin
  if Assigned(RegE001.RegistroE020) then
  begin
    for intFor := 0 to RegE001.RegistroE020.Count - 1 do
    begin
      with RegistroE001.RegistroE020.Items[intFor] do
      begin
        Add(LFill('E020') +
            LFill(COD_CTA_B) +
            LFill(DESC_CTA_LAL) +
            LFill(DT_AP_LAL) +
            LFill(COD_LAN_ORIG, 0) +
            LFill(DESC_LAN_ORIG) +
            LFill(DT_LIM_LAL) +
            LFill(TRIBUTO) +
            LFill(VL_SALDO_FIN, 19, 2) +
            LFill(IND_VL_SALDO_FIN));
      end;
      FRegistroE990.QTD_LIN := FRegistroE990.QTD_LIN + 1;
    end;
    FRegistroE020Count := FRegistroE020Count + RegE001.RegistroE020.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE030(RegE001: TRegistroE001);
var
  intFor: Integer;
begin
  if Assigned(RegE001.RegistroE030) then
  begin
    for intFor := 0 to RegE001.RegistroE030.Count - 1 do
    begin
      with RegistroE001.RegistroE030.Items[intFor] do
      begin
        Add(LFill('E030') +
            LFill(DT_INI) +
            LFill(DT_FIN) +
            LFill(PER_APUR));
      end;
      WriteRegistroE155(RegE001.RegistroE030.Items[intFor]);
      WriteRegistroE355(RegE001.RegistroE030.Items[intFor]);
      FRegistroE990.QTD_LIN := FRegistroE990.QTD_LIN + 1;
    end;
    FRegistroE030Count := FRegistroE030Count + RegE001.RegistroE030.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE155(RegE030: TRegistroE030);
var
  intFor: Integer;
begin
  if Assigned(RegE030.RegistroE155) then
  begin
    for intFor := 0 to RegE030.RegistroE155.Count - 1 do
    begin
      with RegE030.RegistroE155.Items[intFor] do
      begin
        Add(LFill('E155') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(VL_SLD_INI, 19, 2) +
            LFill(IND_VL_SLD_INI) +
            LFill(VL_DEB, 19, 2) +
            LFill(VL_CRED, 19, 2) +
            LFill(VL_SLD_FIN, 19, 2) +
            LFill(IND_VL_SLD_FIN));
      end;
      FRegistroE990.QTD_LIN := FRegistroE990.QTD_LIN + 1;
    end;
    FRegistroE155Count := FRegistroE155Count + RegE030.RegistroE155.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE355(RegE030: TRegistroE030);
var
  intFor: Integer;
begin
  if Assigned(RegE030.RegistroE355) then
  begin
    for intFor := 0 to RegE030.RegistroE355.Count - 1 do
    begin
      with RegE030.RegistroE355.Items[intFor] do
      begin
        Add(LFill('E355') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(VL_SLD_FIN, 19, 2) +
            LFill(IND_VL_SLD_FIN));
      end;
      FRegistroE990.QTD_LIN := FRegistroE990.QTD_LIN + 1;
    end;
    FRegistroE355Count := FRegistroE355Count + RegE030.RegistroE355.Count;
  end;
end;

procedure TBloco_E.WriteRegistroE990;
begin
  if Assigned(FRegistroE990) then
  begin
    with FRegistroE990 do
    begin
      QTD_LIN := QTD_LIN + 1;
      Add(LFill('E990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
