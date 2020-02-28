{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliomar Marchetti e Isaque Pinheiro            }
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

{******************************************************************************
|* Historico
|*
|* 09/09/2015 - Ariel Guareschi - Implementado os métodos faltantes.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_C_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_C, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_C -

  { TBloco_C }

  TBloco_C = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroC001: TRegistroC001;
    FRegistroC990: TRegistroC990;

    FRegistroC040Count: Integer;
    FRegistroC050Count: Integer;
    FRegistroC051Count: Integer;
    FRegistroC053Count: Integer;
    FRegistroC100Count: Integer;
    FRegistroC150Count: Integer;
    FRegistroC155Count: Integer;
    FRegistroC157Count: Integer;
    FRegistroC350Count: Integer;
    FRegistroC355Count: Integer;

    procedure WriteRegistroC040(RegC001: TRegistroC001);
    procedure WriteRegistroC050(RegC040: TRegistroC040);
    procedure WriteRegistroC051(RegC050: TRegistroC050);
    procedure WriteRegistroC053(RegC050: TRegistroC050);
    procedure WriteRegistroC100(RegC040: TRegistroC040);
    procedure WriteRegistroC150(RegC040: TRegistroC040);
    procedure WriteRegistroC155(RegC150: TRegistroC150);
    procedure WriteRegistroC157(RegC155: TRegistroC155);
    procedure WriteRegistroC350(RegC040: TRegistroC040);
    procedure WriteRegistroC355(RegC350: TRegistroC350);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LimpaRegistros; override;

    function RegistroC001New: TRegistroC001;
    function RegistroC040New: TRegistroC040;
    function RegistroC050New: TRegistroC050;
    function RegistroC051New: TRegistroC051;
    function RegistroC053New: TRegistroC053;
    function RegistroC100New: TRegistroC100;
    function RegistroC150New: TRegistroC150;
    function RegistroC155New: TRegistroC155;
    function RegistroC157New: TRegistroC157;
    function RegistroC350New: TRegistroC350;
    function RegistroC355New: TRegistroC355;

    procedure WriteRegistroC001;
    procedure WriteRegistroC990;



    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;


    property RegistroC001: TRegistroC001 read FRegistroC001 write FRegistroC001;
    property RegistroC990: TRegistroC990 read FRegistroC990 write FRegistroC990;

    property RegistroC040Count: Integer read FRegistroC040Count write FRegistroC040Count;
    property RegistroC050Count: Integer read FRegistroC050Count write FRegistroC050Count;
    property RegistroC051Count: Integer read FRegistroC051Count write FRegistroC051Count;
    property RegistroC053Count: Integer read FRegistroC053Count write FRegistroC053Count;
    property RegistroC100Count: Integer read FRegistroC100Count write FRegistroC100Count;
    property RegistroC150Count: Integer read FRegistroC150Count write FRegistroC150Count;
    property RegistroC155Count: Integer read FRegistroC155Count write FRegistroC155Count;
    property RegistroC157Count: Integer read FRegistroC157Count write FRegistroC157Count;
    property RegistroC350Count: Integer read FRegistroC350Count write FRegistroC350Count;
    property RegistroC355Count: Integer read FRegistroC355Count write FRegistroC355Count;

  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_C }

constructor TBloco_C.Create;
begin
  inherited Create;
  CriaRegistros;
end;

procedure TBloco_C.CriaRegistros;
begin
  FRegistroC001 := TRegistroC001.Create;
  FRegistroC990 := TRegistroC990.Create;

  FRegistroC990.QTD_LIN := 0;
end;

destructor TBloco_C.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_C.LiberaRegistros;
begin
  FRegistroC001.Free;
  FRegistroC990.Free;
end;

procedure TBloco_C.LimpaRegistros;
begin
  LiberaRegistros;
  Conteudo.Clear;

  CriaRegistros;
end;

function TBloco_C.RegistroC001New: TRegistroC001;
begin
  Result := FRegistroC001;
end;

function TBloco_C.RegistroC040New: TRegistroC040;
begin
  Result := FRegistroC001.RegistroC040.New();
end;

function TBloco_C.RegistroC050New: TRegistroC050;
var
  UC040: TRegistroC040;
  UC040Count: Integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  if UC040Count = -1 then
    raise Exception.Create('O registro C040 deve ser filho do registro C001, e não existe nenhum C001 pai!');

  UC040  := FRegistroC001.RegistroC040.Items[UC040Count];
  Result := UC040.RegistroC050.New();
end;

function TBloco_C.RegistroC051New: TRegistroC051;
var
  UC050: TRegistroC050;
  UC040Count: integer;
  UC050Count: integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  UC050Count := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC050.Count -1;
  if UC050Count = -1 then
    raise Exception.Create('O registro C051 deve ser filho do registro C050, e não existe nenhum C050 pai!');

  UC050  := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC050.Items[UC050Count];
  Result := UC050.RegistroC051.New();
end;

function TBloco_C.RegistroC053New: TRegistroC053;
var
  UC050: TRegistroC050;
  UC040Count: integer;
  UC050Count: integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  UC050Count := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC050.Count -1;
  if UC050Count = -1 then
    raise Exception.Create('O registro C053 deve ser filho do registro C050, e não existe nenhum C050 pai!');

  UC050  := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC050.Items[UC050Count];
  Result := UC050.RegistroC053.New();
end;

function TBloco_C.RegistroC100New: TRegistroC100;
var
  UC040: TRegistroC040;
  UC040Count: Integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  if UC040Count = -1 then
    raise Exception.Create('O registro C040 deve ser filho do registro C001, e não existe nenhum C001 pai!');

  UC040  := FRegistroC001.RegistroC040.Items[UC040Count];
  Result := UC040.RegistroC100.New();
end;

function TBloco_C.RegistroC150New: TRegistroC150;
var
  UC040: TRegistroC040;
  UC040Count: Integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  if UC040Count = -1 then
    raise Exception.Create('O registro C040 deve ser filho do registro C001, e não existe nenhum C001 pai!');

  UC040  := FRegistroC001.RegistroC040.Items[UC040Count];
  Result := UC040.RegistroC150.New();
end;

function TBloco_C.RegistroC155New: TRegistroC155;
var
  UC150: TRegistroC150;
  UC040Count: integer;
  UC150Count: integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  UC150Count := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC150.Count -1;
  if UC150Count = -1 then
    raise Exception.Create('O registro C155 deve ser filho do registro C150, e não existe nenhum C150 pai!');

  UC150  := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC150.Items[UC150Count];
  Result := UC150.RegistroC155.New();
end;

function TBloco_C.RegistroC157New: TRegistroC157;
var
  UC155: TRegistroC155;
  UC040Count: integer;
  UC150Count: integer;
  UC155Count: integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  UC150Count := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC150.Count -1;
  UC155Count := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC150.Items[UC150Count].RegistroC155.Count -1;
  if UC155Count = -1 then
    raise Exception.Create('O registro C157 deve ser filho do registro C155, e não existe nenhum C155 pai!');

  UC155  := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC150.Items[UC150Count].RegistroC155.Items[UC155Count];
  Result := UC155.RegistroC157.New();
end;

function TBloco_C.RegistroC350New: TRegistroC350;
var
  UC040: TRegistroC040;
  UC040Count: Integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  if UC040Count = -1 then
    raise Exception.Create('O registro C040 deve ser filho do registro C001, e não existe nenhum C001 pai!');

  UC040  := FRegistroC001.RegistroC040.Items[UC040Count];
  Result := UC040.RegistroC350.New();
end;

function TBloco_C.RegistroC355New: TRegistroC355;
var
  UC350: TRegistroC350;
  UC040Count: integer;
  UC350Count: integer;
begin
  UC040Count := FRegistroC001.RegistroC040.Count -1;
  UC350Count := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC350.Count -1;
  if UC350Count = -1 then
    raise Exception.Create('O registro C355 deve ser filho do registro C350, e não existe nenhum C350 pai!');

  UC350  := FRegistroC001.RegistroC040.Items[UC040Count].RegistroC350.Items[UC350Count];
  Result := UC350.RegistroC355.New();
end;

procedure TBloco_C.WriteRegistroC001;
begin
  if Assigned(FRegistroC001) then begin
    with FRegistroC001 do begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(C-C001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
      Add(LFill('C001') +
          LFill( Integer(IND_DAD), 1));

      if (IND_DAD = idComDados) then begin
        WriteRegistroC040(FRegistroC001);
      end;
      FRegistroC990.QTD_LIN:= FRegistroC990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_C.WriteRegistroC040(RegC001: TRegistroC001);
var
  intFor: integer;
begin
  if Assigned(RegC001.RegistroC040) then begin
    for intFor := 0 to RegC001.RegistroC040.Count -1 do begin
      with RegC001.RegistroC040.Items[intFor] do begin
        Add(LFill('C040') +
            LFill(HASH_ECD) +
            LFill(DT_INI) +
            LFill(DT_FIN) +
            LFill(IND_SIT_ESP, 1) +
            LFill(CNPJ, 14) +
            LFill(NUM_ORD, 0) +
            LFill(NIRE, 11) +
            LFill(NAT_LIVR) +
            LFill(COD_VER_LC) +
            LFill(IND_ESC));
      end;
      WriteRegistroC050(RegC001.RegistroC040.Items[intFor]);
      WriteRegistroC100(RegC001.RegistroC040.Items[intFor]);
      WriteRegistroC150(RegC001.RegistroC040.Items[intFor]);
      WriteRegistroC350(RegC001.RegistroC040.Items[intFor]);

      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC040Count := FRegistroC040Count + RegC001.RegistroC040.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC050(RegC040: TRegistroC040);
var
  intFor: integer;
begin
  if Assigned(RegC040.RegistroC050) then begin
    for intFor := 0 to RegC040.RegistroC050.Count -1 do begin
      with RegC040.RegistroC050.Items[intFor] do begin
        Add(LFill('C050') +
            LFill(DT_ALT) +
            LFill(COD_NAT) +
            LFill(IND_CTA) +
            LFill(NIVEL, 0) +
            LFill(COD_CONTA) +
            LFill(COD_CTA_SUP) +
            LFill(CTA));
      end;
      WriteRegistroC051(RegC040.RegistroC050.Items[intFor]);
      WriteRegistroC053(RegC040.RegistroC050.Items[intFor]);
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC050Count := FRegistroC050Count + RegC040.RegistroC050.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC051(RegC050: TRegistroC050);
var
  intFor: integer;
begin
  if Assigned(RegC050.RegistroC051) then begin
    for intFor := 0 to RegC050.RegistroC051.Count -1 do begin
      with RegC050.RegistroC051.Items[intFor] do begin
        Add(LFill('C051') +
            LFill(COD_ENT_REF) +
            LFill(COD_CCUS) +
            LFill(COD_CTA_REF));
      end;
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC051Count := FRegistroC051Count + RegC050.RegistroC051.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC053(RegC050: TRegistroC050);
var
  intFor: integer;
begin
  if Assigned(RegC050.RegistroC053) then begin
    for intFor := 0 to RegC050.RegistroC053.Count -1 do begin
      with RegC050.RegistroC053.Items[intFor] do begin
        Add(LFill('C053') +
            LFill(COD_IDT) +
            LFill(COD_CNT_CORR) +
            LFill(NAT_SUB_CNT));
      end;
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC053Count := FRegistroC053Count + RegC050.RegistroC053.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC100(RegC040: TRegistroC040);
var
  intFor: integer;
begin
  if Assigned(RegC040.RegistroC100) then begin
    for intFor := 0 to RegC040.RegistroC100.Count -1 do begin
      with RegC040.RegistroC100.Items[intFor] do begin
        Add(LFill('C100') +
            LFill(DT_ALT) +
            LFill(COD_CCUS) +
            LFill(CCUS));
      end;
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC100Count := FRegistroC100Count + RegC040.RegistroC100.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC150(RegC040: TRegistroC040);
var
  intFor: integer;
begin
  if Assigned(RegC040.RegistroC150) then begin
    for intFor := 0 to RegC040.RegistroC150.Count -1 do begin
      with RegC040.RegistroC150.Items[intFor] do begin
        Add(LFill('C150') +
            LFill(DT_INI) +
            LFill(DT_FIN));
      end;
      WriteRegistroC155(RegC040.RegistroC150.Items[intFor]);
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC150Count := FRegistroC150Count + RegC040.RegistroC150.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC155(RegC150: TRegistroC150);
var
  intFor: integer;
begin
  if Assigned(RegC150.RegistroC155) then begin
    for intFor := 0 to RegC150.RegistroC155.Count -1 do begin
      with RegC150.RegistroC155.Items[intFor] do begin
        Add(LFill('C155') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(VL_SLD_INI, 19, 2) +
            LFill(IND_VL_SLD_INI) +
            LFill(VL_DEB, 19, 2) +
            LFill(VL_CRED, 19, 2) +
            LFill(VL_SLD_FIN, 19 , 2) +
            LFill(IND_VL_SLD_FIN) +
            LFill(LINHA_ECD, 0));
      end;
      WriteRegistroC157(RegC150.RegistroC155.Items[intFor]);
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC155Count := FRegistroC155Count + RegC150.RegistroC155.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC157(RegC155: TRegistroC155);
var
  intFor: integer;
begin
  if Assigned(RegC155.RegistroC157) then begin
    for intFor := 0 to RegC155.RegistroC157.Count -1 do begin
      with RegC155.RegistroC157.Items[intFor] do begin
        Add(LFill('C157') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(VL_SLD_FIN, 19 ,2 ) +
            LFill(IND_VL_SLD_FIN) +
            LFill(LINHA_ECD, 0));
      end;
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC157Count := FRegistroC157Count + RegC155.RegistroC157.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC350(RegC040: TRegistroC040);
var
  intFor: integer;
begin
  if Assigned(RegC040.RegistroC350) then begin
    for intFor := 0 to RegC040.RegistroC350.Count -1 do begin
      with RegC040.RegistroC350.Items[intFor] do begin
        Add(LFill('C350') +
            LFill(DT_RES));
      end;
      WriteRegistroC355(RegC040.RegistroC350.Items[intFor]);
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC350Count := FRegistroC350Count + RegC040.RegistroC350.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC355(RegC350: TRegistroC350);
var
  intFor: integer;
begin
  if Assigned(RegC350.RegistroC355) then begin
    for intFor := 0 to RegC350.RegistroC355.Count -1 do begin
      with RegC350.RegistroC355.Items[intFor] do begin
        Add(LFill('C355') +
            LFill(COD_CTA) +
            LFill(COD_CCUS) +
            LFill(VL_CTA, 19 ,2 ) +
            LFill(IND_VL_CTA) +
            LFill(LINHA_ECD, 0));
      end;
      FRegistroC990.QTD_LIN := FRegistroC990.QTD_LIN + 1;
    end;
    FRegistroC355Count := FRegistroC355Count + RegC350.RegistroC355.Count;
  end;
end;

procedure TBloco_C.WriteRegistroC990;
begin
  if Assigned(FRegistroC990) then begin
    with FRegistroC990 do begin
      QTD_LIN := QTD_LIN + 1;
      Add(LFill('C990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
