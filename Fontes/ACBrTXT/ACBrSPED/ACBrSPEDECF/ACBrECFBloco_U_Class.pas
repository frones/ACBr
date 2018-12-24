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

unit ACBrECFBloco_U_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_U, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_U -

  { TBloco_U }

  TBloco_U = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroU001: TRegistroU001;
    FRegistroU990: TRegistroU990;

    FRegistroU100Count: Integer;
    FRegistroU150Count: Integer;
    FRegistroU030Count: Integer;
    FRegistroU182Count: Integer;
    FRegistroU180Count: Integer;

    procedure WriteRegistroU030(Regu001: TRegistroU001);
    procedure WriteRegistroU100(RegU030: TRegistroU030);
    procedure WriteRegistroU150(RegU030: TRegistroU030);
    procedure WriteRegistroU180(RegU030: TRegistroU030);
    procedure WriteRegistroU182(RegU030: TRegistroU030);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros; override;

    function RegistroU001New: TRegistroU001;
    function RegistroU030New: TRegistroU030;
    function RegistroU100New: TRegistroU100;
    function RegistroU150New: TRegistroU150;
    function RegistroU180New: TRegistroU180;
    function RegistroU182New: TRegistroU182;

    procedure WriteRegistroU001;
    procedure WriteRegistroU990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroU001: TRegistroU001 read FRegistroU001 write FRegistroU001;
    property RegistroU990: TRegistroU990 read FRegistroU990 write FRegistroU990;

    property RegistroU030Count: Integer read FRegistroU030Count write FRegistroU030Count;
    property RegistroU100Count: Integer read FRegistroU100Count write FRegistroU100Count;
    property RegistroU150Count: Integer read FRegistroU150Count write FRegistroU150Count;
    property RegistroU180Count: Integer read FRegistroU180Count write FRegistroU180Count;
    property RegistroU182Count: Integer read FRegistroU182Count write FRegistroU182Count;
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_U }

constructor TBloco_U.Create;
begin
  inherited Create;
  CriaRegistros;
end;

procedure TBloco_U.CriaRegistros;
begin
  FRegistroU001 := TRegistroU001.Create;
  FRegistroU990 := TRegistroU990.Create;

  FRegistroU030Count := 0;
  FRegistroU100Count := 0;
  FRegistroU150Count := 0;
  FRegistroU180Count := 0;
  FRegistroU182Count := 0;

  FRegistroU990.QTD_LIN := 0;
end;

destructor TBloco_U.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_U.LiberaRegistros;
begin
  FRegistroU001.Free;
  FRegistroU990.Free;
  inherited;
end;

procedure TBloco_U.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_U.RegistroU001New: TRegistroU001;
begin
  Result := FRegistroU001;
end;

function TBloco_U.RegistroU030New: TRegistroU030;
begin
  Result := FRegistroU001.RegistroU030.New(FRegistroU001);
end;

function TBloco_U.RegistroU100New: TRegistroU100;
var
  UU030: TRegistroU030;
  UU030Count: Integer;
begin
  UU030Count := FRegistroU001.RegistroU030.Count -1;
  if UU030Count = -1 then
    raise Exception.Create('O registro U100 deve ser filho do registro U030, e não existe nenhum U030 pai!');

  UU030  := FRegistroU001.RegistroU030.Items[UU030Count];
  Result := UU030.RegistroU100.New(UU030);
end;

function TBloco_U.RegistroU150New: TRegistroU150;
var
  UU030: TRegistroU030;
  UU030Count: Integer;
begin
  UU030Count := FRegistroU001.RegistroU030.Count -1;
  if UU030Count = -1 then
    raise Exception.Create('O registro U150 deve ser filho do registro U030, e não existe nenhum U030 pai!');

  UU030  := FRegistroU001.RegistroU030.Items[UU030Count];
  Result := UU030.RegistroU150.New(UU030);
end;

function TBloco_U.RegistroU180New: TRegistroU180;
var
  UU030: TRegistroU030;
  UU030Count: Integer;
begin
  UU030Count := FRegistroU001.RegistroU030.Count -1;
  if UU030Count = -1 then
    raise Exception.Create('O registro U180 deve ser filho do registro U030, e não existe nenhum U030 pai!');

  UU030  := FRegistroU001.RegistroU030.Items[UU030Count];
  Result := UU030.RegistroU180.New(UU030);
end;

function TBloco_U.RegistroU182New: TRegistroU182;
var
  UU030: TRegistroU030;
  UU030Count: Integer;
begin
  UU030Count := FRegistroU001.RegistroU030.Count -1;
  if UU030Count = -1 then
    raise Exception.Create('O registro U182 deve ser filho do registro U030, e não existe nenhum U030 pai!');

  UU030  := FRegistroU001.RegistroU030.Items[UU030Count];
  Result := UU030.RegistroU182.New(UU030);
end;


procedure TBloco_U.WriteRegistroU001;
begin
  if Assigned(FRegistroU001) then
  begin
    with FRegistroU001 do
    begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(U-U001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
      Add(LFill('U001') +
          LFill( Integer(IND_DAD), 1));

      FRegistroU990.QTD_LIN:= FRegistroU990.QTD_LIN + 1;
      WriteRegistroU030(FRegistroU001);
    end;
  end;
end;

procedure TBloco_U.WriteRegistroU030(RegU001: TRegistroU001);
var
  intFor: integer;
begin
  if Assigned(RegU001.RegistroU030) then
  begin
    for intFor := 0 to RegU001.RegistroU030.Count - 1 do
    begin
      with RegU001.RegistroU030.Items[intFor] do
      begin
        Add( LFill('U030') +
             LFill(DT_INI) +
             LFill(DT_FIN) +
             LFill(PER_APUR) );
      end;
      WriteRegistroU100(RegU001.RegistroU030.Items[intFor]);
      WriteRegistroU150(RegU001.RegistroU030.Items[intFor]);
      WriteRegistroU180(RegU001.RegistroU030.Items[intFor]);
      WriteRegistroU182(RegU001.RegistroU030.Items[intFor]);
      FRegistroU990.QTD_LIN := FRegistroU990.QTD_LIN + 1;
     end;
    FRegistroU030Count := FRegistroU030Count + RegU001.RegistroU030.Count;
  end;
end;

procedure TBloco_U.WriteRegistroU100(RegU030: TRegistroU030);
var
  intFor: integer;
begin
  if Assigned(RegU030.RegistroU100) then
  begin
    for intFor := 0 to RegU030.RegistroU100.Count - 1 do
    begin
      with RegU030.RegistroU100.Items[intFor] do
      begin
        Add( LFill('U100')              +
             LFill(CODIGO)              +
             LFill(DESCRICAO)           +
             LFill(TIPO)                +
             LFill(NIVEL)               +
             LFill(COD_NAT,2)           +
             LFill(COD_CTA_SUP)         +
             VLFill(VAL_CTA_REF_INI,2)  +
             LFill(IND_VAL_CTA_REF_INI) +
             VLFill(VAL_CTA_REF_FIN,2)  +
             LFill(IND_VAL_CTA_REF_FIN) );
      end;
      FRegistroU990.QTD_LIN := FRegistroU990.QTD_LIN + 1;
    end;
    RegistroU100Count := RegistroU100Count + RegU030.RegistroU100.Count;
  end;
end;

procedure TBloco_U.WriteRegistroU150(RegU030: TRegistroU030);
var
  intFor: integer;
begin
  if Assigned(RegU030.RegistroU150) then
  begin
    for intFor := 0 to RegU030.RegistroU150.Count - 1 do
    begin
      with RegU030.RegistroU150.Items[intFor] do
      begin
        Add( LFill('U150')      +
             LFill(CODIGO)      +
             LFill(DESCRICAO)   +
             LFill(TIPO)        +
             LFill(NIVEL)       +
             LFill(COD_NAT)     +
             LFill(COD_CTA_SUP) +
             VLFill(VALOR,2)    +
             LFill(IND_VALOR) );
      end;
      FRegistroU990.QTD_LIN := FRegistroU990.QTD_LIN + 1;
    end;
    RegistroU150Count := RegistroU150Count + RegU030.RegistroU150.Count;
  end;
end;

procedure TBloco_U.WriteRegistroU180(RegU030: TRegistroU030);
var
  intFor: integer;
begin
  if Assigned(RegU030.RegistroU180) then
  begin
    for intFor := 0 to RegU030.RegistroU180.Count - 1 do
    begin
      with RegU030.RegistroU180.Items[intFor] do
      begin
        Add( LFill('U180') +
             LFill(CODIGO) +
             LFill(DESCRICAO) +
             VLFill(VALOR,2) );
      end;
      FRegistroU990.QTD_LIN := FRegistroU990.QTD_LIN + 1;
    end;
    RegistroU180Count := RegistroU180Count + RegU030.RegistroU180.Count;
  end;
end;

procedure TBloco_U.WriteRegistroU182(RegU030: TRegistroU030);
var
  intFor: integer;
begin
  if Assigned(RegU030.RegistroU182) then
  begin
    for intFor := 0 to RegU030.RegistroU182.Count - 1 do
    begin
      with RegU030.RegistroU182.Items[intFor] do
      begin
        Add( LFill('U1820') +
             LFill(CODIGO) +
             LFill(DESCRICAO) +
             VLFill(VALOR,2) );
      end;
      FRegistroU990.QTD_LIN := FRegistroU990.QTD_LIN + 1;
    end;
    RegistroU182Count := RegistroU182Count + RegU030.RegistroU182.Count;
  end;
end;

procedure TBloco_U.WriteRegistroU990;
begin
  if Assigned(FRegistroU990) then
  begin
    with FRegistroU990 do
    begin
      QTD_LIN := QTD_LIN + 1;

      Add(LFill('U990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
