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
|* 11/09/2015 - Ariel Guareschi - Identar no padrao utilizado pela ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_T_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_T, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  { TBloco_T }

  TBloco_T = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroT001: TRegistroT001;
    FRegistroT990: TRegistroT990;
    FRegistroT030: TRegistroT030List;

    FRegistroT120Count: Integer;
    FRegistroT150Count: Integer;
    FRegistroT170Count: Integer;
    FRegistroT181Count: Integer;

    procedure WriteRegistroT120(RegT030: TRegistroT030);
    procedure WriteRegistroT150(RegT030: TRegistroT030);
    procedure WriteRegistroT170(RegT030: TRegistroT030);
    procedure WriteRegistroT181(RegT030: TRegistroT030);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    procedure WriteRegistroT001;
    procedure WriteRegistroT030;
    procedure WriteRegistroT990;

    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros; override;

    property RegistroT001: TRegistroT001 read FRegistroT001 write FRegistroT001;
    property RegistroT030: TRegistroT030List read FRegistroT030 write FregistroT030;
    property RegistroT990: TRegistroT990 read FRegistroT990 write FRegistroT990;

    property RegistroT120Count: Integer read FRegistroT120Count write FRegistroT120Count;
    property RegistroT150Count: Integer read FRegistroT150Count write FRegistroT150Count;
    property RegistroT170Count: Integer read FRegistroT170Count write FRegistroT170Count;
    property RegistroT181Count: Integer read FRegistroT181Count write FRegistroT181Count;
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_T }

constructor TBloco_T.Create;
begin
  inherited Create;
  FRegistroT001 := TRegistroT001.Create;
  FRegistroT030 := TRegistroT030List.Create;
  FRegistroT990 := TRegistroT990.Create;

  FRegistroT120Count := 0;
  FRegistroT150Count := 0;
  FRegistroT170Count := 0;
  FRegistroT181Count := 0;
end;

procedure TBloco_T.CriaRegistros;
begin
  FRegistroT001 := TRegistroT001.Create;
  FRegistroT030 := TRegistroT030List.Create;
  FRegistroT990 := TRegistroT990.Create;

  FRegistroT120Count := 0;
  FRegistroT150Count := 0;
  FRegistroT170Count := 0;
  FRegistroT181Count := 0;

  FRegistroT990.QTD_LIN := 0;
end;

destructor TBloco_T.Destroy;
begin
  FRegistroT001.Free;
  FRegistroT030.Free;
  FRegistroT990.Free;
  inherited;
end;

procedure TBloco_T.LiberaRegistros;
begin
  FRegistroT001.Free;
  FRegistroT030.Free;
  FRegistroT990.Free;
end;

procedure TBloco_T.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

procedure TBloco_T.WriteRegistroT001;
begin
  if Assigned(FRegistroT001) then
  begin
    with FRegistroT001 do
    begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(T-T001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
      Add(LFill('T001') +
          LFill( Integer(IND_DAD), 1));
      FRegistroT990.QTD_LIN:= FRegistroT990.QTD_LIN + 1;
      WriteRegistroT030;
    end;
  end;
end;

procedure TBloco_T.WriteRegistroT030;
var
  intFor: integer;
begin
  if Assigned(FRegistroT030) then
  begin
    for intFor := 0 to FRegistroT030.Count - 1 do
    begin
      with FRegistroT030.Items[intFor] do
      begin
        Add(LFill('T030') +
            LFill(DT_INI) +
            LFill(DT_FIN) +
            LFill(PER_APUR));
      end;

      // Registros Filhos
      WriteRegistroT120(FRegistroT030.Items[intFor]);
      WriteRegistroT150(FRegistroT030.Items[intFor]);
      WriteRegistroT170(FRegistroT030.Items[intFor]);
      WriteRegistroT181(FRegistroT030.Items[intFor]);

      FRegistroT990.QTD_LIN := FRegistroT990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_T.WriteRegistroT120(RegT030: TRegistroT030);
var
  intFor: integer;
begin
  if Assigned(RegT030.RegistroT120) then
  begin
    for intFor := 0 to RegT030.RegistroT120.Count - 1 do
    begin
      with RegT030.RegistroT120.Items[intFor] do
      begin
        Add(LFill('T120')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;
      FRegistroT990.QTD_LIN := FRegistroT990.QTD_LIN + 1;
    end;
    FRegistroT120Count := FRegistroT120Count + RegT030.RegistroT120.Count;
  end;
end;

procedure TBloco_T.WriteRegistroT150(RegT030: TRegistroT030);
var
  intFor: integer;
begin
  if Assigned(RegT030.RegistroT150) then
  begin
    for intFor := 0 to RegT030.RegistroT150.Count - 1 do
    begin
      with RegT030.RegistroT150.Items[intFor] do
      begin
        Add(LFill('T150')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;
      FRegistroT990.QTD_LIN := FRegistroT990.QTD_LIN + 1;
    end;
    FRegistroT150Count := FRegistroT150Count + RegT030.RegistroT150.Count;
  end;
end;

procedure TBloco_T.WriteRegistroT170(RegT030: TRegistroT030);
var
  intFor: integer;
begin
  if Assigned(RegT030.RegistroT170) then
  begin
    for intFor := 0 to RegT030.RegistroT170.Count - 1 do
    begin
      with RegT030.RegistroT170.Items[intFor] do
      begin
        Add(LFill('T170')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;
      FRegistroT990.QTD_LIN := FRegistroT990.QTD_LIN + 1;
    end;
    FRegistroT170Count := FRegistroT170Count + RegT030.RegistroT170.Count;
  end;
end;

procedure TBloco_T.WriteRegistroT181(RegT030: TRegistroT030);
var
  intFor: integer;
begin
  if Assigned(RegT030.RegistroT181) then
  begin
    for intFor := 0 to RegT030.RegistroT181.Count - 1 do
    begin
      with RegT030.RegistroT181.Items[intFor] do
      begin
        Add(LFill('T181')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;
      FRegistroT990.QTD_LIN := FRegistroT990.QTD_LIN + 1;
    end;
    FRegistroT181Count := FRegistroT181Count + RegT030.RegistroT181.Count;
  end;
end;

procedure TBloco_T.WriteRegistroT990;
begin
  if Assigned(FRegistroT990) then
  begin
    with FRegistroT990 do
    begin
      QTD_LIN := QTD_LIN + 1;

      Add(LFill('T990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.

