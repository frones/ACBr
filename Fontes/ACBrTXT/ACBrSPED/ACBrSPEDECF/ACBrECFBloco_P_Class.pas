{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014   Juliomar Marchetti                   }
{					  Isaque Pinheiro		       }
{ 					  Daniel Simões de Almeida	       }
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

unit ACBrECFBloco_P_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_P, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_P-

  { TBloco_P }

  TBloco_P = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroP001: TRegistroP001;
    FRegistroP990: TRegistroP990;
    FRegistroP030: TRegistroP030List;

    FRegistroP230Count: Integer;
    FRegistroP200Count: Integer;
    FRegistroP300Count: Integer;
    FRegistroP130Count: Integer;
    FRegistroP100Count: Integer;
    FRegistroP400Count: Integer;
    FRegistroP500Count: Integer;
    FRegistroP150Count: Integer;

    procedure WriteRegistroP100(RegP030: TRegistroP030);
    procedure WriteRegistroP130(RegP030: TRegistroP030);
    procedure WriteRegistroP150(RegP030: TRegistroP030);
    procedure WriteRegistroP200(RegP030: TRegistroP030);
    procedure WriteRegistroP230(RegP030: TRegistroP030);
    procedure WriteRegistroP300(RegP030: TRegistroP030);
    procedure WriteRegistroP400(RegP030: TRegistroP030);
    procedure WriteRegistroP500(RegP030: TRegistroP030);

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    procedure WriteRegistroP001;
    procedure WriteRegistroP030;
    procedure WriteRegistroP990;

    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros; override;

    function RegistroP001New : TRegistroP001;
    function RegistroP030New : TRegistroP030;
    function RegistroP100New : TRegistroP100;
    function RegistroP130New : TRegistroP130;
    function RegistroP150New : TRegistroP150;
    function RegistroP200New : TRegistroP200;
    function RegistroP230New : TRegistroP230;
    function RegistroP300New : TRegistroP300;
    function RegistroP400New : TRegistroP400;
    function RegistroP500New : TRegistroP500;

    property RegistroP001: TRegistroP001 read FRegistroP001 write FRegistroP001;
    property RegistroP030: TRegistroP030List read FRegistroP030 write FregistroP030;
    property RegistroP990: TRegistroP990 read FRegistroP990 write FRegistroP990;

    property RegistroP100Count: Integer read FRegistroP100Count write FRegistroP100Count;
    property RegistroP130Count: Integer read FRegistroP130Count write FRegistroP130Count;
    property RegistroP150Count: Integer read FRegistroP150Count write FRegistroP150Count;
    property RegistroP200Count: Integer read FRegistroP200Count write FRegistroP200Count;
    property RegistroP230Count: Integer read FRegistroP230Count write FRegistroP230Count;
    property RegistroP300Count: Integer read FRegistroP150Count write FRegistroP300Count;
    property RegistroP400Count: Integer read FRegistroP200Count write FRegistroP400Count;
    property RegistroP500Count: Integer read FRegistroP230Count write FRegistroP500Count;
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_P }

constructor TBloco_P.Create;
begin
  inherited;
  CriaRegistros;
end;

procedure TBloco_P.CriaRegistros;
begin
  inherited;

  FRegistroP001 := TRegistroP001.Create;
  FRegistroP030 := TRegistroP030List.Create;
  FRegistroP990 := TRegistroP990.Create;

  FRegistroP230Count := 0;
  FRegistroP200Count := 0;
  FRegistroP300Count := 0;
  FRegistroP130Count := 0;
  FRegistroP100Count := 0;
  FRegistroP400Count := 0;
  FRegistroP500Count := 0;
  FRegistroP150Count := 0;
//  FRegistroP030Count := 0;

  FRegistroP990.QTD_LIN := 0;
end;

destructor TBloco_P.Destroy;
begin
  FRegistroP001.Free;
  FRegistroP030.Free;
  FRegistroP990.Free;

  inherited;
end;

procedure TBloco_P.LiberaRegistros;
begin
  inherited;

  FRegistroP001.Free;
  FRegistroP030.Free;
  FRegistroP990.Free;
end;

procedure TBloco_P.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_P.RegistroP001New: TRegistroP001;
begin
  Result := FRegistroP001;
end;

function TBloco_P.RegistroP030New: TRegistroP030;
begin
  Result := RegistroP001.RegistroP030.New();
end;

function TBloco_P.RegistroP100New: TRegistroP100;
var
  P100Count: integer;
begin
  P100Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P100Count].RegistroP100.New;
end;

function TBloco_P.RegistroP130New: TRegistroP130;
var
  P130Count: integer;
begin
  P130Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P130Count].RegistroP130.New;
end;

function TBloco_P.RegistroP150New: TRegistroP150;
var
  P150Count: integer;
begin
  P150Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P150Count].RegistroP150.New;
end;

function TBloco_P.RegistroP200New: TRegistroP200;
var
  P200Count: integer;
begin
  P200Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P200Count].RegistroP200.New;
end;

function TBloco_P.RegistroP230New: TRegistroP230;
var
  P230Count: integer;
begin
  P230Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P230Count].RegistroP230.New;
end;

function TBloco_P.RegistroP300New: TRegistroP300;
var
  P300Count: integer;
begin
  P300Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P300Count].RegistroP300.New;
end;

function TBloco_P.RegistroP400New: TRegistroP400;
var
  P400Count: integer;
begin
  P400Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P400Count].RegistroP400.New;
end;

function TBloco_P.RegistroP500New: TRegistroP500;
var
  P500Count: integer;
begin
  P500Count := FRegistroP001.RegistroP030.Count -1;
  Result := FRegistroP001.RegistroP030.Items[P500Count].RegistroP500.New;
end;

procedure TBloco_P.WriteRegistroP001;
begin
  if Assigned(FRegistroP001) then
  begin
    with FRegistroP001 do
    begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(P-P001) Na abertura do bloco, deve ser informado o número 0 ou 1!');

      Add(LFill('P001') +
          LFill( Integer(IND_DAD), 1));
      FRegistroP990.QTD_LIN:= FRegistroP990.QTD_LIN + 1;
      WriteRegistroP030;
    end;
  end;
end;

procedure TBloco_P.WriteRegistroP030;
var
  intFor: integer;
begin
  if Assigned(FRegistroP030) then
  begin
    for intFor := 0 to FRegistroP001.RegistroP030.Count - 1 do
    begin
      with FRegistroP001.RegistroP030.Items[intFor] do
      begin

        Add(LFill('P030') +
            LFill(DT_INI) +
            LFill(DT_FIN) +
            LFill(PER_APUR));
      end;

      // Registros Filhos
      WriteRegistroP100(FRegistroP001.RegistroP030.Items[intFor]);
      WriteRegistroP130(FRegistroP001.RegistroP030.Items[intFor]);
      WriteRegistroP150(FRegistroP001.RegistroP030.Items[intFor]);
      WriteRegistroP200(FRegistroP001.RegistroP030.Items[intFor]);
      WriteRegistroP230(FRegistroP001.RegistroP030.Items[intFor]);
      WriteRegistroP300(FRegistroP001.RegistroP030.Items[intFor]);
      WriteRegistroP400(FRegistroP001.RegistroP030.Items[intFor]);
      WriteRegistroP500(FRegistroP001.RegistroP030.Items[intFor]);

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_P.WriteRegistroP100(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP100) then
  begin
    for intFor := 0 to RegP030.RegistroP100.Count - 1 do
    begin
      with RegP030.RegistroP100.Items[intFor] do
      begin
        /// Layout 5 a partir da escrituração ano calendário 2018
        if DT_INI >= EncodeDate(2018,01,01) then
        begin
          Add(LFill('P100')                  +
              LFill(CODIGO)                  +
              LFill(DESCRICAO)               +
              LFill(TIPO)                    +
              LFill(NIVEL, 1)                +
              LFill(COD_NAT)                 +
              LFill(COD_CTA_SUP)             +
              VLFill(VAL_CTA_REF_INI, 19, 2) +
              LFill(IND_VAL_CTA_REF_INI, 1)  +
              VLFill(VAL_CTA_REF_DEB,2)      +
              VLFill(VAL_CTA_REF_CRED,2)     +
              VLFill(VAL_CTA_REF_FIN, 19, 2) +
              LFill(IND_VAL_CTA_REF_FIN, 1));
        end
        else
        begin
          Add(LFill('P100')                  +
              LFill(CODIGO)                  +
              LFill(DESCRICAO)               +
              LFill(TIPO)                    +
              LFill(NIVEL, 1)                +
              LFill(COD_NAT)                 +
              LFill(COD_CTA_SUP)             +
              VLFill(VAL_CTA_REF_INI, 19, 2) +
              LFill(IND_VAL_CTA_REF_INI, 1)  +
              VLFill(VAL_CTA_REF_FIN, 19, 2) +
              LFill(IND_VAL_CTA_REF_FIN, 1));
        end;
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
    end;

    FRegistroP100Count := FRegistroP100Count + RegP030.RegistroP100.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP130(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP130) then
  begin
    for intFor := 0 to RegP030.RegistroP130.Count - 1 do
    begin
      with RegP030.RegistroP130.Items[intFor] do
      begin

        Add(LFill('P130')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
    end;

    FRegistroP130Count := FRegistroP130Count + RegP030.RegistroP130.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP150(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP150) then
  begin
    for intFor := 0 to RegP030.RegistroP150.Count - 1 do
    begin
      with RegP030.RegistroP150.Items[intFor] do
      begin

        Add(LFill('P150')       +
            LFill(CODIGO)       +
            LFill(DESCRICAO)    +
            LFill(TIPO)         +
            LFill(NIVEL, 1)     +
            LFill(COD_NAT)      +
            LFill(COD_CTA_SUP)  +
            VLFill(VALOR, 19, 2) +
            LFill(IND_VALOR, 1));
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
    end;

    FRegistroP150Count := FRegistroP150Count + RegP030.RegistroP150.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP200(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP200) then
  begin
    for intFor := 0 to RegP030.RegistroP200.Count - 1 do
    begin
      with RegP030.RegistroP200.Items[intFor] do
      begin

        Add(LFill('P200')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
    end;

    FRegistroP200Count := FRegistroP200Count + RegP030.RegistroP200.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP230(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP230) then
  begin
    for intFor := 0 to RegP030.RegistroP230.Count - 1 do
    begin
      with RegP030.RegistroP230.Items[intFor] do
      begin

        Add(LFill('P230')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
    end;

    FRegistroP230Count := FRegistroP230Count + RegP030.RegistroP230.Count;
  end;
end;

procedure TBloco_P.WriteRegistroP300(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP300) then
  begin
    for intFor := 0 to RegP030.RegistroP300.Count - 1 do
    begin
      with RegP030.RegistroP300.Items[intFor] do
      begin
        Add(LFill('P300')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
      FRegistroP300Count := FRegistroP300Count + 1;
    end;
  end;
end;

procedure TBloco_P.WriteRegistroP400(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP400) then
  begin
    for intFor := 0 to RegP030.RegistroP400.Count - 1 do
    begin
      with RegP030.RegistroP400.Items[intFor] do
      begin
        Add(LFill('P400')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
      FRegistroP400Count := FRegistroP400Count + 1;
    end;
  end;
end;

procedure TBloco_P.WriteRegistroP500(RegP030: TRegistroP030);
var
  intFor: integer;
begin
  if Assigned(RegP030.RegistroP500) then
  begin
    for intFor := 0 to RegP030.RegistroP500.Count - 1 do
    begin
      with RegP030.RegistroP500.Items[intFor] do
      begin
        Add(LFill('P500')    +
            LFill(CODIGO)    +
            LFill(DESCRICAO) +
            VLFill(VALOR, 19, 2));
      end;

      FRegistroP990.QTD_LIN := FRegistroP990.QTD_LIN + 1;
      FRegistroP500Count := FRegistroP500Count + 1;
    end;
  end;
end;

procedure TBloco_P.WriteRegistroP990;
begin
  if Assigned(FRegistroP990) then
  begin
    with FRegistroP990 do
    begin
      QTD_LIN := QTD_LIN + 1;
      Add(LFill('P990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
