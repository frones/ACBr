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
|* --/--/2015: Juliomar Marchetti
|*  - Criação.
|* 12/08/2015: Isaque Pinheiro
|*  - Distribuição da primeira versão.
|* 18/08/2015: Ariel Guareschi
|*  - Alterado a geração do arquivo.
|* 20/08/2015: Lutzem Massao Aihara
|*  - Reestrurada a geração do arquivo e implementado funções "RegistroJXXXNew".
|* 11/09/2015 - Ariel Guareschi - Identar no padrao utilizado pela ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrECFBloco_L_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_L, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_L -
  TBloco_L = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;

    FRegistroL001: TRegistroL001;
    FRegistroL990: TRegistroL990;

    FRegistroL030Count: Integer;
    FRegistroL100Count: Integer;
    FRegistroL200Count: Integer;
    FRegistroL210Count: Integer;
    FRegistroL300Count: Integer;

    procedure WriteRegistroL030(RegL001: TRegistroL001);
    procedure WriteRegistroL100(RegL030: TRegistroL030);
    procedure WriteRegistroL200(RegL030: TRegistroL030);
    procedure WriteRegistroL210(RegL030: TRegistroL030);
    procedure WriteRegistroL300(RegL030: TRegistroL030);

    procedure CriaRegistros;
    procedure LiberaRegistros;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LimpaRegistros; override;

    function RegistroL001New: TRegistroL001;
    function RegistroL030New: TRegistroL030;
    function RegistroL100New: TRegistroL100;
    function RegistroL200New: TRegistroL200;
    function RegistroL210New: TRegistroL210;
    function RegistroL300New: TRegistroL300;

    procedure WriteRegistroL001;
    procedure WriteRegistroL990;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;
    property RegistroL001: TRegistroL001 read FRegistroL001 write FRegistroL001;
    property RegistroL990: TRegistroL990 read FRegistroL990 write FRegistroL990;

    property RegistroL030Count: Integer read FRegistroL030Count write FRegistroL030Count;
    property RegistroL100Count: Integer read FRegistroL100Count write FRegistroL100Count;
    property RegistroL200Count: Integer read FRegistroL200Count write FRegistroL200Count;
    property RegistroL210Count: Integer read FRegistroL210Count write FRegistroL210Count;
    property RegistroL300Count: Integer read FRegistroL300Count write FRegistroL300Count;
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_L }

constructor TBloco_L.Create;
begin
  inherited Create;
  CriaRegistros;
end;

procedure TBloco_L.CriaRegistros;
begin
  FRegistroL001 := TRegistroL001.Create;
  FRegistroL990 := TRegistroL990.Create;

  FRegistroL030Count := 0;
  FRegistroL100Count := 0;
  FRegistroL200Count := 0;
  FRegistroL210Count := 0;
  FRegistroL300Count := 0;

  FRegistroL990.QTD_LIN := 0;
end;

destructor TBloco_L.Destroy;
begin
  LiberaRegistros;
  inherited;
end;

procedure TBloco_L.LiberaRegistros;
begin
  FRegistroL001.Free;
  FRegistroL990.Free;

  inherited;
end;

procedure TBloco_L.LimpaRegistros;
begin
  /// Limpa os Registros
  LiberaRegistros;
  Conteudo.Clear;

  /// Recriar os Registros Limpos
  CriaRegistros;
end;

function TBloco_L.RegistroL001New: TRegistroL001;
begin
  Result := FRegistroL001;
end;

function TBloco_L.RegistroL030New: TRegistroL030;
begin
  Result := FRegistroL001.RegistroL030.New();
end;

function TBloco_L.RegistroL100New: TRegistroL100;
var
  UL030: TRegistroL030;
  UL030Count: Integer;
begin
  UL030Count := FRegistroL001.RegistroL030.Count -1;
  if UL030Count = -1 then
    raise Exception.Create('O registro L100 deve ser filho do registro L030, e não existe nenhum L030 pai!');

  UL030  := FRegistroL001.RegistroL030.Items[UL030Count];
  Result := UL030.RegistroL100.New();
end;

function TBloco_L.RegistroL200New: TRegistroL200;
var
  UL030: TRegistroL030;
  UL030Count: Integer;
begin
  UL030Count := FRegistroL001.RegistroL030.Count -1;
  if UL030Count = -1 then
    raise Exception.Create('O registro L200 deve ser filho do registro L030, e não existe nenhum L030 pai!');

  UL030  := FRegistroL001.RegistroL030.Items[UL030Count];
  Result := UL030.RegistroL200.New();
end;

function TBloco_L.RegistroL210New: TRegistroL210;
var
  UL030: TRegistroL030;
  UL030Count: Integer;
begin
  UL030Count := FRegistroL001.RegistroL030.Count -1;
  if UL030Count = -1 then
    raise Exception.Create('O registro L210 deve ser filho do registro L030, e não existe nenhum L030 pai!');

  UL030  := FRegistroL001.RegistroL030.Items[UL030Count];
  Result := UL030.RegistroL210.New();
end;

function TBloco_L.RegistroL300New: TRegistroL300;
var
  UL030: TRegistroL030;
  UL030Count: Integer;
begin
  UL030Count := FRegistroL001.RegistroL030.Count -1;
  if UL030Count = -1 then
    raise Exception.Create('O registro L300 deve ser filho do registro L030, e não existe nenhum L030 pai!');

  UL030  := FRegistroL001.RegistroL030.Items[UL030Count];
  Result := UL030.RegistroL300.New();
end;

procedure TBloco_L.WriteRegistroL100(RegL030: TRegistroL030);
var
  intFor: integer;
begin
  if Assigned(RegL030.RegistroL100) then
  begin
    for intFor := 0 to RegL030.RegistroL100.Count - 1 do
    begin
      with RegL030.RegistroL100.Items[intFor] do
      begin
        /// Layout 5 a partir da escrituração ano calendário 2018
        if DT_INI >= EncodeDate(2018,01,01) then
        begin
          Add( LFill('L100')              +
               LFill(CODIGO)              +
               LFill(DESCRICAO)           +
               LFill(TIPO)                +
               LFill(NIVEL, 3)            +
               LFill(COD_NAT,2)           +
               LFill(COD_CTA_SUP)         +
               VLFill(VAL_CTA_REF_INI,2)  +
               LFill(IND_VAL_CTA_REF_INI) +
               VLFill(VAL_CTA_REF_DEB,2)  +
               VLFill(VAL_CTA_REF_CRED,2) +
               VLFill(VAL_CTA_REF_FIN,2)  +
               LFill(IND_VAL_CTA_REF_FIN) );
        end
        else
        begin
          Add( LFill('L100')              +
               LFill(CODIGO)              +
               LFill(DESCRICAO)           +
               LFill(TIPO)                +
               LFill(NIVEL, 3)            +
               LFill(COD_NAT,2)           +
               LFill(COD_CTA_SUP)         +
               VLFill(VAL_CTA_REF_INI,2)  +
               LFill(IND_VAL_CTA_REF_INI) +
               VLFill(VAL_CTA_REF_FIN,2)  +
               LFill(IND_VAL_CTA_REF_FIN) );
        end;
      end;
      FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
    end;
    RegistroL100Count := RegistroL100Count + RegL030.RegistroL100.Count;
  end;
end;

procedure TBloco_L.WriteRegistroL200(RegL030: TRegistroL030);
var
  intFor: integer;
begin
  if Assigned(RegL030.RegistroL200) then
  begin
    for intFor := 0 to RegL030.RegistroL200.Count - 1 do
    begin
      with RegL030.RegistroL200.Items[intFor] do
      begin
        Add( LFill('L200') +
             LFill(IND_AVAL_ESTOQ) );
      end;
      FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
    end;
    RegistroL200Count := RegistroL200Count + RegL030.RegistroL200.Count;
  end;
end;

procedure TBloco_L.WriteRegistroL210(RegL030: TRegistroL030);
var
  intFor: integer;
begin
  if Assigned(RegL030.RegistroL210) then
  begin
    for intFor := 0 to RegL030.RegistroL210.Count - 1 do
    begin
      with RegL030.RegistroL210.Items[intFor] do
      begin
        Add( LFill('L210') +
             LFill(CODIGO) +
             LFill(DESCRICAO) +
             VLFill(VALOR,2) );
      end;
      FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
    end;
    RegistroL210Count := RegistroL210Count + RegL030.RegistroL210.Count;
  end;
end;

procedure TBloco_L.WriteRegistroL300(RegL030: TRegistroL030);
var
  intFor: integer;
begin
  if Assigned(RegL030.RegistroL300) then
  begin
    for intFor := 0 to RegL030.RegistroL300.Count - 1 do
    begin
      with RegL030.RegistroL300.Items[intFor] do
      begin
        Add( LFill('L300')      +
             LFill(CODIGO)      +
             LFill(DESCRICAO)   +
             LFill(TIPO)        +
             LFill(NIVEL, 3)    +
             LFill(COD_NAT)     +
             LFill(COD_CTA_SUP) +
             VLFill(VALOR,2)    +
             LFill(IND_VALOR) );
      end;
      FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
    end;
    RegistroL300Count := RegistroL300Count + RegL030.RegistroL300.Count;
  end;
end;

procedure TBloco_L.WriteRegistroL001;
begin
  if Assigned(FRegistroL001) then
  begin
    with FRegistroL001 do
    begin
      Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(L-L001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
      Add(LFill('L001') +
          LFill( Integer(IND_DAD), 1));
      if (IND_DAD = idComDados) then
      begin
        WriteRegistroL030(FRegistroL001);
      end;
      FRegistroL990.QTD_LIN:= FRegistroL990.QTD_LIN + 1;
    end;
  end;
end;

procedure TBloco_L.WriteRegistroL030(RegL001: TRegistroL001);
var
  intFor: integer;
begin
  if Assigned(RegL001.RegistroL030) then
  begin
    for intFor := 0 to RegL001.RegistroL030.Count - 1 do
    begin
      with RegL001.RegistroL030.Items[intFor] do
      begin
        Add( LFill('L030') +
             LFill(DT_INI) +
             LFill(DT_FIN) +
             LFill(PER_APUR) );
      end;
      WriteRegistroL100(RegL001.RegistroL030.Items[intFor]);
      WriteRegistroL200(RegL001.RegistroL030.Items[intFor]);
      WriteRegistroL210(RegL001.RegistroL030.Items[intFor]);
      WriteRegistroL300(RegL001.RegistroL030.Items[intFor]);
      FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
     end;
    FRegistroL030Count := FRegistroL030Count + RegL001.RegistroL030.Count;
  end;
end;

procedure TBloco_L.WriteRegistroL990;
begin
  if Assigned(FRegistroL990) then
  begin
    with FRegistroL990 do
    begin
      QTD_LIN := QTD_LIN + 1;
      Add(LFill('L990') +
          LFill(QTD_LIN, 0));
    end;
  end;
end;

end.
