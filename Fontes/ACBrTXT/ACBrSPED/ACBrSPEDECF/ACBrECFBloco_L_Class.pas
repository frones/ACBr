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

unit ACBrECFBloco_L_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_L, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_L -

  { TBloco_L }

  TBloco_L = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroL001: TRegistroL001;
    FRegistroL030: TRegistroL030List;
    FRegistroL990: TRegistroL990;

    FRegistroL100Count: Integer;
    FRegistroL200Count: Integer;
    FRegistroL210Count: Integer;
    FRegistroL300Count: Integer;

    function WriteRegistroL100(RegL030: TRegistroL030): String;
    function WriteRegistroL200(RegL030: TRegistroL030): String;
    function WriteRegistroL210(RegL030: TRegistroL030): String;
    function WriteRegistroL300(RegL030: TRegistroL030): String;

    procedure CriaRegistros;overload;
    procedure LiberaRegistros;overload;
  public
    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    constructor Create;
    destructor Destroy;
    procedure LimpaRegistros;

    function WriteRegistroL001: String;
    function WriteRegistroL030: String;
    function WriteRegistroL990: String;

    property RegistroL001: TRegistroL001 read FRegistroL001 write FRegistroL001;
    property RegistroL030: TRegistroL030List read FRegistroL030 write FRegistroL030;
    property RegistroL990: TRegistroL990 read FRegistroL990 write FRegistroL990;

    property RegistroL100Count: Integer read FRegistroL100Count write FRegistroL100Count;
    property RegistroL200Count: Integer read FRegistroL200Count write FRegistroL200Count;
    property RegistroL210Count: Integer read FRegistroL210Count write FRegistroL210Count;
    property RegistroL300Count: Integer read FRegistroL300Count write FRegistroL300Count;
  published
  end;


implementation

uses
  ACBrTXTUtils, StrUtils;

{ TBloco_L }

constructor TBloco_L.Create;
begin
  FRegistroL001 := TRegistroL001.Create;
  FRegistroL030 := TRegistroL030List.Create;
  FRegistroL990 := TRegistroL990.Create;

  FRegistroL100Count := 0;
  FRegistroL200Count := 0;
  FRegistroL210Count := 0;
  FRegistroL300Count := 0;

  FRegistroL990.QTD_LIN := 0;
end;

procedure TBloco_L.CriaRegistros;
begin
  inherited;

  FRegistroL001 := TRegistroL001.Create;
  FRegistroL030 := TRegistroL030List.Create;
  FRegistroL990 := TRegistroL990.Create;

  FRegistroL990.QTD_LIN := 0;
end;

destructor TBloco_L.Destroy;
begin
  FRegistroL001.Free;
  FRegistroL030.Free;
  FRegistroL990.Free;

  inherited;
end;

procedure TBloco_L.LiberaRegistros;
begin
  FRegistroL001.Free;
  FRegistroL030.Free;
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

function TBloco_L.WriteRegistroL100(RegL030: TRegistroL030): String;
var
  intFor: integer;
  strRegistroL100: String;
begin
  strRegistroL100 := '';

  if Assigned(RegL030.RegistroL100) then
  begin
     for intFor := 0 to RegL030.RegistroL100.Count - 1 do
     begin
        with RegL030.RegistroL100.Items[intFor] do
        begin
           ///
           strRegistroL100 :=  strRegistroL100 + LFill('L100')              +
                                                 LFill(CODIGO)              +
                                                 LFill(DESCRICAO)           +
                                                 LFill(TIPO)                +
                                                 LFill(NIVEL)               +
                                                 LFill(COD_NAT,2)           +
                                                 LFill(COD_CTA_SUP)         +
                                                 LFill(VAL_CTA_REF_INI,2)   +
                                                 LFill(IND_VAL_CTA_REF_INI) +
                                                 LFill(VAL_CTA_REF_FIN,2)   +
                                                 LFill(IND_VAL_CTA_REF_FIN) +
                                                 Delimitador                +
                                                 #13#10;
        end;
       FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
     end;
     RegistroL100Count := RegistroL100Count + RegL030.RegistroL100.Count;
  end;
  Result := strRegistroL100;
end;

function TBloco_L.WriteRegistroL200(RegL030: TRegistroL030): String;
var
  intFor: integer;
  strRegistroL200: String;
begin
  strRegistroL200 := '';

  if Assigned(RegL030.RegistroL200) then
  begin
     for intFor := 0 to RegL030.RegistroL200.Count - 1 do
     begin
        with RegL030.RegistroL200.Items[intFor] do
        begin
           ///
           strRegistroL200 :=  strRegistroL200 + LFill('L200') +
                                                 LFill(IND_AVAL_ESTOQ) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
     end;
     RegistroL200Count := RegistroL200Count + RegL030.RegistroL200.Count;
  end;
  Result := strRegistroL200;
end;

function TBloco_L.WriteRegistroL210(RegL030: TRegistroL030): String;
var
  intFor: integer;
  strRegistroL210: String;
begin
  strRegistroL210 := '';

  if Assigned(RegL030.RegistroL210) then
  begin
     for intFor := 0 to RegL030.RegistroL210.Count - 1 do
     begin
        with RegL030.RegistroL210.Items[intFor] do
        begin
           ///
           strRegistroL210 :=  strRegistroL210 + LFill('L210') +
                                                 LFill(CODIGO) +
                                                 LFill(DESCRICAO) +
                                                 LFill(VALOR,2) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
     end;
     RegistroL210Count := RegistroL210Count + RegL030.RegistroL210.Count;
  end;
  Result := strRegistroL210;
end;

function TBloco_L.WriteRegistroL300(RegL030: TRegistroL030): String;
var
  intFor: integer;
  strRegistroL300: String;
begin
  strRegistroL300 := '';

  if Assigned(RegL030.RegistroL300) then
  begin
     for intFor := 0 to RegL030.RegistroL300.Count - 1 do
     begin
        with RegL030.RegistroL300.Items[intFor] do
        begin
           ///
           strRegistroL300 :=  strRegistroL300 + LFill('L300')      +
                                                 LFill(CODIGO)      +
                                                 LFill(DESCRICAO)   +
                                                 LFill(TIPO)        +
                                                 LFill(NIVEL)       +
                                                 LFill(COD_NAT)     +
                                                 LFill(COD_CTA_SUP) +
                                                 LFill(VALOR,2)     +
                                                 LFill(IND_VALOR)   +
                                                 Delimitador        +
                                                 #13#10;
        end;
       FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
     end;
     RegistroL300Count := RegistroL300Count + RegL030.RegistroL300.Count;
  end;
  Result := strRegistroL300;
end;

function TBloco_L.WriteRegistroL001: String;
begin
  Result := '';

  if Assigned(FRegistroL001) then
  begin
     with FRegistroL001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(L-L001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('L001') +
                 LFill( Integer(IND_DAD), 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroL990.QTD_LIN:= FRegistroL990.QTD_LIN + 1;
     end;
  end;
end;

function TBloco_L.WriteRegistroL030: String;
var
  intFor: integer;
  strRegistroL030: String;
begin
  strRegistroL030 := '';

  if Assigned(FRegistroL030) then
  begin
     for intFor := 0 to FRegistroL030.Count - 1 do
     begin
        with FRegistroL030.Items[intFor] do
        begin
           ///
           strRegistroL030 :=  strRegistroL030 + LFill('L030')    +
                                                 LFill(DT_INI)    +
                                                 LFill(DT_FIN)    +
                                                 LFill(PER_APUR)  +
                                                 Delimitador      +
                                                 #13#10;
        end;
        // Registros Filhos
        strRegistroL030 := strRegistroL030 +
                           WriteRegistroL100(FRegistroL030.Items[intFor])+
                           WriteRegistroL200(FRegistroL030.Items[intFor])+
                           WriteRegistroL210(FRegistroL030.Items[intFor])+
                           WriteRegistroL300(FRegistroL030.Items[intFor]);

       FRegistroL990.QTD_LIN := FRegistroL990.QTD_LIN + 1;
     end;
  end;
  Result := strRegistroL030;
end;

function TBloco_L.WriteRegistroL990: String;
begin
  Result := '';

  if Assigned(FRegistroL990) then
  begin
     with FRegistroL990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Result := LFill('L990') +
                 LFill(QTD_LIN, 0) +
                 Delimitador +
                 #13#10;
     end;
  end;
end;

end.
