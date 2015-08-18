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

unit ACBrECFBloco_J_Class;

interface

uses
  SysUtils, Classes, DateUtils, ACBrSped, ACBrECFBloco_J, ACBrECFBlocos,
  ACBrTXTClass, ACBrECFBloco_0_Class;

type
  /// TBloco_J -

  { TBloco_J }

  TBLOCO_J = class(TACBrSPED)
  private
    FBloco_0: TBloco_0;
    FRegistroJ001: TRegistroJ001;      /// BLOCO I - RegistroJ001
    //FRegistroJ015: TRegistroJ015List;  /// BLOCO I - Lista de RegistroJ015
    FRegistroJ050: TRegistroJ050List;  /// BLOCO I - Lista de RegistroJ050
    FRegistroJ100: TRegistroJ100List;  /// BLOCO I - Lista de RegistroJ100
    FRegistroJ990: TRegistroJ990;      /// BLOCO I - FRegistroJ990

    FRegistroJ051Count: Integer;
    FRegistroJ053Count: Integer;

    function WriteRegistroJ051(RegJ050: TRegistroJ050): String;
    function WriteRegistroJ053(RegJ050: TRegistroJ050): String;
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros;

    property Bloco_0: TBloco_0 read FBloco_0 write FBloco_0;

    function WriteRegistroJ001: String;
    function WriteRegistroJ050: String;
    function WriteRegistroJ100: String;
    function WriteRegistroJ990: String;

    property RegistroJ001: TRegistroJ001     read FRegistroJ001 write FRegistroJ001;
    //property RegistroJ015: TRegistroJ015List read fRegistroJ015 write fRegistroJ015;
    property RegistroJ050: TRegistroJ050List read fRegistroJ050 write fRegistroJ050;
    property RegistroJ100: TRegistroJ100List read fRegistroJ100 write fRegistroJ100;
    property RegistroJ990: TRegistroJ990     read FRegistroJ990 write FRegistroJ990;

    property RegistroJ051Count: Integer read FRegistroJ051Count write FRegistroJ051Count;
    property RegistroJ053Count: Integer read FRegistroJ053Count write FRegistroJ053Count;
  end;

implementation

{ TBloco_J }

constructor TBloco_J.Create;
begin
  FRegistroJ001 := TRegistroJ001.Create;
  FRegistroJ050 := TRegistroJ050List.Create;
  FRegistroJ100 := TRegistroJ100List.Create;
  FRegistroJ990 := TRegistroJ990.Create;

  FRegistroJ051Count := 0;
  FRegistroJ053Count := 0;

  FRegistroJ990.QTD_LIN := 0;
end;

destructor TBloco_J.Destroy;
begin
  FRegistroJ001.Free;
  FRegistroJ050.Free;
  FRegistroJ100.Free;

  FRegistroJ990.Free;
  inherited;
end;

procedure TBloco_J.LimpaRegistros;
begin
  FRegistroJ050.Clear;
  FRegistroJ100.Clear;

  FRegistroJ051Count := 0;
  FRegistroJ053Count := 0;

  FRegistroJ990.QTD_LIN:= 0;
end;

function TBloco_J.WriteRegistroJ001: String;
begin
  Result := '';

  if Assigned(FRegistroJ001) then
  begin
     with FRegistroJ001 do
     begin
       Check(((IND_DAD = idComDados) or (IND_DAD = idSemDados)), '(J-J001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('J001') +
                 LFill( Integer(IND_DAD), 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistroJ990.QTD_LIN:= FRegistroJ990.QTD_LIN + 1;
     end;
  end;
end;


function TBloco_J.WriteRegistroJ050: String;
var
intFor: integer;
strRegistroJ050: String;
begin
  strRegistroJ050 := '';

  if Assigned(FRegistroJ050) then
  begin
     for intFor := 0 to FRegistroJ050.Count - 1 do
     begin
        with FRegistroJ050.Items[intFor] do
        begin
           ///
           strRegistroJ050 :=  strRegistroJ050 + LFill('J050') +
                                                 LFill(DT_ALT) +
                                                 LFill(COD_NAT, 2) +
                                                 LFill(IND_CTA, 1) +
                                                 LFill(NIVEL) +
                                                 LFill(COD_CTA) +
                                                 LFill(COD_CTA_SUP) +
                                                 LFill(CTA) +
                                                 Delimitador +
                                                 #13#10;
        end;
        // Registros Filhos
        strRegistroJ050 := strRegistroJ050 +
                           WriteRegistroJ051(FRegistroJ050.Items[intFor] ) +
                           WriteRegistroJ053(FRegistroJ050.Items[intFor] );

       FRegistroJ990.QTD_LIN := FRegistroJ990.QTD_LIN + 1;
     end;
  end;
  Result := strRegistroJ050;
end;

function TBloco_J.WriteRegistroJ051(RegJ050: TRegistroJ050): String;
var
intFor: integer;
strRegistroJ051: String;
begin
  strRegistroJ051 := '';

  if Assigned(RegJ050.RegistroJ051) then
  begin
     for intFor := 0 to RegJ050.RegistroJ051.Count - 1 do
     begin
        with RegJ050.RegistroJ051.Items[intFor] do
        begin
           ///
           strRegistroJ051 :=  strRegistroJ051 + LFill('J051') +
//                                                 LFill(COD_ENT_REF, 2) +
                                                 LFill(COD_CCUS) +
                                                 LFill(COD_CTA_REF) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroJ990.QTD_LIN := FRegistroJ990.QTD_LIN + 1;
     end;
     FRegistroJ051Count := FRegistroJ051Count + RegJ050.RegistroJ051.Count;
  end;
  Result := strRegistroJ051;
end;

function TBloco_J.WriteRegistroJ053(RegJ050: TRegistroJ050): String;
var
intFor: integer;
strRegistroJ053: String;
begin
  strRegistroJ053 := '';

  if Assigned(RegJ050.RegistroJ053) then
  begin
     for intFor := 0 to RegJ050.RegistroJ053.Count - 1 do
     begin
        with RegJ050.RegistroJ053.Items[intFor] do
        begin
           ///
           strRegistroJ053 :=  strRegistroJ053 + LFill('J053') +
                                                 LFill(COD_IDT) +
                                                 LFill(COD_CNT_CORR) +
                                                 LFill(NAT_SUB_CNT) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroJ990.QTD_LIN := FRegistroJ990.QTD_LIN + 1;
     end;
     FRegistroJ053Count := FRegistroJ053Count + RegJ050.RegistroJ053.Count;
  end;
  Result := strRegistroJ053;
end;
function TBloco_J.WriteRegistroJ100: String;
var
intFor: integer;
strRegistroJ100: String;
begin
  strRegistroJ100 := '';

  if Assigned(RegistroJ100) then
  begin
     for intFor := 0 to RegistroJ100.Count - 1 do
     begin
        with RegistroJ100.Items[intFor] do
        begin
           ///
           strRegistroJ100 :=  strRegistroJ100 + LFill('J100') +
                                                 LFill(DT_ALT) +
                                                 LFill(COD_CCUS) +
                                                 LFill(CCUS) +
                                                 Delimitador +
                                                 #13#10;
        end;
       FRegistroJ990.QTD_LIN := FRegistroJ990.QTD_LIN + 1;
     end;
  end;
  Result := strRegistroJ100;
end;


function TBloco_J.WriteRegistroJ990: String;
begin
  Result := '';

  if Assigned(FRegistroJ990) then
  begin
     with FRegistroJ990 do
     begin
       QTD_LIN := QTD_LIN + 1;
       ///
       Result := LFill('J990') +
                 LFill(QTD_LIN, 0) +
                 Delimitador +
                 #13#10;
     end;
  end;
end;

end.
