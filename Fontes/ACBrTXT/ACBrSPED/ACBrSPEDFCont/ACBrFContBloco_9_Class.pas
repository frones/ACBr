{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
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
|* 10/04/2009: Isaque Pinheiro
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}

unit ACBrFContBloco_9_Class;

interface

uses SysUtils, Classes, DateUtils, ACBrSped, ACBrFContBloco_9;

type
  /// TBloco_9 -
  TBloco_9 = class(TACBrSPED)
  private
    FRegistro9001: TRegistro9001;      /// BLOCO 9 - Registro9001
    FRegistro9900: TRegistro9900List;  /// BLOCO 9 - Lista de Registro9900
    FRegistro9990: TRegistro9990;      /// BLOCO 9 - FRegistro9990
    FRegistro9999: TRegistro9999;      /// BLOCO 9 - Registro9999
  public
    constructor Create; /// Create
    destructor Destroy; override; /// Destroy
    procedure LimpaRegistros; override;

    function WriteRegistro9001: String;
    function WriteRegistro9900: String;
    function WriteRegistro9990: String;
    function WriteRegistro9999: String;

    property Registro9001: TRegistro9001     read FRegistro9001 write FRegistro9001;
    property Registro9900: TRegistro9900List read FRegistro9900 write FRegistro9900;
    property Registro9990: TRegistro9990     read FRegistro9990 write FRegistro9990;
    property Registro9999: TRegistro9999     read FRegistro9999 write FRegistro9999;
  end;

implementation

constructor TBloco_9.Create;
begin
  inherited Create;
  FRegistro9001 := TRegistro9001.Create;
  FRegistro9900 := TRegistro9900List.Create;
  FRegistro9990 := TRegistro9990.Create;
  FRegistro9999 := TRegistro9999.Create;

  FRegistro9990.QTD_LIN_9 := 0;
end;

destructor TBloco_9.Destroy;
begin
  FRegistro9001.Free;
  FRegistro9900.Free;
  FRegistro9990.Free;
  FRegistro9999.Free;
  inherited;
end;

procedure TBloco_9.LimpaRegistros;
begin
  FRegistro9900.Clear;

  FRegistro9990.QTD_LIN_9 := 0;
end;

function TBloco_9.WriteRegistro9001: String;
begin
  Result := '';

  if Assigned(FRegistro9001) then
  begin
     with FRegistro9001 do
     begin
       Check(((IND_DAD = 0) or (IND_DAD = 1)), '(9-9001) Na abertura do bloco, deve ser informado o número 0 ou 1!');
       ///
       Result := LFill('9001') +
                 LFill(IND_DAD, 1) +
                 Delimitador +
                 #13#10;
       ///
       FRegistro9990.QTD_LIN_9 := FRegistro9990.QTD_LIN_9 + 1;
     end;
  end;
end;

function TBloco_9.WriteRegistro9900: String;
var
intFor: integer;
strRegistro9900: String;
begin
  strRegistro9900 := '';

  if Assigned(FRegistro9900) then
  begin
     for intFor := 0 to FRegistro9900.Count - 1 do
     begin
        with FRegistro9900.Items[intFor] do
        begin
           strRegistro9900 := strRegistro9900 + LFill('9900') +
                                                LFill(REG_BLC) +
                                                LFill(QTD_REG_BLC, 0) +
                                                Delimitador +
                                                #13#10;
        end;
     end;
     FRegistro9990.QTD_LIN_9 := FRegistro9990.QTD_LIN_9 + FRegistro9900.Count + 2;
  end;
  Result := strRegistro9900;
end;

function TBloco_9.WriteRegistro9990: String;
begin
  Result := '';

  if Assigned(FRegistro9990) then
  begin
     with FRegistro9990 do
     begin
        Result := LFill('9990') +
                  LFill(QTD_LIN_9, 0) +
                  Delimitador +
                  #13#10;
     end;
  end;
end;

function TBloco_9.WriteRegistro9999: String;
begin
  if Assigned(Registro9999) then
  begin
     with FRegistro9999 do
     begin
        Result := LFill('9999') +
                  LFill(QTD_LIN, 0) +
                  Delimitador +
                  #13#10;
     end;
  end;
end;

end.
