{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasCobsConsultadas;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase, ACBrPIXSchemasParametrosConsultaCob,
  ACBrPIXSchemasCob;

type

  { TACBrPIXCobsConsultadas }

  TACBrPIXCobsConsultadas = class(TACBrPIXSchema)
  private
    fcobs: TACBrPIXCobCompletaArray;
    fparametros: TACBrPIXParametrosConsultaCob;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    destructor Destroy; override;
    procedure Assign(Source: TACBrPIXCobsConsultadas);

    property parametros: TACBrPIXParametrosConsultaCob read fparametros;
    property cobs: TACBrPIXCobCompletaArray read fcobs;
  end;

implementation

{ TACBrPIXCobsConsultadas }

constructor TACBrPIXCobsConsultadas.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcobs := TACBrPIXCobCompletaArray.Create('cobs');
  fparametros := TACBrPIXParametrosConsultaCob.Create('parametros');
end;

destructor TACBrPIXCobsConsultadas.Destroy;
begin
  fcobs.Free;
  fparametros.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobsConsultadas.Clear;
begin
  fparametros.Clear;
  fcobs.Clear;
end;

procedure TACBrPIXCobsConsultadas.Assign(Source: TACBrPIXCobsConsultadas);
begin
  fparametros.Assign(Source.parametros);
  fcobs.Assign(Source.cobs);
end;

procedure TACBrPIXCobsConsultadas.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  fparametros.WriteToJSon(AJSon);
  fcobs.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobsConsultadas.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  fparametros.ReadFromJSon(AJSon);
  fcobs.ReadFromJSon(AJSon);
end;

function TACBrPIXCobsConsultadas.IsEmpty: Boolean;
begin
  Result := fparametros.IsEmpty and fcobs.IsEmpty;
end;

end.

