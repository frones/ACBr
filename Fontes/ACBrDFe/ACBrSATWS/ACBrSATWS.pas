{******************************************************************************}
{ Projeto: Componente ACBrSATWS                                               }
{ Biblioteca multiplataforma de componentes Delphi para Geração de arquivos    }
{ do Bloco X                                                                   }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{******************************************************************************}

{$I ACBr.inc}

unit ACBrSATWS;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeConfiguracoes, ACBrSATWS_WebServices,
  ACBrUtil;

type

  { TConfiguracoesSATWS }

  TConfiguracoesSATWS = class(TConfiguracoes)
  public
    procedure Assign(DeConfiguracoesSATWS: TConfiguracoesSATWS); reintroduce;
  published
    property Geral;
    property WebServices;
  end;

  { TACBrSATWS }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  {$ENDIF RTL230_UP}	
  TACBrSATWS = class(TACBrDFe)
  private
    FWebServices: TACBrSATWS_WebServices;
    function GetConfiguracoes: TConfiguracoesSATWS;
    procedure SetConfiguracoes(const Value: TConfiguracoesSATWS);
  protected
    function CreateConfiguracoes: TConfiguracoes; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property WebServices: TACBrSATWS_WebServices read FWebServices write FWebServices;
  published
    property Configuracoes: TConfiguracoesSATWS read GetConfiguracoes Write SetConfiguracoes;
  end;

implementation

{ TConfiguracoesSATWS }

procedure TConfiguracoesSATWS.Assign(
  DeConfiguracoesSATWS: TConfiguracoesSATWS);
begin
  WebServices.Assign(DeConfiguracoesSATWS.WebServices);
end;

{ TACBrSATWS }

constructor TACBrSATWS.Create(AOwner: TComponent);
begin
  inherited;

  FWebServices  := TACBrSATWS_WebServices.Create(Self);
end;

destructor TACBrSATWS.Destroy;
begin
  FWebServices.Free;

  inherited;
end;

function TACBrSATWS.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoesSATWS.Create(Self);
end;

function TACBrSATWS.GetConfiguracoes: TConfiguracoesSATWS;
begin
  Result := TConfiguracoesSATWS(FPConfiguracoes);
end;

procedure TACBrSATWS.SetConfiguracoes(const Value: TConfiguracoesSATWS);
begin
  FPConfiguracoes := Value;
end;

end.
