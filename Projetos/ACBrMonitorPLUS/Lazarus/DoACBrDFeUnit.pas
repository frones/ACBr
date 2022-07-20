{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

unit DoACBrDFeUnit;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrMonitorConsts, ACBrDFeUtil, ACBrDFe;

type

{ TACBrCarregarDFe }

TACBrCarregarDFe = class
protected
  FXMLorFile           : String;
  FPathDFe             : String;
  FPathDFeExtensao     : String;
  FRetornaFalha        : Boolean;

  FpACBrDFe: TACBrDFe;
  FMsgValidaPath : TStringList;

  procedure CarregarDFePath( const AValue: String ); virtual;
  procedure CarregarDFeXML( const AValue: String ); virtual;
  function ValidarDFe( const AValue: String ): Boolean; virtual;

  function ValidarPath(const AValue: String): Boolean;
  procedure SetFXMLorFile(const AValue: String);

  property XMLorFile : String read FXMLorFile write SetFXMLorFile;

public
  constructor Create(AACBrDFe: TACBrDFe; AXMLorFile: String; ARetornaFalha: Boolean = True );
  Destructor  Destroy; Override;

  property PathDFe        : String read FPathDFe;
  property PathDFeExtensao: String read FPathDFeExtensao;
  property RetornaFalha   : Boolean read FRetornaFalha;

end;

implementation

uses
  DoACBrUnit;

{ TACBrCarregarDFe }

procedure TACBrCarregarDFe.CarregarDFePath(const AValue: String);
begin
  raise Exception.Create(SErroNaoImplementado);
end;

procedure TACBrCarregarDFe.CarregarDFeXML(const AValue: String);
begin
  raise Exception.Create(SErroNaoImplementado);
end;

function TACBrCarregarDFe.ValidarDFe( const AValue: String ): Boolean;
begin
  Result := False;
  raise Exception.Create(SErroNaoImplementado);
end;

function TACBrCarregarDFe.ValidarPath(const AValue: String): Boolean;
begin
  Result := False;
  if (FilesExists( AValue ) ) then
    Result:= True
  else
    FMsgValidaPath.Add( Format( SErroArqNaoEncontado, [AValue] ) );
end;

procedure TACBrCarregarDFe.SetFXMLorFile(const AValue: String);
var
  IsXML : Boolean;
begin
  try
    IsXML := StringIsXML(AValue);

    if not(IsXML) then
    begin
      try
        if ValidarPath(AValue) then
          CarregarDFePath(AValue);

        if not(ValidarDFe(AValue) ) then
        begin
          if ValidarPath(FPathDFe) then
            CarregarDFePath(FPathDFe);
        end;

        if not(ValidarDFe(AValue) ) then
        begin
          if ValidarPath(FPathDFeExtensao) then
            CarregarDFePath(FPathDFeExtensao)
          else if RetornaFalha then
            raise Exception.Create( ACBrStr( FMsgValidaPath.Text ) );
        end;

      finally
        FMsgValidaPath.Clear;
      end;
    end
    else
      CarregarDFeXML(AValue);

  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

constructor TACBrCarregarDFe.Create(AACBrDFe: TACBrDFe; AXMLorFile: String; ARetornaFalha: Boolean);
begin
  inherited Create;
  FMsgValidaPath   := TStringList.Create;
  FPathDFe         := '';
  FPathDFeExtensao := '';
  FRetornaFalha    := ARetornaFalha;
  FpACBrDFe := AACBrDFe;
  XMLorFile := AXMLorFile;
end;

destructor TACBrCarregarDFe.Destroy;
begin
  inherited;
  FMsgValidaPath.Free;
end;


end.

