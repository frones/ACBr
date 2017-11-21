{******************************************************************************}
{ Projeto: Componente ACBrBlocoX                                               }
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

unit pcnRetEnvBlocoX;

interface

uses
  SysUtils, Classes, pcnConversao, pcnLeitor;

type

  { TRetEnvBlocoX }

  TRetEnvBlocoX = class(TPersistent)
  private
    fLeitor         : TLeitor;
    fVersao         : AnsiString;
    fSituacaoProcCod: Integer;
    fSituacaoProcStr: AnsiString;
    fRecibo         : AnsiString;
    fTipo           : AnsiString;
    fMensagem       : AnsiString;
    fDataRef        : AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor         : TLeitor    read fLeitor;
    property Versao         : AnsiString read fVersao;
    property SituacaoProcCod: Integer    read fSituacaoProcCod;
    property SituacaoProcStr: AnsiString read fSituacaoProcStr;
    property Recibo         : AnsiString read fRecibo;
    property Tipo           : AnsiString read fTipo;
    property Mensagem       : AnsiString read fMensagem;
    property DataRef        : AnsiString read fDataRef;
  end;

implementation

{ TRetEnvBlocoX }

constructor TretEnvBlocoX.Create;
begin
  inherited;
  fLeitor := TLeitor.Create;
end;

destructor TretEnvBlocoX.Destroy;
begin
  fLeitor.Free;
  inherited;
end;

function TretEnvBlocoX.LerXml: Boolean;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;
    if (Leitor.rExtrai(1, 'Resposta') <> '') then
    begin
      fVersao          := Leitor.rAtributo('Versao');
      fSituacaoProcCod := Leitor.rCampo(tcInt, 'SituacaoProcessamentoCodigo');
      fSituacaoProcStr := Leitor.rCampo(tcStr, 'SituacaoProcessamentoDescricao');
      fRecibo          := Leitor.rCampo(tcStr, 'Recibo');
      fTipo            := Leitor.rCampo(tcStr, 'Tipo');
      fMensagem        := Leitor.rCampo(tcStr, 'Mensagem');
      fDataRef         := Leitor.rCampo(tcStr, 'DataReferencia');
      
      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

