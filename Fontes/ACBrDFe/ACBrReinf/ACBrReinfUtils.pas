{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}

unit ACBrReinfUtils;

interface

uses
  SysUtils, TypInfo, pcnConversaoReinf;

type

  TReinfUtils = class
  public
    class function StrToZero(const AString: string; ATamanho : Integer; AEsquerda: Boolean = true): string;
  end;

function eSSimNaoToStr(AValue: tpSimNao): string;

implementation

function eSSimNaoToStr(AValue: tpSimNao): string;
begin
  if AValue = tpSim then
    Result := 'S'
  else
    Result := 'N';
end;

{ TReinfUtils }

class function TReinfUtils.StrToZero(const AString: string; ATamanho: Integer; AEsquerda: Boolean): string;
var
  Str: string;
begin
  Str := AString;
  while Length(Str) < ATamanho do
  begin
    if AEsquerda then
      Str := '0' + Str
    Else
      Str := Str + '0';
  end;
  Result := Str;
end;

end.

