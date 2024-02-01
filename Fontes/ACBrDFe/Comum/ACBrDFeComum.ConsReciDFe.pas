{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

{$I ACBr.inc}

unit ACBrDFeComum.ConsReciDFe;

interface

uses
  SysUtils, Classes,
  pcnConversao;

type
  TConsReciDFe = class
  private
    FtpAmb: TpcnTipoAmbiente;
    FnRec: string;
    FVersao: string;
    FNameSpace: string;
    FtagGrupoMsg: string;
  public
    constructor Create(const AVersao, ANameSpace, AtagGrupoMsg: string);
    destructor Destroy; override;

    function GerarXML: string;
    function ObterNomeArquivo: string;

    property tpAmb: TpcnTipoAmbiente read FtpAmb write FtpAmb;
    property nRec: string read FnRec write FnRec;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TConsReciDFe }

constructor TConsReciDFe.Create(const AVersao, ANameSpace, AtagGrupoMsg: string);
begin
  inherited Create;

  FVersao := AVersao;
  FNameSpace := ANameSpace;
  FtagGrupoMsg := AtagGrupoMsg;
end;

destructor TConsReciDFe.Destroy;
begin

  inherited;
end;

function TConsReciDFe.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(FnRec) + '-ped-rec.xml';
end;

function TConsReciDFe.GerarXML: string;
begin
  Result := '<consReci' + FtagGrupoMsg + ' ' + FNameSpace + ' versao="' + Fversao + '">' +
              '<tpAmb>' + tpAmbToStr(tpAmb) + '</tpAmb>' +
              '<nRec>' + FnRec + '</nRec>' +
            '</consReci' + FtagGrupoMsg + '>';
end;

end.

