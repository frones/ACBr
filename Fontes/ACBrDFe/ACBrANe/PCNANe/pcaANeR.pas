{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcaANeR;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  ACBrUtil.Base,
  pcnConversao, pcnLeitor, pcaANe;

type

  TANeR = class(TObject)
  private
    FLeitor: TLeitor;
    FANe: TANe;
  public
    constructor Create(AOwner: TANe);
    destructor Destroy; override;
    function LerXml: Boolean;
    property Leitor: TLeitor read FLeitor write FLeitor;
    property ANe: TANe       read FANe    write FANe;
  end;

implementation

{ TANeR }

constructor TANeR.Create(AOwner: TANe);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FANe := AOwner;
end;

destructor TANeR.Destroy;
begin
  FLeitor.Free;
  
  inherited Destroy;
end;

function TANeR.LerXml: Boolean;
begin
  Leitor.Grupo := Leitor.Arquivo;

  ANe.usuario := Leitor.rCampo(tcStr, 'usuario');
  ANe.senha   := Leitor.rCampo(tcStr, 'senha');
  ANe.codatm  := Leitor.rCampo(tcStr, 'codatm');

  ANe.xmlDFe := Leitor.rCampo(tcStr, 'xmlCTe');

  if ANe.xmlDFe = '' then
    ANe.xmlDFe := Leitor.rCampo(tcStr, 'xmlNFe');

  if ANe.xmlDFe = '' then
    ANe.xmlDFe := Leitor.rCampo(tcStr, 'xmlMDFe');

  ANe.xmlDFe := StringReplace(ANe.xmlDFe, '<![CDATA[', '', [rfReplaceAll]);
  ANe.xmlDFe := StringReplace(ANe.xmlDFe, ']]>', '', [rfReplaceAll]);

  ANe.aplicacao     := Leitor.rCampo(tcStr, 'aplicacao');
  ANe.assunto       := Leitor.rCampo(tcStr, 'assunto');
  ANe.remetentes    := Leitor.rCampo(tcStr, 'remetentes');
  ANe.destinatarios := Leitor.rCampo(tcStr, 'destinatarios');
  ANe.corpo         := Leitor.rCampo(tcStr, 'corpo');
  ANe.chave         := Leitor.rCampo(tcStr, 'chave');
  ANe.chaveresp     := Leitor.rCampo(tcStr, 'chaveresp');

  Result := True;
end;

end.

