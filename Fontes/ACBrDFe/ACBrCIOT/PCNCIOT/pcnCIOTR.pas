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

unit pcnCIOTR;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  ACBrUtil.Strings,
  pcnLeitor, ACBrCIOTConversao, pcnCIOT;

type

  TCIOTR = class(TPersistent)
  private
    FLeitor: TLeitor;
    FCIOT: TCIOT;
  public
    constructor Create(AOwner: TCIOT);
    destructor Destroy; override;
    function LerXml: Boolean;
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property CIOT: TCIOT       read FCIOT    write FCIOT;
  end;

implementation

{ TCIOTR }

constructor TCIOTR.Create(AOwner: TCIOT);
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FCIOT := AOwner;
end;

destructor TCIOTR.Destroy;
begin
  FLeitor.Free;

  inherited Destroy;
end;

function TCIOTR.LerXml: Boolean;
begin
  Leitor.Grupo := Leitor.Arquivo;

//  CIOT.usuario := Leitor.rCampo(tcStr, 'usuario');
//  CIOT.senha   := Leitor.rCampo(tcStr, 'senha');
//  CIOT.codatm  := Leitor.rCampo(tcStr, 'codatm');
//
//  CIOT.xmlDFe := Leitor.rCampo(tcStr, 'xmlCTe');
//
//  if CIOT.xmlDFe = '' then
//    CIOT.xmlDFe := Leitor.rCampo(tcStr, 'xmlNFe');
//
//  if CIOT.xmlDFe = '' then
//    CIOT.xmlDFe := Leitor.rCampo(tcStr, 'xmlMDFe');
//
//  CIOT.xmlDFe := StringReplace(CIOT.xmlDFe, '<![CDATA[', '', [rfReplaceAll]);
//  CIOT.xmlDFe := StringReplace(CIOT.xmlDFe, ']]>', '', [rfReplaceAll]);
//
//  CIOT.aplicacao     := Leitor.rCampo(tcStr, 'aplicacao');
//  CIOT.assunto       := Leitor.rCampo(tcStr, 'assunto');
//  CIOT.remetentes    := Leitor.rCampo(tcStr, 'remetentes');
//  CIOT.destinatarios := Leitor.rCampo(tcStr, 'destinatarios');
//  CIOT.corpo         := Leitor.rCampo(tcStr, 'corpo');
//  CIOT.chave         := Leitor.rCampo(tcStr, 'chave');
//  CIOT.chaveresp     := Leitor.rCampo(tcStr, 'chaveresp');

  Result := True;
end;

end.

