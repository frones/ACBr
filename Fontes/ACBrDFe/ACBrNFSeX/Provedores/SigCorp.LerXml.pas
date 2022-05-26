{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit SigCorp.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlDocument,
  ACBrNFSeXLerXml_ABRASFv2;

type
  { TNFSeR_SigCorp203 }

  TNFSeR_SigCorp203 = class(TNFSeR_ABRASFv2)
  protected
    function LerDataHoraCancelamento(const ANode: TACBrXmlNode): TDateTime; override;
    function LerDataEmissao(const ANode: TACBrXmlNode): TDateTime; override;
    function LerDataEmissaoRps(const ANode: TACBrXmlNode): TDateTime; override;

    function NormatizarXml(const aXml: string): string; override;
  public

  end;

implementation

uses
  ACBrXmlBase,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SigCorp
//==============================================================================

{ TNFSeR_SigCorp203 }

function TNFSeR_SigCorp203.LerDataEmissao(const ANode: TACBrXmlNode): TDateTime;
var
  xDataHora: string;
begin
  xDataHora := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcStr);

  if Pos('/', xDataHora) = 2 then
    xDataHora := '0' + xDataHora;

  if Pos('/', xDataHora) = 3 then
  begin
    if Copy(xDataHora, 1, 2) > '12' then
      result := EncodeDataHora(xDataHora, 'DD/MM/YYYY')
    else
      result := EncodeDataHora(xDataHora, 'MM/DD/YYYY');
  end
  else
    result := EncodeDataHora(xDataHora, '');
end;

function TNFSeR_SigCorp203.LerDataEmissaoRps(
  const ANode: TACBrXmlNode): TDateTime;
var
  xDataHora: string;
begin
  xDataHora := ObterConteudo(ANode.Childrens.FindAnyNs('DataEmissao'), tcStr);

  if Pos('/', xDataHora) = 2 then
    xDataHora := '0' + xDataHora;

  if Pos('/', xDataHora) = 3 then
  begin
    if Copy(xDataHora, 1, 2) > '12' then
      result := EncodeDataHora(xDataHora, 'DD/MM/YYYY')
    else
      result := EncodeDataHora(xDataHora, 'MM/DD/YYYY');
  end
  else
    result := EncodeDataHora(xDataHora, '');
end;

function TNFSeR_SigCorp203.LerDataHoraCancelamento(
  const ANode: TACBrXmlNode): TDateTime;
var
  xDataHora: string;
begin
  xDataHora := ObterConteudo(ANode.Childrens.FindAnyNs('DataHoraCancelamento'), tcStr);

  if Pos('/', xDataHora) = 2 then
    xDataHora := '0' + xDataHora;

  if Pos('/', xDataHora) = 3 then
  begin
    if Copy(xDataHora, 1, 2) > '12' then
      result := EncodeDataHora(xDataHora, 'DD/MM/YYYY')
    else
      result := EncodeDataHora(xDataHora, 'MM/DD/YYYY');
  end
  else
    result := EncodeDataHora(xDataHora, '');
end;

function TNFSeR_SigCorp203.NormatizarXml(const aXml: string): string;
begin
  Result := inherited NormatizarXml(aXml);

  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

end.
