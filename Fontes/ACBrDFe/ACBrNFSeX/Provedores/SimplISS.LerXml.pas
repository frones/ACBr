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

unit SimplISS.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXLerXml_ABRASFv1, ACBrNFSeXLerXml_ABRASFv2, ACBrXmlDocument, ACBrXmlBase;

type
  { TNFSeR_SimplISS }

  TNFSeR_SimplISS = class(TNFSeR_ABRASFv1)
  protected

  public

  end;

  { TNFSeR_SimplISS203 }

  TNFSeR_SimplISS203 = class(TNFSeR_ABRASFv2)
  protected
    procedure LerServico(const ANode: TACBrXmlNode); override;
  public

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     SimplISS
//==============================================================================


{ TNFSeR_SimplISS203 }

procedure TNFSeR_SimplISS203.LerServico(const ANode: TACBrXmlNode);
var
   AuxNode: TACBrXmlNode;
begin
  inherited LerServico(ANode);

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Servico');
  if AuxNode <> nil then
  begin
    if NFSe.OutrasInformacoes = '' then
    begin
      NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasInformacoes'), tcStr);
      NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, '&lt;br&gt;', ';', [rfReplaceAll]);
    end;
  end;
end;

end.
