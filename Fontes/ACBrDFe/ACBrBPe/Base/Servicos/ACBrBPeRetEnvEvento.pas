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

unit ACBrBPeRetEnvEvento;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, ACBrXmlBase,
  pcnSignature,
  ACBrBPeEventoClass;

type
  TRetEventoBPe = class(TObject)
  private
    Fversao: string;
    FretInfEvento: TRetInfEvento;
    Fsignature: Tsignature;

    FXML: string;
    FXmlRetorno: string;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property versao: string read Fversao write Fversao;
    property retInfEvento: TRetInfEvento read FretInfEvento write FretInfEvento;
    property signature: Tsignature read Fsignature write Fsignature;

    property XML: string read FXML write FXML;
    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrBPeConversao,
  ACBrUtil.Strings,
  ACBrXmlDocument,
  ACBrXmlReader;

{ TRetEventoBPe }

constructor TRetEventoBPe.Create;
begin
  inherited Create;

  Fsignature := Tsignature.Create;
end;

destructor TRetEventoBPe.Destroy;
begin
  Fsignature.Free;

  inherited;
end;

function TRetEventoBPe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, ANodeAux, SignatureNode{, ReferenceNode, X509DataNode}: TACBrXmlNode;
  ok: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Document.LoadFromXml(XmlRetorno);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        versao := ObterConteudoTag(ANode.Attributes.Items['versao']);

        ANodeAux := ANode.Childrens.FindAnyNs('infEvento');

        if ANodeAux <> nil then
        begin
          retInfEvento.Id := ObterConteudoTag(ANodeAux.Attributes.Items['Id']);
          retInfEvento.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
          retInfEvento.verAplic := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
          retInfEvento.cOrgao := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cOrgao'), tcInt);
          retInfEvento.cStat := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
          retInfEvento.xMotivo := ACBrStr(ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr));
          retInfEvento.chBPe := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('chBPe'), tcStr);
          retInfEvento.tpEvento := StrToTpEventoBPe(ok, ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('tpEvento'), tcStr));
          retInfEvento.xEvento := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('xEvento'), tcStr);
          retInfEvento.nSeqEvento := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nSeqEvento'), tcInt);
          retInfEvento.dhRegEvento := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhRegEvento'), tcDatHor);
          retInfEvento.nProt := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
        end;

        SignatureNode := ANode.Childrens.FindAnyNs('Signature');

        LerSignature(SignatureNode, signature);
        {
        if SignatureNode <> nil then
        begin
          ReferenceNode := SignatureNode.Childrens.FindAnyNs('SignedInfo')
                                        .Childrens.FindAnyNs('Reference');
          X509DataNode :=  SignatureNode.Childrens.FindAnyNs('KeyInfo')
                                        .Childrens.FindAnyNs('X509Data');

          signature.URI := ObterConteudoTag(ReferenceNode.Attributes.Items['URI']);
          signature.DigestValue := ObterConteudoTag(ReferenceNode.Childrens.FindAnyNs('DigestValue'), tcStr);
          signature.SignatureValue := ObterConteudoTag(SignatureNode.Childrens.FindAnyNs('SignatureValue'), tcStr);
          signature.X509Certificate := ObterConteudoTag(X509DataNode.Childrens.FindAnyNs('X509Certificate'), tcStr);
        end;
        }
      end;

      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

end.
