{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pmdfeSignature;

interface

uses
  SysUtils, Classes,
  pcnConversao, pcnGerador, pcnConsts;

type

  TSignature = class(TPersistent)
  private
    FGerador: TGerador;
    FURI: String;
    FDigestValue: String;
    FSignatureValue: String;
    FX509Certificate: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
  published
    property Gerador: TGerador       read FGerador         write FGerador;
    property URI: String             read FURI             write FURI;
    property DigestValue: String     read FDigestValue     write FDigestValue;
    property SignatureValue: String  read FSignatureValue  write FSignatureValue;
    property X509Certificate: String read FX509Certificate write FX509Certificate;
  end;

implementation

{ TSignature }

constructor TSignature.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TSignature.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TSignature.GerarXML: Boolean;
begin
  FGerador.ArquivoFormatoXML := '';
  FGerador.Opcoes.TagVaziaNoFormatoResumido := false;
  FGerador.FIgnorarTagIdentacao := '|Reference URI|SignatureMethod|Transform Algorithm="http://www.w3.org/TR|/Transforms|/Reference|';

  Gerador.wGrupo('Signature xmlns="http://www.w3.org/2000/09/xmldsig#"', 'XS01');
  Gerador.wGrupo('SignedInfo', 'XS02');
  Gerador.wGrupo('CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/', 'XS03');
  Gerador.wGrupo('SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1"/', 'XS05');
  Gerador.wGrupo('Reference URI="#MDFe' + FURI + '"', 'XS07');
  Gerador.wGrupo('Transforms', 'XS10');
  Gerador.wGrupo('Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/', 'SX12');
  Gerador.wGrupo('Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315"/', 'SX12');
  Gerador.wGrupo('/Transforms');
  Gerador.wGrupo('DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1"/', 'XS15');
  Gerador.wCampo(tcStr, 'XS17', 'DigestValue', 000, 999, 1, FDigestValue, DSC_DigestValue);
  Gerador.wGrupo('/Reference');
  Gerador.wGrupo('/SignedInfo');
  Gerador.wCampo(tcStr, 'XS18', 'SignatureValue', 000, 999, 1, FSignatureValue, DSC_SignatureValue);
  Gerador.wGrupo('KeyInfo', 'XS19');
  Gerador.wGrupo('X509Data', 'XS20');
  Gerador.wCampo(tcStr, 'XS21', 'X509Certificate', 000, 999, 1, FX509Certificate, DSC_X509Certificate);
  Gerador.wGrupo('/X509Data');
  Gerador.wGrupo('/KeyInfo');
  Gerador.wGrupo('/Signature');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

