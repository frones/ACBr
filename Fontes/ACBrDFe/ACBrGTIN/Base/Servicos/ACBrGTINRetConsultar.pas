{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrGTINRetConsultar;

interface

uses
  SysUtils, Classes, DateUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase;

type

  TRetConsultarGTIN = class(TObject)
  private
    Fversao: String;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    FGTIN: String;
    FtpGTIN: Integer;
    FxProd: String;
    FNCM: String;
    FCEST: String;

    FXmlRetorno: String;
  public
    function LerXml: Boolean;

    property versao: String    read Fversao   write Fversao;
    property verAplic: String  read FverAplic write FverAplic;
    property cStat: Integer    read FcStat    write FcStat;
    property xMotivo: String   read FxMotivo  write FxMotivo;
    property dhResp: TDateTime read FdhResp   write FdhResp;
    property GTIN: String      read FGTIN     write FGTIN;
    property tpGTIN: Integer   read FtpGTIN   write FtpGTIN;
    property xProd: String     read FxProd    write FxProd;
    property NCM: String       read FNCM      write FNCM;
    property CEST: String      read FCEST     write FCEST;

    property XmlRetorno: String read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrXmlDocument, ACBrXmlBase;

{ TRetConsultarGTIN }

function TRetConsultarGTIN.LerXml: boolean;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;
  try
    try
      Document.LoadFromXml(XmlRetorno);
      ANode := Document.Root.Childrens.FindAnyNs('retConsGTIN');

      if ANode <> nil then
      begin
        versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
        verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
        cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
        xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
        dhResp := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhResp'), tcDatHor);
        GTIN := ObterConteudoTag(ANode.Childrens.FindAnyNs('GTIN'), tcStr);
        tpGTIN := ObterConteudoTag(ANode.Childrens.FindAnyNs('tpGTIN'), tcInt);
        xProd := ObterConteudoTag(ANode.Childrens.FindAnyNs('xProd'), tcStr);
        NCM := ObterConteudoTag(ANode.Childrens.FindAnyNs('NCM'), tcStr);
        CEST := ObterConteudoTag(ANode.Childrens.FindAnyNs('CEST'), tcStr);
      end;

      Result := True;
    except
      Result := False;
    end;
  finally
    Document.Free;
  end;
end;

end.

